#lang racket

; Experimental proof of concept for *file-level* Forge evaluation service
; When a file text is received:
;   (1) read it with a modified reader that produces a lambda syntax-object
;   (2) evaluate that syntax object into a lambda 
;   (3) execute the lambda (which, within the lambda, defines preds etc. and invokes the solver)

; TODOs:
;   - some in file below
;   - support run (returning instances) as well as test results
;   - select to execute a single run via param
;   - support different #lang selections via param
;   - state (?) to provide instance stream
;   - error handling

; If we want this to connect seamlessly with Sterling, we may need to conform to the Sterling API.

(require forge/sigs
         forge/lang/service-expander
         (only-in forge/shared do-time))
(require (prefix-in frg: (only-in forge/lang/service-reader read-syntax)))
(require (prefix-in frg: (only-in forge/server/modelToXML solution-to-XML-string)))

(require web-server/http
         web-server/http/response
         web-server/web-server
         web-server/http/json
         json
         net/url
         web-server/dispatchers/dispatch
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer))
         
(require (prefix-in @ (only-in racket/contract ->)))

; commit to #lang forge in this proof-of-concept
(require forge/choose-lang-specific
         forge/lang/lang-specific-checks)
(set-checker-hash! forge-checker-hash)
(set-ast-checker-hash! forge-ast-checker-hash)
(set-inst-checker-hash! forge-inst-checker-hash)
(set-check-lang! 'forge)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Risk that this keeps old pred names etc.
; Use namespace-undefine-variable! ?
(define forge-expander-ns (module->namespace 'forge/lang/service-expander))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (run-model-file bytes)
  (@-> bytes? forge:State?)
  ; Restore the Forge state (this does NOT clean out the Racket namespace)
  ; Restore the *solver* state as well (delete old runs and allow GC)
  (forge:clear-state-and-solver)
  
  (forge:set-option! 'verbose 0)           ; TODO: note only works if the file doesn't reset this
  (forge:set-option! 'run_sterling 'false) ; TODO: why isn't this taking effect?
  
  ; Step 0: Scrub a #lang prefix, if one is present
  ; TODO: this should be improved to remove until newline or comment
  ; TODO: UTF8 only
  (define str (regexp-replace #rx"#lang forge|froglet|forge/bsl" (bytes->string/utf-8 bytes) ""))
  ; Step 1: read the file string into a lambda syntax object
  (define filestx (frg:read-syntax #f (open-input-string str)))
  ; Step 2: evaluate the syntax object into a procedure
  ; TODO: security concerns from calling eval like this?
  ;    racket/sandbox + make-evaluator
  (define fileproc (eval filestx forge-expander-ns))
  ; Step 3: execute the lambda
  (fileproc))

; Add CORS header(s); note this will not overwrite if already there
(define (cors-update resp)
  (struct-copy response resp
               [headers (cons (header #"Access-Control-Allow-Origin" #"*")
                              (response-headers resp))]))

; A servelet is a function from request -> can-be-response
(define (start-file req)
  (printf "start-file request received~n~n~a~n~n" req)
  ; Query *parameters* (GET and POST)
  (define binds (request-bindings/raw req))
  ; Query *data* (POST)
  ; Aside: GET request bodies can contain data, but they should have no semantic weight
  ; thus, we'll require POST for using this endpoint
  (define pdata (request-post-data/raw req))
  ;(printf "binds: ~a~n" binds)
  ;(printf "pdata: ~a~n" pdata)
  (define state-result (run-model-file pdata))
  ; ^ unused, and can use forge:curr-state regardless
  (define response-json
    (hash
     'endpoint "load"
     'run_ids (map symbol->string
                    (hash-keys (forge:State-runmap forge:curr-state)))))
  (cors-update (response/jsexpr response-json)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO: does the above invoke the solver? it shouldn't

; N.B.: jsexpr? won't allow symbols except as hash keys; need to convert

(define (get-binding binds k)
  (match (bindings-assq k binds)
    [(binding:form _ val) val]
    [else (raise (format "no key in bindings: ~a" k))]))

(define (get-sat req)
  (printf "get-sat request received~n")
  (define binds (request-bindings/raw req))
  (define id-bytes (get-binding binds #"id"))
  (define runmap (forge:State-runmap forge:curr-state))
  (define id (bytes->string/utf-8 id-bytes))
  (cond
    [(not (hash-has-key? runmap (string->symbol id)))
     (error-unknown-id "sat" id)]
    [else
     (define response-json
       (hash
        'endpoint "sat"
        'id id
        'sat (forge:is-sat? (hash-ref runmap (string->symbol id)))))
     (cors-update (response/jsexpr response-json))]))


; Return an instance in Alloy-XML format (for use by Sterling)
(define (get-next req)
  (printf "get-next request received (only first instance supported): ~a~n" req)
  (define binds (request-bindings/raw req))
  (define id-bytes (get-binding binds #"id"))
  (define runmap (forge:State-runmap forge:curr-state))
  (define id (bytes->string/utf-8 id-bytes))
  (cond
    [(not (hash-has-key? runmap (string->symbol id)))
     (error-unknown-id "next" id)]
    [else 
     (define run (hash-ref runmap (string->symbol id)))
     (define generator (forge:Run-result run))
     (define ret (tree:get-value generator))
     (define command-string (format "~a" (syntax->datum (forge:Run-command run))))
     ;(set! model-lazy-tree (tree:get-child model-lazy-tree mode))
     (define response-json
       (hash
        'endpoint "next"
        'id id
        'instance (frg:solution-to-XML-string ret
                                              (forge:get-relation-map run)
                                              (forge:Run-name run) 
                                              command-string
                                              "service"
                                              (forge:get-bitwidth (forge:Run-run-spec run))
                                              forge:forge-version)))
     (cors-update (response/jsexpr response-json))]))

(define (error-unknown-id endpoint id)
  (response/jsexpr  
   (hash
    'type "error"
    'message "unknown id"
    'endpoint endpoint
    'id id)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; `serve/servelet` won't let us combine multiple servelets, so instead
; we sequence two dispatchers and use `serve`
; Thanks to: https://defn.io/2020/02/12/racket-web-server-guide/

(define (request-path-has-prefix? req p)
  (string-prefix? (path->string (url->path (request-uri req))) p))

(define (make-dispatcher proc methods prefix)
  (lambda (conn req)
    (if (and (request-path-has-prefix? req prefix)
             (member (string-downcase (bytes->string/utf-8 (request-method req)))
                     (map string-downcase methods)))
        (output-response conn (proc req))
        (next-dispatcher))))
(define (make-error-dispatcher)
  (lambda (conn req)
    (printf "malformed request, unknown endpoint or unsupported method: ~a~n" req)
    (output-response conn (response/jsexpr (hash 'error "invalid endpoint")))))

(define server-port 17100)

(define stop
  (serve
   #:dispatch (sequencer:make (make-dispatcher start-file '("post") "/load")
                              (make-dispatcher get-sat '("get") "/sat")
                              (make-dispatcher get-next '("get") "/next")
                              (make-error-dispatcher))
   #:listen-ip "127.0.0.1"
   #:port server-port))

(with-handlers ([exn:break? (lambda (e)
                              (stop))])
  (sync/enable-break never-evt))