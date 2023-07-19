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

(require forge/sigs
         forge/lang/expander)
(require (prefix-in frg: (only-in forge/lang/service-reader read-syntax)))

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


(define server-port 17100)

(define/contract (run-model-file bytes)
  (@-> bytes? forge:State?)
  ; Restore the Forge state (this does NOT clean out the Racket namespace)
  ; Restore the *solver* state as well (delete old runs and allow GC)
  (forge:clear-state-and-solver)
  
  (forge:set-option! 'verbose 0)           ; TEMP: note only works if the file doesn't reset this
  (forge:set-option! 'run_sterling 'false) ; TODO: why isn't this taking effect?
  
  ; Step 0: Scrub a #lang prefix, if one is present
  ; TODO: this should be improved to remove until newline or comment
  ; TODO: UTF8 only
  (define str (regexp-replace #rx"#lang forge|froglet|forge/bsl" (bytes->string/utf-8 bytes) ""))
  ;(printf "FILE STRING=~a~n" str)
  ; Step 1: read the file string into a lambda syntax object
  (define filestx (frg:read-syntax #f (open-input-string str)))
  ; Step 2: evaluate the syntax object into a procedure
  (define forge-expander-ns (module->namespace 'forge/lang/expander))
  ; TODO: security concerns from calling eval like this?
  (define fileproc (eval filestx forge-expander-ns))
  ; Step 3: execute the lambda
  (fileproc))

; A servelet is a function from request -> can-be-response
(define (start-file req)
  (printf "start-file request received~n")
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
     'type "load"
     'run-ids (map symbol->string
                    (hash-keys (forge:State-runmap forge:curr-state)))))
  (response/jsexpr response-json))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO: does the above invoke the solver? it shouldn't

; N.B.: jsexpr? won't allow symbols except as hash keys; need to convert

(define (get-binding binds k)
  (match (bindings-assq k binds)
    [(binding:form _ val) val]
    [else (raise (format "no key in bindings: ~a" k))]))

; REQUIRE: model file has been loaded with the run ID being requested
(define (get-sat req)
  (printf "get-sat request received~n")
  (define binds (request-bindings/raw req))
  (define id-bytes (get-binding binds #"id"))
  (define runmap (forge:State-runmap forge:curr-state))
  (define id (bytes->string/utf-8 id-bytes))
  (define response-json
    (hash
     'type "is-sat?"
     'id id
     'sat (forge:is-sat? (hash-ref runmap (string->symbol id)))))
  (response/jsexpr response-json))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; `serve/servelet` won't let us combine multiple servelets, so instead
; we sequence two dispatchers and use `serve`
; Thanks to: https://defn.io/2020/02/12/racket-web-server-guide/

(define (request-path-has-prefix? req p)
  (string-prefix? (path->string (url->path (request-uri req))) p))

(define (make-dispatcher proc prefix)
  (lambda (conn req)
    (if (request-path-has-prefix? req prefix)
        (output-response conn (proc req))
        (next-dispatcher))))
(define (make-error-dispatcher)
  (printf "Request with unknown endpoint~n")
  (lambda (conn req)
    (output-response conn (response/jsexpr (hash 'error "invalid endpoint")))))

(define stop
  (serve
   #:dispatch (sequencer:make (make-dispatcher start-file "/load")
                              (make-dispatcher get-sat "/sat")
                              (make-error-dispatcher))
   #:listen-ip "127.0.0.1"
   #:port server-port))

(with-handlers ([exn:break? (lambda (e)
                              (stop))])
  (sync/enable-break never-evt))