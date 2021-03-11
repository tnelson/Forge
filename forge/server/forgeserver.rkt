#lang racket

(require (only-in "../lang/ast.rkt" relation-name)
         "modelToXML.rkt" xml
         net/sendurl "../racket-rfc6455/net/rfc6455.rkt" net/url web-server/http/request-structs racket/runtime-path
         racket/async-channel
         racket/hash)
(require "eval-model.rkt"
         (prefix-in tree: "../lazy-tree.rkt"))

(require "../pardinus-cli/server/kks.rkt")

(require "../lang/reader.rkt")
(require "../shared.rkt")
(require "../lang/reader.rkt")
(require "../sigs-structs.rkt")
(require forge/amalgam)
(require (prefix-in logging: forge/logging/logging))

(provide display-model)

;(define-runtime-path sterling-path "../sterling-js/dist/index.html")
(define-runtime-path sterling-path "../sterling/build/index.html")

;;;;;;;;;;;;;;;;;;;;
; Sent to Sterling before the instance XML for temporal problems only
  (define temporal-setup "{\n\
    \"type\": \"buttons\",\n\
    \"buttons\": [\n\
    {\n\
      \"text\": \"Next Config\",\n\
      \"command\": \"next-C\",\n\
      \"icon\": \"circle-arrow-right\",\n\
      \"disabled\": false\n\
    },\n\
    {\n\
      \"text\": \"Next\",\n\
      \"command\": \"next\",\n\
      \"icon\": \"circle-arrow-right\",\n\
      \"disabled\": false\n\
    }\n\
    ] }\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;



; name is the name of the model
; get-next-model returns the next model each time it is called, or #f.
(define (display-model the-run orig-lazy-tree relation-map evaluate-func name command filepath bitwidth funs-n-preds get-contrast-model-generator)

  (define current-tree orig-lazy-tree)    
  (define (get-current-model)
    (tree:get-value current-tree))
  (define (get-next-model [next-mode 'P])    
    (set! current-tree (tree:get-child current-tree next-mode))    
    (get-current-model))

  ; For compare/contrast models.
  ; Map of generators
  (define contrast-model-generators #f)
  (define contrast-models #f)

  ; TODO: TN, re-enable these
  #;(define (make-contrast-model-generators)
    (define make-generator (curry get-contrast-model-generator model))
    (set! contrast-model-generators
      (hash 'compare-min (make-generator 'compare 'close)
            'compare-max (make-generator 'compare 'far)
            'contrast-min (make-generator 'contrast 'close)
            'contrast-max (make-generator 'contrast 'far)))

    (set! contrast-models
      (make-hash (list (cons 'compare-min #f)
                       (cons 'compare-max #f)
                       (cons 'contrast-min #f)
                       (cons 'contrast-max #f)))))
  ;(make-contrast-model-generators)

  (define (get-current-contrast-model type)
    (hash-ref contrast-models type))
  (define (get-next-contrast-model type)
    (hash-set! contrast-models type 
               ((hash-ref contrast-model-generators type)))
    (hash-ref contrast-models type))

  (define command-string (format "~a" (syntax->datum command)))

  ; Define a hashof(relname-symbol, hashof(listof(atom-symbol), listof(annotation-symbol))    
  (define (build-tuple-annotations-for-ln model)
    (define (build-tann-hash relname pair-list ksym vsym)
      (for/hash ([pr (filter (lambda (maybe-pr) (equal? relname (cdr maybe-pr))) pair-list)])
        ; build *LIST* of annotations, each of which is a pair
        (values (car pr) (list (cons ksym vsym))))) 
    
    ; only for the first element of a trace TODO
    (when (> (get-verbosity) VERBOSITY_LOW)
      (printf "generating locally-necessary tuples...model field unused...~n"))
    (match-define (cons yes no) (get-locally-necessary-list the-run (get-current-model)))
    ; To ease building annotation hash, just discover which relations are present in advance
    ;(printf "LNtuples+: ~a~n LNtuples-: ~a~n" yes no)
    (for/hash ([relname (remove-duplicates (map cdr (append yes no)))])
      (let ([true-hash (build-tann-hash relname yes 'LN 'true)]
            [false-hash (build-tann-hash relname no 'LN 'false)])
      ; uppercase
      ;(printf "building union of ~a~n  and ~a~n" true-hash false-hash)
      (values relname (hash-union true-hash false-hash)))))
  
  (define (get-xml model)    
    (define tuple-annotations (if (and (Sat? model) (equal? 'on (get-option the-run 'local_necessity)))
                                  (build-tuple-annotations-for-ln model)
                                  (hash)))
    (when (> (get-verbosity) VERBOSITY_LOW)
      (printf "tuple annotations were: ~a~n" tuple-annotations))
    (solution-to-XML-string model relation-map name command-string filepath bitwidth forge-version #:tuple-annotations tuple-annotations))


  ;(printf "Instance : ~a~n" model)
  (define chan (make-async-channel))

  (define stop-service
    (ws-serve
     ; This is the connection handler function, it has total control over the connection
     ; from the time that conn-headers finishes responding to the connection request, to the time
     ; the connection closes. The server generates a new handler thread for this function
     ; every time a connection is initiated.
     (λ (connection _)
       ; Enable custom temporal exploration buttons for temporal mode
       (when (equal? (get-option the-run 'problem_type) 'temporal)                     
                     (ws-send! connection temporal-setup))                    

       (let loop ()

         ; The only thing we should be receiving is next-model requests, current requests (from a new connection), and pings.
         (define m (ws-recv connection))

         (unless (eof-object? m)
           ;(println m)
           (cond [(equal? m "ping")
                  (when (> (get-verbosity) VERBOSITY_LOW)
                    (printf "RECEIVED: ping~n"))
                  (ws-send! connection "pong")]
                 [(equal? m "current")
                  (when (> (get-verbosity) VERBOSITY_LOW)
                    (printf "RECEIVED: current~n"))
                  (ws-send! connection (get-xml (get-current-model)))]

                 [(equal? m "next-C") 
                  (when (> (get-verbosity) VERBOSITY_LOW)
                    (printf "RECEIVED: next-C~n"))
                  (get-next-model 'C)                  
                  (ws-send! connection (get-xml (get-current-model)))]

                 [(equal? m "next")
                  (when (> (get-verbosity) VERBOSITY_LOW)
                    (printf "RECEIVED: next~n"))
                  (get-next-model)
                  ; TN disabled for now 01/25/2021
                  ;(make-contrast-model-generators)
                  (ws-send! connection (get-xml (get-current-model)))]

                 ; Sterling is notifying Forge of some event, so that Forge can log or take action
                 ;“NOTIFY:${view}:${LN}“, so you’ll have four possible messages:
                 ;NOTIFY:GRAPH:LN+
                 ;NOTIFY:GRAPH:LN-
                 ;NOTIFY:TABLE:LN+
                 ;NOTIFY:TABLE:LN-
                 [(string-prefix? m "NOTIFY:")
                  (when (> (get-verbosity) VERBOSITY_LOW)
                    (printf "RECEIVED: notification (TODO: log if enabled)~n"))
                  (logging:log-notification m #f)
                  ;; TODO Log )
                  ; No reply needed
                  ]
                 
                 [(equal? m "compare-min")
                  (when (> (get-verbosity) VERBOSITY_LOW)
                    (printf "RECEIVED: compare-min~n"))
                  (define contrast-model (get-next-contrast-model 'compare-min))
                  (ws-send! connection (string-append "CMP:MIN:" (get-xml contrast-model)))]

                 [(equal? m "compare-max")
                  (when (> (get-verbosity) VERBOSITY_LOW)
                    (printf "RECEIVED: compare-max~n"))
                  (define contrast-model (get-next-contrast-model 'compare-max))
                  (ws-send! connection (string-append "CMP:MAX:" (get-xml contrast-model)))]

                 [(equal? m "contrast-min")
                  (when (> (get-verbosity) VERBOSITY_LOW)
                    (printf "RECEIVED: contrast-min~n"))
                  (define contrast-model (get-next-contrast-model 'contrast-min))
                  (ws-send! connection (string-append "CST:MIN:" (get-xml contrast-model)))]

                 [(equal? m "contrast-max")
                  (when (> (get-verbosity) VERBOSITY_LOW)
                    (printf "RECEIVED: contrast-max~n"))
                  (define contrast-model (get-next-contrast-model 'contrast-max))
                  (ws-send! connection (string-append "CST:MAX:" (get-xml contrast-model)))]

                 [(string-prefix? m "EVL:") ; (equal? m "eval-exp")
                  (define parts (regexp-match #px"^EVL:(\\d+):(.*)$" m))
                  (define command (third parts))
                  (when (> (get-verbosity) VERBOSITY_LOW)
                    (printf "Eval query: ~a~n" command))
                  (define result (evaluate-func command))
                  (ws-send! connection (format "EVL:~a:~a" (second parts) result))]
                 [else
                  (ws-send! connection "BAD REQUEST")
                  (printf "Bad request: ~a~n" m)])
           (loop))))
     #:port 0 #:confirmation-channel chan))

  (define port (async-channel-get chan))

  (cond [(string? port)
         (displayln "NO PORTS AVAILABLE!!")]
        [else
         (send-url/file sterling-path #f #:query (number->string port))
         (printf "Sterling running. Hit enter to stop service.\n")         
         (flush-output)
         (void (read-char))
         (stop-service)]))
