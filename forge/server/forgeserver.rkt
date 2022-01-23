#lang racket

; Interface between Sterling and Forge
; Using the December 2021 protocol found here:
;   https://sterling-docs.vercel.app/sterling-connection/receive

(require (only-in "../lang/ast.rkt" relation-name)
         "modelToXML.rkt" xml
         net/sendurl "../racket-rfc6455/net/rfc6455.rkt" net/url web-server/http/request-structs racket/runtime-path
         racket/async-channel
         racket/hash)
(require (only-in "eval-model.rkt" ->string)
         (prefix-in tree: "../lazy-tree.rkt"))        
(require json) 

(require "../pardinus-cli/server/kks.rkt")

(require "../lang/reader.rkt")
(require "../shared.rkt")
(require "../lang/reader.rkt")
(require "../sigs-structs.rkt")
(require forge/amalgam)
(require (prefix-in logging: forge/logging/logging))

(provide display-model)

(define-runtime-path sterling-path "../sterling/build/index.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;



; name is the name of the model
; get-next-model returns the next model each time it is called, or #f.
(define (display-model the-run orig-lazy-tree relation-map evaluate-func name command filepath bitwidth funs-n-preds get-contrast-model-generator)

  (define current-tree orig-lazy-tree)
  (define curr-datum-id 1) ; nonzero
  (define id-to-instance-map (make-hash)) ; mutable hash
  
  (define (get-current-model)
    (tree:get-value current-tree))
  (define (get-next-model [next-mode 'P])    
    (set! current-tree (tree:get-child current-tree next-mode))
    (set! curr-datum-id (+ curr-datum-id 1))
    (hash-set! id-to-instance-map curr-datum-id (get-current-model))    
    (values curr-datum-id (get-current-model)))
  
  (define command-string (format "~a" (syntax->datum command)))
  
  (define (send-to-sterling m #:connection connection)
    (when (>= (get-verbosity) VERBOSITY_STERLING) 
      (printf "Sending message to Sterling: ~a~n" m))
    (ws-send! connection m))
  
   (define (get-xml model)    
    ;(define tuple-annotations (if (and (Sat? model) (equal? 'on (get-option the-run 'local_necessity)))
    ;                              (build-tuple-annotations-for-ln model)
    ;                              (hash)))
    ; TODO: disable LN for winter '21 dev
    (define tuple-annotations (hash))
    (when (>= (get-verbosity) VERBOSITY_STERLING)
      (printf "tuple annotations were: ~a~n" tuple-annotations))
    (solution-to-XML-string model relation-map name command-string filepath bitwidth forge-version #:tuple-annotations tuple-annotations))
  
  (define (handle-json connection m)
    (define json-m
      (with-handlers ([exn:fail:syntax?
                       (lambda (e)                      
                         (send-to-sterling "BAD REQUEST" #:connection connection)
                         (printf "Expected JSON request from Sterling, got: ~a~n" m))])
        (string->jsexpr m)))
    (unless (and (hash? json-m) (hash-has-key? json-m 'type))
      (send-to-sterling "BAD REQUEST" #:connection connection)
      (printf "Expected hash-table JSON request with type field, got: ~a~n" m))

    (define temporal? (equal? (get-option the-run 'problem_type) 'temporal))
    
    (cond
      [(equal? (hash-ref json-m 'type) "click")
       ; A message notifying the data provider that a Button was clicked
       ; by the user while viewing a specific datum.
       (unless (hash-has-key? (hash-ref json-m 'payload) 'onClick)
         (printf "Got a click event from Sterling without a payload: ~a~n" json-m))
       (define onClick (hash-ref (hash-ref json-m 'payload) 'onClick))
       (define next? (equal? "next" (substring onClick 0 4)))
       (cond
         [next?          
          (define-values (datum-id inst)
            (cond [(equal? onClick "next-C") (get-next-model 'C)]
                  [(equal? onClick "next-P") (get-next-model 'P)]
                  [(equal? onClick "next") (get-next-model)]
                  [else
                   (printf "Sterling: unexpected 'next' request type: ~a~n" json-m)]))
          (define xml (get-xml inst))
          (define response (make-sterling-data xml datum-id temporal?))
          (send-to-sterling response #:connection connection)]
         [else
          (printf "Sterling: unexpected onClick: ~a~n" json-m)])     
       ]
      [(equal? (hash-ref json-m 'type) "data")
       ; A message requesting the current data to display to the user.
       ; This message will be sent when the connection is established
       ; (or re-established). Respond in turn with a data message.
       ; TODO: should we re-enable make-contrast-model-generators?
       (define inst (get-current-model)) 
       (define id curr-datum-id)
       (define xml (get-xml inst))
       (define response (make-sterling-data xml id temporal?))
       (send-to-sterling response #:connection connection)     
       ]
      [(equal? (hash-ref json-m 'type) "eval")
       ; A message requesting that the provider evaluate some expression
       ; associated with a specific datum. Respond with the result of
       ; evaluating the expression with an eval message.
       (define expression (get-from-json json-m '(payload expression)))
       (define id (get-from-json json-m '(payload id)))
       (define datum-id (get-from-json json-m '(payload datumId)))
       (when (not (equal? datum-id curr-datum-id))
         (printf "Error: Sterling requested outdated evaluator; reporting back inaccurate data!"))
       (define result (evaluate-func expression))       
       (define response (make-sterling-eval result id datum-id))
       (send-to-sterling response #:connection connection)]     
      [(equal? (hash-ref json-m 'type) "meta")
       ; A message requesting data about the data provider, such as the provider's
       ; name and the types of views it supports. Respond in turn with a meta message.
       (send-to-sterling (make-sterling-meta) #:connection connection)]      
      [else
       (send-to-sterling "BAD REQUEST" #:connection connection)
       (printf "Sterling message contained unexpected type field: ~a~n" json-m)]))
  
  (define chan (make-async-channel))

  (define stop-service
    (ws-serve
     ; This is the connection handler function, it has total control over the connection
     ; from the time that conn-headers finishes responding to the connection request, to the time
     ; the connection closes. The server generates a new handler thread for this function
     ; every time a connection is initiated.
     (λ (connection _)       
       (let loop ()         
         (define m (ws-recv connection))
         (unless (eof-object? m)           
           (when (>= (get-verbosity) VERBOSITY_STERLING) 
             (printf "Message received from Sterling: ~a~n" m))
           (cond [(equal? m "ping")
                  (send-to-sterling "pong" #:connection connection)]
                 [else (handle-json connection m)])
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

; Get a value from a nested JSON dictionary, using the <path> list
; input must be a hash (i.e., a translated JSON dictionary)
(define/contract (get-from-json json-m path)
  (-> (and/c jsexpr? hash?) (listof symbol?) jsexpr?)
  (cond [(empty? path) json-m]
        [else 
         (unless (hash-has-key? json-m (first path))
           (error (format "get-from-json expected JSON dictionary with ~a field, got: ~a~n" (first path) json-m)))
         (get-from-json (hash-ref json-m (first path)) (rest path))]))

(define (make-sterling-data xml id temporal?)
  (jsexpr->string
   (hash
    'type "data"
    'version 1
    'payload (hash
              'enter (list
                      (hash 'id id
                            'format "alloy"
                            'data xml
                            'buttons (cond [temporal?
                                            (list (hash 'text "Next Trace"
                                                        'mouseover "(Keeps configuration constant)"
                                                        'onClick "next-P")
                                                  (hash 'text "Next Config"
                                                        'mouseover "(Forces different configuration)"
                                                        'onClick "next-C"))]
                                           [else
                                            (list (hash 'text "Next"
                                                        'mouseover "(Get next instance)"
                                                        'onClick "next"))])                            
                            'evaluator #t))))))

(define (make-sterling-eval result id datum-id)
  ; note datum-id currently unused in the response JSON
  (jsexpr->string	
   (hash 'type "eval"
         'version 1
         'payload (hash 'id (->string id)
                        'result (->string result)))))

(define/contract (make-sterling-meta)
  (-> string?)
  (jsexpr->string	
   (hash 'type "meta"
         'version 1
         'payload (hash 'name "Forge"
                        'evaluator #t
                        'views (list "graph" "table" "script")))))
  


#|
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
|#



 ; For compare/contrast models.
  ; Map of generators
  ;  (define contrast-model-generators #f)
  ;  (define contrast-models #f)

  ; TODO: TN, re-enable these
  ;  (define (make-contrast-model-generators)
  ;    (define make-generator (curry get-contrast-model-generator model))
  ;    (set! contrast-model-generators
  ;      (hash 'compare-min (make-generator 'compare 'close)
  ;            'compare-max (make-generator 'compare 'far)
  ;            'contrast-min (make-generator 'contrast 'close)
  ;            'contrast-max (make-generator 'contrast 'far)))
  ;
  ;    (set! contrast-models
  ;      (make-hash (list (cons 'compare-min #f)
  ;                       (cons 'compare-max #f)
  ;                       (cons 'contrast-min #f)
  ;                       (cons 'contrast-max #f)))))
  ;(make-contrast-model-generators)

  ;  (define (get-current-contrast-model type)
  ;    (hash-ref contrast-models type))
  ;  (define (get-next-contrast-model type)
  ;    (hash-set! contrast-models type 
  ;               ((hash-ref contrast-model-generators type)))
  ;    (hash-ref contrast-models type))

  ;  ; Define a hashof(relname-symbol, hashof(listof(atom-symbol), listof(annotation-symbol))    
  ;  (define (build-tuple-annotations-for-ln model)
  ;    (define (build-tann-hash relname pair-list ksym vsym)
  ;      (for/hash ([pr (filter (lambda (maybe-pr) (equal? relname (cdr maybe-pr))) pair-list)])
  ;        ; build *LIST* of annotations, each of which is a pair
  ;        (values (car pr) (list (cons ksym vsym))))) 
  ;    
  ;    ; only for the first element of a trace TODO
  ;    (when (> (get-verbosity) VERBOSITY_LOW)
  ;      (printf "generating locally-necessary tuples...model field unused...~n"))
  ;    (match-define (cons yes no) (get-locally-necessary-list the-run (get-current-model)))
  ;    ; To ease building annotation hash, just discover which relations are present in advance
  ;    ;(printf "LNtuples+: ~a~n LNtuples-: ~a~n" yes no)
  ;    (for/hash ([relname (remove-duplicates (map cdr (append yes no)))])
  ;      (let ([true-hash (build-tann-hash relname yes 'LN 'true)]
  ;            [false-hash (build-tann-hash relname no 'LN 'false)])
  ;      ; uppercase
  ;      ;(printf "building union of ~a~n  and ~a~n" true-hash false-hash)
  ;      (values relname (hash-union true-hash false-hash)))))
  ;  
