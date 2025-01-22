#lang racket/base

; Interface between Sterling and Forge
; Using the December 2021 protocol found here:
;   https://sterling-docs.vercel.app/sterling-connection/receive

;;;;;;;;;;;;;;;;;;;;
; This module provides two very similar functions.
;
; start-sterling-menu:
;   should be called at the very end of the `execs` submodule, to start Sterling
;   proactively and provide a menu of commands to get instances for.
; display-model:
;   should be called for any context where a menu of commands doesn't fit (such as a
;   failing test, perhaps). 

(require (only-in forge/lang/ast relation-name raise-forge-error)
         forge/server/modelToXML
         forge/evaluator
         xml
         net/sendurl "../racket-rfc6455/net/rfc6455.rkt" net/url web-server/http/request-structs racket/runtime-path
         racket/async-channel
         racket/hash
         (only-in racket empty? first rest)
         (only-in forge/server/eval-model ->string)
         (prefix-in tree: forge/utils/lazy-tree)
         json
         racket/contract
         (prefix-in @ (only-in racket >= <= > <))
         syntax/srcloc
         forge/server/serve-sterling-static)
(require (only-in forge/lang/alloy-syntax/parser [parse forge-lang:parse])
         (only-in forge/lang/alloy-syntax/tokenizer [make-tokenizer forge-lang:make-tokenizer]))


; for verbosity option
(require forge/shared)
; for get-option
(require forge/sigs-structs)

(provide display-model start-sterling-menu)

(define-runtime-path sterling-path "../sterling/build/index.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;

; Get a value from a nested JSON dictionary, using the <path> list
; input must be a hash (i.e., a translated JSON dictionary)
(define/contract (get-from-json json-m path)
  (-> (and/c jsexpr? hash?) (listof symbol?) jsexpr?)
  (cond [(empty? path) json-m]
        [else 
         (unless (hash-has-key? json-m (first path))
           (error (format "get-from-json expected JSON dictionary with ~a field, got: ~a~n" (first path) json-m)))
         (get-from-json (hash-ref json-m (first path)) (rest path))]))

; name is the name of the model
; get-next-instance returns the next instance each time it is called, or #f.
(define (display-model
         the-run orig-lazy-tree relation-map evaluate-func name command
         filepath bitwidth funs-n-preds get-contrast-model-generator)
  (do-time "forgeserver display-model")

  (define state-for-run (Run-spec-state (Run-run-spec the-run)))

  (define current-tree orig-lazy-tree)
  (define curr-datum-id 1) ; nonzero
  (define id-to-instance-map (make-hash)) ; mutable hash
  
  (define (get-current-instance)
    (define returned-instance (tree:get-value current-tree))
    (set-box! (Run-last-sterling-instance the-run) returned-instance)
    returned-instance)
  (define (get-next-instance [next-mode 'P])    
    (set! current-tree (tree:get-child current-tree next-mode))
    (set! curr-datum-id (+ curr-datum-id 1))
    ; get-current-instance updates the Run's last sterling instance cursor when called
    (hash-set! id-to-instance-map curr-datum-id (get-current-instance))
    (values curr-datum-id (get-current-instance)))
  
  (define command-string (format "~a" (syntax->datum command)))
  
  (define (send-to-sterling m #:connection connection)
    (when (@>= (get-verbosity) VERBOSITY_STERLING) 
      (printf "Sending message to Sterling: ~a~n" m))
    (ws-send! connection m))
  
   (define (get-xml soln)    
    ;(define tuple-annotations (if (and (Sat? model) (equal? 'on (get-option the-run 'local_necessity)))
    ;                              (build-tuple-annotations-for-ln model)
    ;                              (hash)))
    ; TODO: disable LN for winter '21 dev
    (define tuple-annotations (hash))
    (when (@>= (get-verbosity) VERBOSITY_STERLING)
      (printf "tuple annotations were: ~a~n" tuple-annotations))
    (solution-to-XML-string soln relation-map name command-string filepath
                            bitwidth forge-version #:tuple-annotations tuple-annotations
                            #:run-options (State-options (Run-spec-state (Run-run-spec the-run)))))
  
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
          (define old-datum-id curr-datum-id)
          (define-values (datum-id inst)
            (cond [(equal? onClick "next-C") (get-next-instance 'C)]
                  [(equal? onClick "next-P") (get-next-instance 'P)]
                  [(equal? onClick "next") (get-next-instance)]
                  [else
                   (printf "Sterling: unexpected 'next' request type: ~a~n" json-m)]))
          (define xml (get-xml inst))
          (define response (make-sterling-data xml datum-id name temporal? (Sat? inst) old-datum-id))
          (send-to-sterling response #:connection connection)]
         [else
          (printf "Sterling: unexpected onClick: ~a~n" json-m)])     
       ]
      [(equal? (hash-ref json-m 'type) "data")
       ; A message requesting the current data to display to the user.
       ; This message will be sent when the connection is established
       ; (or re-established). Respond in turn with a data message.
       ; TODO: should we re-enable make-contrast-model-generators?
       (define inst (get-current-instance)) 
       (define id curr-datum-id)
       (define xml (get-xml inst))
       (define response (make-sterling-data xml id name temporal? (Sat? inst)))
       (send-to-sterling response #:connection connection)     
       ]
      [(equal? (hash-ref json-m 'type) "eval")
       ; A message requesting that the provider evaluate some expression
       ; associated with a specific datum. Respond with the result of
       ; evaluating the expression with an eval message.
       (define expression (get-from-json json-m '(payload expression)))
       (define id (get-from-json json-m '(payload id)))
       (define datum-id (get-from-json json-m '(payload datumId)))

       ; Racket-side this is an int; Sterling-side this is a string:
       (when (not (equal? (->string datum-id) (->string curr-datum-id)))
         (printf "Error: Sterling requested outdated evaluator (id=~a; curr-id=~a); reporting back inaccurate data!" datum-id curr-datum-id))
       (define result (evaluate-func expression))       
       (define response (make-sterling-eval result id datum-id))
       (send-to-sterling response #:connection connection)]     
      [(equal? (hash-ref json-m 'type) "meta")
       ; A message requesting data about the data provider, such as the provider's
       ; name and the types of views it supports. Respond in turn with a meta message.
       ; ***** In this function, only a single run is available. *****
       (send-to-sterling (make-sterling-meta (list name)) #:connection connection)]      
      [else
       (send-to-sterling "BAD REQUEST" #:connection connection)
       (printf "Sterling message contained unexpected type field: ~a~n" json-m)]))
  
  (define chan (make-async-channel))

  ; After this is defined, the service is active and should be listening
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
           (when (@>= (get-verbosity) VERBOSITY_STERLING) 
             (printf "Message received from Sterling: ~a~n" m))
           (cond [(equal? m "ping")
                  (send-to-sterling "pong" #:connection connection)]
                 [else (handle-json connection m)])
           (loop))))
     ; default #:port 0 will assign an ephemeral port
     #:port (get-option the-run 'sterling_port) #:confirmation-channel chan))

  (define port (async-channel-get chan))
  (cond [(string? port)
         (displayln "NO PORTS AVAILABLE. Unable to start web server and Sterling visualizer.")]
        [(equal? 'off (get-option the-run 'run_sterling))
         (void)]
        [else
         ; Attempt to open a browser to the Sterling index.html, with the proper port
         ; If this cannot be opened for whatever reason, keep the server open but print
         ; a warning, allowing the user a workaround.
         (with-handlers ([exn?
                          (lambda (e) (printf "Racket could not open a browser on your system; you may be able manually navigate to this address, which is where Forge expects Sterling to load:~n  ~a~n"
                                              (string-append (path->string sterling-path) "?" (number->string port))))])
           (send-url/file sterling-path #f #:query (number->string port)))
         
         (printf "Sterling running. Hit enter to stop service.\n")
         (when (> (get-verbosity) VERBOSITY_LOW)
           (printf "Using port: ~a~n" (number->string port)))
         (flush-output)
         (void (read-char))
         ; Once a character is read, stop the server
         (stop-service)]))

(define (make-sterling-data xml id run-name temporal? not-done? [old-id #f])
  (jsexpr->string
   (hash
    'type "data"
    'version 1
    'payload (hash
              'enter (list
                      (hash 'id id
                            'generatorName (->string run-name)
                            'format "alloy"
                            'data xml
                            'buttons (cond [(not not-done?) (list)]
                                           [temporal?
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
                            'evaluator not-done?))
                'update (if old-id 
                            (list (hash 'id old-id
                                        'actions (list)
                                        'evaluator #f))
                            (list))))))

(define (make-sterling-eval result id datum-id)
  ; note datum-id currently unused in the response JSON
  (jsexpr->string	
   (hash 'type "eval"
         'version 1
         'payload (hash 'id (->string id)
                        'result (->string result)))))

(define/contract (make-sterling-meta defined-run-names)
  (-> (listof symbol?) string?)
  (jsexpr->string	
   (hash 'type "meta"
         'version 1
         'payload (hash 'name "Forge"
                        'evaluator #t
                        'views (list "graph" "table" "script")
                        'generators (map ->string defined-run-names)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Infrastructure for starting up sterling in "command selector" mode
;; The namespace is passed to aid evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (start-sterling-menu curr-state nsa)
  ; The runs defined in the state at this point, which should be after all declarations.
  (define runmap (State-runmap curr-state))
  (define defined-run-names (hash-keys runmap))

  ; Assumption: if Sterling is the source of instance requests, there will be no
  ; instance requests seen from any other sources.

  ; Keep track of current instance context for each run. We do this because (Run-result r)
  ; is not modified, so that other parts of Forge can read the entire instance history if
  ; necessary. This is only populated lazily, to avoid running the solver at once on everything.
  (define current-tree-locations (make-hash)) ; mutable hash

  (when (> (get-verbosity) VERBOSITY_LOW)
    (printf "Starting Sterling in command-selection mode...~n")
    (printf "Available commands: ~a~n" defined-run-names))
  
  (define (send-to-sterling m #:connection connection)
    (when (@>= (get-verbosity) VERBOSITY_STERLING) 
      (printf "Sending message to Sterling: ~a~n" m))
    (ws-send! connection m))

  (define curr-datum-id 0) ; nonzero
  (define id-to-instance-map (make-hash)) ; mutable hash
  (define id-to-run-map (make-hash)) ; mutable hash

  
  ; This (large) handler processes incoming request messages from Sterling
  (define (handle-json connection m #:context [context #f])
    (define json-m
      (with-handlers ([exn:fail:syntax?
                       (lambda (e)                      
                         (send-to-sterling "BAD REQUEST" #:connection connection)
                         (raise-forge-error
                          #:msg (format "Expected JSON request from Sterling, got: ~a~n" m)
                          #:context context))])
        (string->jsexpr m)))
    (unless (and (hash? json-m) (hash-has-key? json-m 'type))
      (send-to-sterling "BAD REQUEST" #:connection connection)
      (raise-forge-error
       #:msg (format "Expected hash-table JSON request with type field, got: ~a~n" m)
       #:context context))
    
    (cond
      [(equal? (hash-ref json-m 'type) "click")
       ; A message notifying the data provider that a Button was clicked
       ; by the user. This may be in the context of a specific datum, entail a
       ; request for a datum from a (named) generator, or neither.
       (unless (and
                (hash-has-key? json-m 'payload)
                (hash-has-key? (hash-ref json-m 'payload) 'onClick)
                (hash-has-key? (hash-ref json-m 'payload) 'context)
                (hash-has-key? (hash-ref (hash-ref json-m 'payload) 'context) 'generatorName))
         (raise-forge-error
          #:msg (format "Got a click event from Sterling without a well-formed payload: ~a~n" json-m)
          #:context context))

       (define payload (hash-ref json-m 'payload))
       (define onClick (hash-ref payload 'onClick))
       (define context (hash-ref payload 'context))
       (define generatorName (string->symbol (hash-ref context 'generatorName)))
       (define next? (equal? "next" (substring onClick 0 4)))
       (unless (hash-has-key? runmap generatorName)
         (raise-forge-error
          #:msg (format "Sterling requested 'next' data for a nonexistent command: ~a~n" generatorName)
          #:context context))
         
       (define the-run (hash-ref runmap generatorName))
       (define temporal? (equal? (get-option the-run 'problem_type) 'temporal))
       (define name (Run-name the-run))

       ; Get the current location in this run's lazy result tree.
       (define (get-current-tree)
         (cond 
           [(hash-has-key? current-tree-locations name) (hash-ref current-tree-locations name)]
           [else (Run-result the-run)]))

       ; Get the _next_ location in this run's lazy result tree (no mutation)
       ; If we have not enumerated an instance yet, use the first one given.
       (define (get-next-tree [next-mode 'P])
         (cond 
           [(hash-has-key? current-tree-locations name)
            (tree:get-child (hash-ref current-tree-locations name) next-mode)]
           [else (Run-result the-run)]))

       ; Get the latest instance of this run's lazy result tree. Do not advance the tree pointer.
       (define (get-current-instance)
         (define returned-instance (tree:get-value (get-current-tree)))
         (set-box! (Run-last-sterling-instance the-run) returned-instance)
         returned-instance)
       
       ; Get the next instance of this run's lazy result tree. Advance the tree pointer.
       (define (get-next-instance [next-mode 'P])
         (define next-tree (get-next-tree next-mode))
         (set! curr-datum-id (+ curr-datum-id 1))

         ; update the pointer into the enumeration of instances
         (hash-set! current-tree-locations name next-tree)

         ; call get-current-instance to update the Run's last sterling cursor
         ; this must happen after current-tree-locations is updated.
         (hash-set! id-to-instance-map curr-datum-id (get-current-instance))
         (hash-set! id-to-run-map curr-datum-id the-run)
         
         (values curr-datum-id (get-current-instance)))

       ; The command string that corresponds to this generator.
       (define command-string (format "~a" (syntax->datum (Run-command the-run))))

       ; The filepath of the command. We could also get this, possibly (?) more 
       ; reliably, via (current-load-relative-directory) in the expander.)
       (define filepath (if (source-location-source (Run-command the-run))
                            (path->string (source-location-source (Run-command the-run)))
                            "/no-name.frg"))

       ; The bitwidth for this run.
       (define bitwidth (get-bitwidth (Run-run-spec the-run)))
       
       ; Helper to convert a solution to Alloy-format instance XML
       (define (get-xml soln)    
         (define tuple-annotations (hash)) ; no annotations at the moment
         (solution-to-XML-string soln
                                 (get-relation-map the-run) name command-string filepath
                                 bitwidth forge-version #:tuple-annotations tuple-annotations
                                 #:run-options (State-options (Run-spec-state (Run-run-spec the-run)))))

       (cond
        [next?          
         (define old-datum-id curr-datum-id)
         (define-values (datum-id inst)
           (cond [(equal? onClick "next-C") (get-next-instance 'C)]
                 [(equal? onClick "next-P") (get-next-instance 'P)]
                [(equal? onClick "next") (get-next-instance)]
                [else
                 (raise-forge-error
                  #:msg (format "Error: Sterling sent unexpected 'next' request type: ~a~n" json-m)
                  #:context context)]))
         (define xml (get-xml inst))
         ; is-running? is about the _solver process itself_.
         ; is-run-closed? is about whether this specific run has been terminated
         ; Sat? is about whether the solution we have is Sat.
         (define response (make-sterling-data xml datum-id name temporal?
                                              (and (is-running? the-run) (not (is-run-closed? the-run)) (Sat? inst))
                                              old-datum-id))
        (send-to-sterling response #:connection connection)]
       [else
        (raise-forge-error #:msg (format "Sterling: unexpected onClick: ~a~n" json-m)
                           #:context context)])]
      [(equal? (hash-ref json-m 'type) "data")
       ; A message requesting the current data to display to the user.
       ; This message will be sent when the connection is established
       ; (or re-established). Respond in turn with a data message.
       ; TODO: should we re-enable make-contrast-model-generators?
       ;;(define inst (get-current-instance)) 
       ;;(define id curr-datum-id)
       ;;(define xml (get-xml inst))
       ;;(define response (make-sterling-data xml id name temporal?))
       ;;(send-to-sterling response #:connection connection)
       (printf "Ignoring Sterling 'data' request...~n")
       ]
      [(equal? (hash-ref json-m 'type) "eval")
       ; A message requesting that the provider evaluate some expression
       ; associated with a specific datum. Respond with the result of
       ; evaluating the expression with an eval message.
       (define expression (get-from-json json-m '(payload expression)))
       (define id (get-from-json json-m '(payload id)))
       (define datum-id (string->number (get-from-json json-m '(payload datumId))))

       ;; Racket-side this is an int; Sterling-side this is a string:
       (when (not (equal? (->string datum-id) (->string curr-datum-id)))
         (raise-forge-error
          #:msg (format "Error: Sterling requested outdated evaluator (id=~a; curr-id=~a); reporting back inaccurate data!" datum-id curr-datum-id)
          #:context context))
       (unless (hash-has-key? id-to-run-map datum-id)
         (raise-forge-error
          #:msg (format "Error: Sterling provided an unknown instance ID in evaluator query (id=~a))" datum-id)
          #:context context))
       (define the-run (hash-ref id-to-run-map datum-id))
       
       (define (evaluate-func str-command)
         (define pipe1 (open-input-string str-command))
         (define pipe2 (open-input-string (format "eval ~a" str-command)))
         
         (with-handlers ([(lambda (x) #t) 
                          (lambda (exn) (exn-message exn))])
           ; Read command as syntax from pipe
           (define expr
             (cond [(equal? (get-option curr-state 'eval-language) 'surface)
                    (forge-lang:parse "/no-name" (forge-lang:make-tokenizer pipe2))]
                   [(equal? (get-option curr-state 'eval-language) 'core)
                    (read-syntax 'Evaluator pipe1)]
                   [else (raise-forge-error
                          #:msg "Could not evaluate in current language - must be surface or core."
                          #:context #f)]))
           
           ; Evaluate command
           (define full-command (datum->syntax #f `(let
                                                       ,(for/list ([atom (Run-atoms the-run)]
                                                                   #:when (symbol? atom))
                                                          `[,atom (atom ',atom)])
                                                     ,expr)))
           (define ns (namespace-anchor->namespace (nsa)))
           (define command (eval full-command ns))
           (evaluate the-run '() command)))
       
       
       (define result (evaluate-func expression))       
       (define response (make-sterling-eval result id datum-id))
       (send-to-sterling response #:connection connection)]     
      [(equal? (hash-ref json-m 'type) "meta")
       ; A message requesting data about the data provider, such as the provider's
       ; name and the types of views it supports. Respond in turn with a meta message.
       (send-to-sterling (make-sterling-meta defined-run-names) #:connection connection)]      
      [else
       (send-to-sterling "BAD REQUEST" #:connection connection)
       (raise-forge-error
        #:msg (format "Sterling message contained unexpected type field: ~a~n" json-m)
        #:context context)]))
  
  (define chan (make-async-channel))

  ; After this is defined, the service is active and should be listening
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
           (when (@>= (get-verbosity) VERBOSITY_STERLING) 
             (printf "Message received from Sterling: ~a~n" m))
           (cond [(equal? m "ping")
                  (send-to-sterling "pong" #:connection connection)]
                 [else (handle-json connection m)])
           (loop))))
     ; default #:port 0 will assign an ephemeral port
     #:port (get-option curr-state 'sterling_port) #:confirmation-channel chan))
  (define port (async-channel-get chan))
  (when (string? port)
    (printf "NO PORTS AVAILABLE. Could not start provider server.~n"))
  
  ; Now, serve the static sterling website files (this will be a different server/port).
  (unless (equal? 'off (get-option curr-state 'run_sterling))
    (serve-sterling-static #:provider-port port)))