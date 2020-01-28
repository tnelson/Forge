#lang racket

(require (only-in "../lang/ast.rkt" relation-name)
         "modelToXML.rkt" xml
         net/sendurl "../racket-rfc6455/net/rfc6455.rkt" net/url web-server/http/request-structs racket/runtime-path
         racket/async-channel)
(require "eval-model.rkt")
; TODO: remove this once evaluator is in; just want to show we can evaluate something
(require (only-in "../lang/ast.rkt" univ))

(require "../lang/reader.rkt")

(provide display-model)


(define-runtime-path sterling-path "../sterling-js/dist/index.html")

; name is the name of the model
; get-next-model returns the next model each time it is called, or #f.
(define (display-model get-next-model name command filepath bitwidth)
  (define model (get-next-model))
  (define chan (make-async-channel))
  
  (define stop-service
    (ws-serve
     ; This is the connection handler function, it has total control over the connection
     ; from the time that conn-headers finishes responding to the connection request, to the time
     ; the connection closes. The server generates a new handler thread for this function
     ; every time a connection is initiated.
     (λ (connection _)
       (let loop ()
               
         ; The only thing we should be receiving is next-model requests, current requests (from a new connection), and pings.
         (define m (ws-recv connection))
               
         (unless (eof-object? m)
           ;(println m)
           (cond [(equal? m "ping")
                  (ws-send! connection "pong")]
                 [(equal? m "current")
                  (ws-send! connection (model-to-XML-string model name command filepath bitwidth))]
                 [(equal? m "next")
                  (set! model (get-next-model))
                  (ws-send! connection (model-to-XML-string model name command filepath bitwidth))]
                 [(string-prefix? m "EVL:") ; (equal? m "eval-exp")
                  (define parts (regexp-match #px"^EVL:(\\d+):(.*)$" m))
                  (define command (third parts))

                  ;(define stringPortFromEvaluator (open-input-string (string-append "eval " command)))
                  ;(define stxFromEvaluator (read-syntax 'Evaluator stringPortFromEvaluator))
                  ;(println (eval (last (syntax->datum stxFromEvaluator))))
                  ;(dynamic-require 'kkcli #f)
                  
                  (define stringPortFromEvaluator (open-input-string command))
                  (define stxFromEvaluator (read stringPortFromEvaluator))
                  ;(println stxFromEvaluator)

                  ; TODO: convert via Expr macro
                  ; faking it to make progress
                  ;(define exp univ)
                  (define maxint 8)
                  ; TODO: use eval-form if formula
                  ;(define result (eval-exp exp (model->binding model) maxint))
                  (define result (eval-exp stxFromEvaluator (model->binding (cdr model)) maxint))
                  ;(println result)
                  ;(printf "result: ~a~n" result)
                  
                  (ws-send! connection (format "EVL:~a:~a" (second parts) result))] 
                 [else
                  (ws-send! "BAD REQUEST")])
           (loop))))
     #:port 0 #:confirmation-channel chan))

  (define port (async-channel-get chan))

  (cond [(string? port)
         (displayln "NO PORTS AVAILABLE!!")]
        [else
         (send-url/file sterling-path #f #:query (number->string port))
         (printf "Sterling running. Hit enter to stop service.\n")
         (void (read-char))
         (stop-service)]))
