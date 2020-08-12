#lang racket

(require (rename-in (only-in "../lang/ast.rkt" node/expr/relation-name)
                    [node/expr/relation-name relation-name])
         "modelToXML.rkt" xml
         net/sendurl "../racket-rfc6455/net/rfc6455.rkt" net/url web-server/http/request-structs racket/runtime-path
         racket/async-channel
         racket/hash)
(require "eval-model.rkt")
(require "../kodkod-cli/server/kks.rkt")
; TODO: remove this once evaluator is in; just want to show we can evaluate something

(require "../lang/reader.rkt")
(require "../shared.rkt")
(require "../lang/reader.rkt")

(provide display-model)

;(define-runtime-path sterling-path "../sterling-js/dist/index.html")
(define-runtime-path sterling-path "../sterling/build/index.html")

; name is the name of the model
; get-next-model returns the next model each time it is called, or #f.
(define (display-model get-next-model evaluate name command filepath bitwidth funs-n-preds atom-rels)
  (define model (get-next-model))

  ; Begin hack to remove helper relations
  (define (clean-model)
    (when (equal? (car model) 'sat)
    (define instance (cdr model))
    (for ([rel atom-rels])
      (hash-remove! instance rel))))
  (clean-model)
  ; End hack to remove helper relations

  ;(printf "Instance : ~a~n" model)
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
                  (ws-send! connection (model-to-XML-string model name command filepath bitwidth forge-version))]
                 [(equal? m "next")
                  (set! model (get-next-model))
                  (clean-model)
                  (ws-send! connection (model-to-XML-string model name command filepath bitwidth forge-version))]
                 [(string-prefix? m "EVL:") ; (equal? m "eval-exp")
                  (define parts (regexp-match #px"^EVL:(\\d+):(.*)$" m))
                  (define command (third parts))
                  (define result (evaluate command))
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
