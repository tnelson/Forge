#lang racket

(require (only-in "../lang/ast.rkt" relation-name)
         "modelToXML.rkt" xml
         net/sendurl "../../racket-rfc6455/net/rfc6455.rkt" net/url web-server/http/request-structs racket/runtime-path
         racket/async-channel)

(provide display-model)

(define-runtime-path sterling-path "../../sterling-static/index.html")

; name is the name of the model
; get-next-model returns the next model each time it is called, or #f.
(define (display-model name get-next-model)

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
           (displayln m)
           (cond [(equal? m "ping")
                  (ws-send! connection "pong")]
                 [(equal? m "current")
                  (ws-send! connection (model-to-XML-string model))]
                 [(equal? m "next")
                  (set! model (get-next-model))
                  (ws-send! connection (model-to-XML-string model))]
                 [else
                  (ws-send! "BAD REQUEST")])
           (loop))))
     #:port 0 #:confirmation-channel chan))

  (define port (async-channel-get chan))

  (cond [(string? port)
         (displayln "NO PORTS AVAILABLE!!")]
        [else
         (send-url/file sterling-path #:query (number->string port))
         (printf "Sterling running. Hit enter to stop service.\n")
         (void (read-char))
         (stop-service)]))
