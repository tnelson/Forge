#lang racket

(require (only-in "../lang/ast.rkt" relation-name)
         "modelToXML.rkt" xml
         net/sendurl net/rfc6455 web-server/http/request-structs)

(provide display-model)

;(define-runtime-path static-files "static")

#|(define (model-trim model)
  (define newmodel (make-hash))
  (if (hash? model)
      (begin
        (hash-map model (lambda (k v) (if
                                       (not (equal? #\$ (string-ref (relation-name k) 0)))
                                       (begin
                                         (hash-set! newmodel k v)
                                         v)
                                       v)))
        newmodel)
      model))|#

; name is the name of the model
; get-next-model returns the next model each time it is called, or #f.
(define (display-model non-abstract-sig-names name get-next-model)

  ; Start the websocket server. ws-serve returns the function that stops the server.
  (define stop-service (ws-serve
                        
                        ; This is the connection handler function, it has total control over the connection
                        ; from the time that conn-headers finishes responding to the connection request, to the time
                        ; the connection closes. The server generates a new handler thread for this function
                        ; every time a connection is initiated.
                        (λ (connection _)
                          (let loop ()
               
                            ; The only thing we should be receiving is next-model requests, and pings.
                            (define m (ws-recv connection))
                            (printf "RECEIVED: |~a|\n" m)
               
                            (unless (eof-object? m)
                              (cond [(equal? m "ping")
                                     (ws-send! connection "pong")]
                                    [else
                                     (define nextmodel (get-next-model))
                                     (ws-send! connection (model-to-XML-string (get-next-model) non-abstract-sig-names))])
                              (loop))))
                        #:port 3000))
  
  ; Possible way to wait for the server to be properly set up:
  ; Constantly try to connect to it, from here, in a delayed loop.
  ; When successful, drop the connection and proceed to opening the browser.
  
  (send-url/file "../../sterling-static/index.html")
  (printf "Server running. Hit enter to stop service.\n")
  (void (read-line))
  (stop-service))
