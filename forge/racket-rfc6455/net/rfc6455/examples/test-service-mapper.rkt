#lang racket/base
;; Demonstrates the ws-serve* and ws-service-mapper server utilities.
;; Public Domain.

(module+ main
  (require racket/match)
  (require "../../rfc6455.rkt")

  (define (connection-handler c)
    (let loop ()
      (sync (handle-evt (alarm-evt (+ (current-inexact-milliseconds) 1000))
                        (lambda _
                          (ws-send! c "Waited another second")
                          (loop)))
            (handle-evt (ws-recv-evt c #:payload-type 'text)
                        (lambda (m)
                          (unless (eof-object? m)
                            (if (equal? m "goodbye")
                                (ws-send! c "Goodbye!")
                                (begin (ws-send! c (format "You said: ~v" m))
                                       (loop))))))))
    (ws-close! c))

  (define stop-service
    (ws-serve* #:port 8081
               (ws-service-mapper
                ["/test"
                 [(subprotocol) (lambda (c) (ws-send! c "This is the subprotocol handler"))]
                 [(#f) connection-handler]])))

  (printf "Server running. Hit enter to stop service.\n")
  (void (read-line))
  (stop-service))
