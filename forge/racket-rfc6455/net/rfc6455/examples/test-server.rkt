#lang racket/base
;; Demonstrates the ws-serve interface.
;; Public Domain.

(module+ main
  (require "../../rfc6455.rkt")

  (require racket/cmdline)

  (define port 8081)
  (define idle-timeout #f)
  (command-line #:once-each
                ["--timeout" SECONDS "Set per-connection idle timeout"
                 (set! idle-timeout (string->number SECONDS))]
                ["--port" PORT "Set service port"
                 (set! port (string->number PORT))])

  (define (connection-handler c state)
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

  (when idle-timeout
    (ws-idle-timeout idle-timeout))
  (define stop-service
    (ws-serve #:port port connection-handler))

  (printf "Server running. Hit enter to stop service.\n")
  (void (read-line))
  (stop-service))
