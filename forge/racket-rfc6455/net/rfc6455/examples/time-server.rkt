#lang racket/base
;; Time server, demonstrating the ws-serve interface.
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

  (define next-connection-id 0)

  (define (connection-handler c state)
    (define id next-connection-id)
    (set! next-connection-id (+ next-connection-id 1)) ;; not thread safe
    (printf "Connection received: ~v\n" id)
    (let loop ((deadline (current-inexact-milliseconds)))
      (sync (handle-evt (alarm-evt deadline)
                        (lambda _
                          (ws-send! c (number->string deadline))
                          (loop (+ deadline 1000))))
            (handle-evt (ws-recv-evt c #:payload-type 'text)
                        (lambda (m)
                          (unless (eof-object? m)
                            (printf "Ignoring message ~v received from ~v\n" m id)
                            (loop deadline))))))
    (ws-close! c)
    (printf "Connection closed: ~v\n" id))

  (when idle-timeout
    (ws-idle-timeout idle-timeout))
  (define stop-service
    (ws-serve #:port port connection-handler))

  (printf "Server running. Hit enter to stop service.\n")
  (void (read-line))
  (stop-service))
