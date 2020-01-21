#lang racket
;; Echo server, demonstrating the ws-serve interface.
;; Public Domain.

(module+ main
  (require "../../rfc6455.rkt")

  (define port 8081)
  (define idle-timeout #f)
  (command-line #:once-each
                ["--timeout" SECONDS "Set per-connection idle timeout"
                 (set! idle-timeout (string->number SECONDS))]
                ["--port" PORT "Set service port"
                 (set! port (string->number PORT))])

  (define (connection-handler c state)
    (define id (gensym 'conn))
    (printf "~a: Connection received\n" id)
    (let loop ()
      (match (ws-recv c #:payload-type 'text)
        [(? eof-object?) (void)]
        [m
         (printf "~a: Received: ~v\n" id m)
         (ws-send! c m)
         (loop)]))
    (printf "~a: Close status: ~v ~v\n"
            id
            (ws-conn-close-status c)
            (ws-conn-close-reason c))
    (ws-close! c)
    (printf "~a: Connection closed\n" id))

  (when idle-timeout (ws-idle-timeout idle-timeout))
  (define stop-service (ws-serve #:port port connection-handler))
  (printf "Server running. Hit enter to stop service.\n")
  (void (read-line))
  (stop-service))
