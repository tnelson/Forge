#lang racket/base
;; Client for time-server.rkt.
;; Public Domain.

(module+ main
  (require "../../rfc6455.rkt")
  (require net/url)

  (printf "Connecting...\n")
  (define c (ws-connect (string->url "ws://localhost:8081/")))
  (printf "Connected. Press ENTER to quit.\n")

  (ws-send! c "Hello from time-client-with-sync.rkt")
  (let loop ()
    (sync (handle-evt c
                      (lambda (m)
                        (printf "Got message: ~a\n" m)
                        (unless (eof-object? m)
                          (loop))))
          (handle-evt (current-input-port)
                      (lambda _
                        (unless (equal? (read-line) "")
                          (loop))))))

  (printf "Disconnecting.\n")
  (ws-close! c))
