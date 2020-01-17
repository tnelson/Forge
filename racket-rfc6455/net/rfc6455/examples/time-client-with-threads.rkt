#lang racket/base
;; Client for time-server.rkt.
;; Public Domain.

(module+ main
  (require "../../rfc6455.rkt")
  (require net/url)

  (void
   (thread
    (lambda ()
      (printf "Connecting...\n")
      (define c (ws-connect (string->url "ws://localhost:8081/")))
      (printf "Connected.\n")
      (ws-send! c "Hello from time-client-with-threads.rkt")
      (let loop ()
        (define m (ws-recv c))
        (printf "Got message: ~a\n" m)
        (unless (eof-object? m)
          (loop)))
      (printf "Server disconnected. (You probably still need to press ENTER to quit.)\n"))))

  (printf "Connecting in a background thread. Press ENTER to quit.\n")
  (let loop ()
    (unless (equal? (read-line) "")
      (loop))))
