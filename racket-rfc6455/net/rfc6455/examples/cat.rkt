#lang racket/base
;; Simple pass-through cat-like client.
;; Public Domain.

(module+ main
  (require "../../rfc6455.rkt")
  (require net/url)
  (require racket/cmdline)
  (require (only-in racket/port read-line-evt))

  (define server "ws://localhost:8081/")
  (define close-status 1000)
  (define close-reason "")
  (command-line #:once-each
                ["--server" URL "URL of Websocket server to contact"
                 (set! server URL)]
                ["--status" CODE "Status code to use during WebSocket close"
                 (set! close-status (string->number CODE))]
                ["--reason" MESSAGE "Explanatory text to use during close"
                 (set! close-reason MESSAGE)])

  (define c (ws-connect (string->url server)))
  (let loop ()
    (sync (handle-evt (read-line-evt (current-input-port) 'any)
                      (lambda (line)
                        (if (eof-object? line)
                            (void)
                            (begin (ws-send! c line)
                                   (loop)))))
          (handle-evt (ws-recv-evt c #:payload-type 'text)
                      (lambda (line)
                        (if (eof-object? line)
                            (void)
                            (begin (printf "~a\n" line)
                                   (loop)))))))
  (ws-close! c
             #:status close-status
             #:reason close-reason))
