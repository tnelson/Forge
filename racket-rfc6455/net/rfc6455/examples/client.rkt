#lang racket/base
;; Demonstrates the ws-connect interface.
;; Public Domain.

(module+ main
  (require "../../rfc6455.rkt")
  (require net/url)

  (define (recv/print c)
    (printf "Got message: ~a\n" (ws-recv c)))

  (define (do-connection protocol)
    (printf "Connecting using protocol ~a...\n" protocol)
    (define c (ws-connect (string->url "ws://localhost:8081/test?foo=bar") #:protocol protocol))
    (ws-send! c (format "Hello from Racket WS client, protocol variant ~a" protocol))
    (for ((i 5)) (recv/print c))
    (ws-send! c "goodbye")
    (recv/print c)
    (ws-close! c))

  (do-connection 'rfc6455)
  (do-connection 'hybi00))
