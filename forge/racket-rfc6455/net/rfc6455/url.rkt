#lang racket/base

(require net/url)

(provide ws-url?
	 wss-url?)

(define (ws-url? x)
  (and (url? x)
       (or (equal? (url-scheme x) "ws")
	   (equal? (url-scheme x) "wss"))))

(define (wss-url? x)
  (and (url? x)
       (equal? (url-scheme x) "wss")))
