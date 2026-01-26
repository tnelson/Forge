#lang racket/base 

(require (only-in racket/stream stream? stream-empty? empty-stream stream-first stream-rest stream-cons))
(provide stream-map/once)

(define (stream-map/once func strm)
      (stream-cons 
        (func (stream-first strm))
              (stream-map/once func (stream-rest strm))))
