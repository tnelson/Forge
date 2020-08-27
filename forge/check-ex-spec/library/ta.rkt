#lang racket

(provide encrypt-file read-key)
(require "encrypt-decrypt/library.rkt")

(define (read-key loc)
  (read-key-from-file loc #t))