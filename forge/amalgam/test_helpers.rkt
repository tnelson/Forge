#lang racket

(provide to-string)

; helper that converts the output of substitutor to a string
(define (to-string output)
  (format "~v" output))
