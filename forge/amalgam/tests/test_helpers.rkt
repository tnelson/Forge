#lang racket/base

(provide toString)

; helper that converts the output of substitutor to a string
(define (toString output)
  (format "~v" output))
