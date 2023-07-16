#lang racket

(define (read-syntax path port)
  (define src-lines (port->lines port))
  (datum->syntax #f '(module lucy br
                       src-lines)))
(provide read-syntax)