#lang racket

(require (for-syntax syntax/parse))
(require (for-syntax "testing2.rkt"))
(require "testing2.rkt")

(provide (all-defined-out))



(define-syntax (run stx)
    (define-syntax-class node-expr
        (pattern x:expr #:when (R? (eval-syntax #'x))))

    (syntax-parse stx
        [(run (~var a node-expr))
         #'(display a)]))

(run (R))