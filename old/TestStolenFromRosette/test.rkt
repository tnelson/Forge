#lang racket

(require "core/term.rkt")

(define-syntax-rule (printf-smt arg ...)
  (printf arg ...))

(define-syntax (declare-const stx)
  (syntax-case stx ()
    [(_ id type)
    (identifier? #'id)
    (syntax/loc stx (define var (constant #'id type)))]))

