#lang racket/base

(provide
  raise-type-error
  raise-operator-error
  todo-not-implemented)

(require
  froglet/util)

;; ---

(define who 'froglet)

(define-syntax-rule (raise-type-error msg kv* ...)
  (raise-syntax-error who msg kv* ...))

(define-syntax-rule (raise-operator-error op)
  (raise-type-error (format "~a operator is not permitted" (syntax-e op)) op))

(define (todo-not-implemented msg)
  (log-froglet-warning "not implemented: ~a" msg))

