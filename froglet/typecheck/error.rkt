#lang racket/base

(provide
  raise-type-error
  todo-not-implemented)

(require
  froglet/util)

;; ---

(define who 'froglet)

(define-syntax-rule (raise-type-error msg kv* ...)
  (raise-syntax-error who msg kv* ...))

(define (todo-not-implemented msg)
  (log-froglet-warning "not implemented: ~a" msg))

