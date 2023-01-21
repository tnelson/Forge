#lang racket/base

(provide
  raise-type-error
  raise-operator-error
  raise-form-error
  raise-unknown-sig-error
  raise-unknown-pred-error
  todo-not-implemented)

(require
  froglet/util)

;; ---

(define who 'froglet)

(define (raise-type-error msg . kv*)
  (apply raise-syntax-error who msg kv*))

(define (raise-operator-error op)
  (raise-type-error (format-id-error "operator is not permitted" op) op))

(define (raise-form-error id)
  (raise-type-error (format-id-error "is not part of the language" id) id))

(define (raise-unknown-sig-error id)
  (raise-unknown-id-error id 'sig))

(define (raise-unknown-pred-error id)
  (raise-unknown-id-error id 'pred))

(define (raise-unknown-id-error id what)
  (raise-type-error (format "unknown ~a name" what) id))

(define (format-id-error msg id)
  (format "~a ~a" (syntax-e id) msg))

(define-syntax-rule (raise-operator-error op)
  (raise-type-error (format "~a operator is not permitted" (syntax-e op)) op))

(define (todo-not-implemented msg)
  (log-froglet-warning "not implemented: ~a" msg))

