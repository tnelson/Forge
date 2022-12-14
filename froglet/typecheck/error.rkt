#lang racket/base

(provide
  raise-type-error
  err:undefined-sig)

;; ---

(define-syntax-rule (raise-type-error msg kv* ...)
  ;; TODO make this useful
  (raise-syntax-error 'froglet:typecheck msg kv* ...))

(define err:undefined-sig "Undefined sig")
