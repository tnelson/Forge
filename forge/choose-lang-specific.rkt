#lang racket/base

(provide
  set-checker-hash! get-checker-hash
  set-ast-checker-hash! get-ast-checker-hash
  set-check-lang! get-check-lang
  set-inst-checker-hash! get-inst-checker-hash)

(require
  (for-syntax racket/base racket/syntax syntax/parse))

(define-syntax (define-getter/setter stx)
  (syntax-parse stx
    [(_ name)
     #:with box-id (format-id stx "~a-box" #'name)
     #:with get-id (format-id stx "get-~a" #'name)
     #:with set-id (format-id stx "set-~a!" #'name)
     #'(begin
         (define box-id (box #f))
         (define (get-id) (unbox box-id))
         (define (set-id h) (set-box! box-id h)))]))

(define-getter/setter checker-hash)
(define-getter/setter ast-checker-hash)
(define-getter/setter inst-checker-hash)
(define-getter/setter check-lang)

