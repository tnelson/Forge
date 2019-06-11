#lang rosette

(provide all-defined-out)

(define-syntax (declare-const stx)
  (syntax-case stx ()
    [(_ id type) #'(define-symbolic id type)]))

(define-syntax (declare-fun stx)
  (syntax-case stx ()
    [(_ id (domain ...) range)
     #'(define-symbolic id (~> domain ... range))]))

(define-syntax (define-const stx)
  (syntax-case stx ()
    [(_ id type body) #'(define id body)]))

(define-syntax (define-fun stx)
  (syntax-case stx ()
    [(_ id ([arg type] ...) range body)
     #'(define (id arg ...) body)]))

(define-syntax (forall2 stx)
  (syntax-case stx ()
    [(_ ([arg type] ...) body)
    #'(forall (map (lambda (x t) (begin (define-symbolic x t) x)) '(arg ...) '(type ...)) body)]))

(define-syntax (exists stx)
  (syntax-case stx ()
    [(_ ([arg type] ...) body)
     #'(exists (map (lambda (x t) (begin (define-symbolic x t) x))) body)]))

(define (mod a b) (modulo a b))

(define (= a b) (equal? a b))