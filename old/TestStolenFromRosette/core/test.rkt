#lang racket

(require racket/syntax (for-syntax racket racket/syntax) racket/generic
         "type.rkt" "reporter.rkt")


(struct term 
  (val                 ; (or/c any/c (cons/c function? (non-empty-listof any/c)))
   type                ; type?  
   ord)                ; integer?  
  #:methods gen:typed 
  [(define (get-type v) (term-type v))]
  #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "~a" (term->string self)))])

(struct constant term ())

(require "state.rkt")

(define-syntax-rule (printf-smt arg ...)
  (printf arg ...))

(define-syntax (declare-const stx)
  (syntax-case stx ()
    [(_ id type)
    (identifier? #'id)
    (syntax/loc stx (define var (constant #'id type)))]))

