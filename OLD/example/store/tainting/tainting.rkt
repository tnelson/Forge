#lang racket

; (require (prefix-in @ "some-defs.rkt"))
(require (for-syntax syntax/parse))

(define-for-syntax (my-expand expr)
  (when (syntax-tainted? expr)
    (printf "Tainted syntax: ~a~n" expr))
  (define expanded (local-expand expr (list) #f))
  (datum->syntax expr (if (list? (syntax-e expanded))
                          (map my-expand (syntax-e expanded))
                          expanded)))

(define-syntax (my-define-syntax-rule stx)
  (syntax-case stx ()
    [(_ pattern result)
     (with-syntax ([pattern-name (car (syntax-e #'pattern))])
     #'(define-syntax (pattern-name stx)
         (syntax-case stx ()
           [pattern (syntax/loc stx result)])))]))


(define-syntax (Expr stx)
  (syntax-parse stx #:datum-literals (+)
    [(Expr a + b)
     #'(@+ a b)]))

(Expr 1 + 2)