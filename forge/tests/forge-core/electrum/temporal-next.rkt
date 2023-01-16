#lang forge/core

option run_sterling off


(require (prefix-in ru: rackunit))

(set-option! 'verbose 0)
(set-option! 'problem_type 'temporal)

(sig Constant)
(sig Variable #:is-var "var")

(run myrun #:preds [(some Constant) (some Variable)])

(ru:check-equal? (forge:is-sat? myrun) #t)
(define results (forge:Run-result myrun))
(define base (tree:get-value results))

; Buyer beware: Undefined behavior if trying to explore multi branches
(define newC (tree:get-value (tree:get-child results 'C)))
(define newP (tree:get-value (tree:get-child (tree:get-child results 'C) 'P)))

(ru:check-equal? (forge:Sat? newC) #t)
(ru:check-equal? (forge:Sat? newP) #t)

(define sameness '((same-signature ((Variable ()) (Constant ())) ((Variable ()) (Constant ())))))
; Not strictly the place for this unit test, but include here since this is an undocumented function
(ru:check-equal? (solution-diff newP newP) sameness)

; All different instances, not repeating
(ru:check-not-equal? (solution-diff base newC) sameness)
(ru:check-not-equal? (solution-diff base newP) sameness)
(ru:check-not-equal? (solution-diff newP newC) sameness)
