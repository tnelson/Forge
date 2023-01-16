#lang forge/core

option run_sterling off


(require (prefix-in ru: rackunit))

(set-option! 'verbose 0)

(sig A)

(run myrun #:preds [(some A)])

(ru:check-equal? (forge:is-sat? myrun) #t)
(define results (forge:Run-result myrun))
(define sol0 (tree:get-value results))

; Buyer beware: Undefined behavior if trying to explore multi branches
(define sol1 (tree:get-value (tree:get-child results 'next)))
(define sol2 (tree:get-value (tree:get-child (tree:get-child results 'next) 'next)))

(ru:check-equal? (forge:Sat? sol1) #t)
(ru:check-equal? (forge:Sat? sol2) #t)

(define sameness '((same-signature ((A ())) ((A ())))))
; Not strictly the place for this unit test, but include here since this is an undocumented function
(ru:check-equal? (solution-diff sol2 sol2) sameness)

; All different instances, not repeating
(ru:check-not-equal? (solution-diff sol0 sol1) sameness)
(ru:check-not-equal? (solution-diff sol0 sol2) sameness)
(ru:check-not-equal? (solution-diff sol1 sol2) sameness)
