#lang forge/core

option run_sterling off
(require (prefix-in ru: rackunit))

(set-option! 'verbose 0)
;(set-option! 'problem_type 'temporal)

(define Constant (make-sig 'Constant))
(define Variable (make-sig 'Variable #:is-var #t))

(define var-opt
  (struct-copy forge:Options forge:DEFAULT-OPTIONS [problem_type 'temporal]))

(define myrun (make-run #:name 'myrun
                        #:preds (list (some Constant) (some Variable))
                        #:sigs (list Constant Variable)
                        #:options var-opt))

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
