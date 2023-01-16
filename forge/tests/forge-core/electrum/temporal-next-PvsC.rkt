#lang forge/core

(require (prefix-in ru: rackunit) 
         (only-in racket second third))

(set-option! 'verbose 0)
(set-option! 'problem_type 'temporal)

(sig A #:one)
(sig B)
(relation r1 (A B) #:is-var "var")
(relation r2 (A B))

(run myrun #:preds [(some r1) (some r2)])

(ru:check-equal? (forge:is-sat? myrun) #t)
(define results (forge:Run-result myrun))
(define base (tree:get-value results))

; Buyer beware: Undefined behavior if trying to explore multi branches
(define newC (tree:get-value (tree:get-child results 'C)))
(define newP (tree:get-value (tree:get-child (tree:get-child results 'C) 'P)))

(ru:check-equal? (forge:Sat? newC) #t)
(ru:check-equal? (forge:Sat? newP) #t)

; A diff has three components; the second and third give additions and subtractions for each relation.
(define (no-change idiffs)
  (andmap (lambda (d)
            (and (equal? 'same-signature (first d))
                 (andmap (lambda (rl) (empty? (second rl))) (second d))
                 (andmap (lambda (rl) (empty? (second rl))) (third d)))) idiffs))

; Not strictly the place for this unit test, but include here since this is an undocumented function
(ru:check-true (no-change (solution-diff newP newP)))

; All different instances, not repeating
(ru:check-false (no-change (solution-diff base newC)))
(ru:check-false (no-change (solution-diff base newP)))
(ru:check-false (no-change (solution-diff newP newC)))
