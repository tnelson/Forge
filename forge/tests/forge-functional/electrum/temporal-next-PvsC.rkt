#lang forge/core

(require (prefix-in ru: rackunit))

(set-option! 'verbose 0)
;(set-option! 'problem_type 'temporal)

(define A (make-sig 'A #:one #t))
(define B (make-sig 'B))
(define r1 (make-relation 'r1 (list A B) #:is-var #t))
(define r2 (make-relation 'r2 (list A B)))

(define myrun-options
  (struct-copy forge:Options forge:DEFAULT-OPTIONS [problem_type 'temporal]))

(define myrun (make-run #:name 'myrun
                        #:preds (list (some r1) (some r2))
                        #:sigs (list A B)
                        #:relations (list r1 r2)
                        #:options myrun-options))

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
