#lang forge/core

(require (prefix-in @ rackunit))

(sig Parent)
(sig A #:extends Parent #:one)
(sig B #:extends Parent #:one)

; Create a Run, but don't open a visualizer
(run myRun #:preds [])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(@check-equal?
 (length (forge:bound-upper (first (filter (lambda (x) (equal? A (forge:bound-relation x))) (forge:Run-kodkod-bounds myRun)))))
 1
 "#:one sigs should have exactly one element in their upper bound")
 
(@check-equal?
 (forge:bound-upper (first (filter (lambda (x) (equal? A (forge:bound-relation x))) (forge:Run-kodkod-bounds myRun))))
 (forge:bound-lower (first (filter (lambda (x) (equal? A (forge:bound-relation x))) (forge:Run-kodkod-bounds myRun))))
 "#:one sigs should be exact-bounded")

 
(@check-not-equal?
 (forge:bound-upper (first (filter (lambda (x) (equal? A (forge:bound-relation x))) (forge:Run-kodkod-bounds myRun))))
 (forge:bound-upper (first (filter (lambda (x) (equal? B (forge:bound-relation x))) (forge:Run-kodkod-bounds myRun))))
 "Upper bounds between #:one siblings should never overlap")

; Safety check: Regardless of what we think bounds do, confirm that overlap is impossible
(test oneSigsCannotOverlap #:preds [(some (& A B))] #:expect unsat)
