#lang racket

(require "sigs.rkt")
(require (prefix-in @ racket))
; (set-verbosity 10)

; Example 1
; (sig Node)
; (sig Root #:one #:extends Node)

; (relation edges (Node Node))

; (pred acyclic (no (& iden (^ edges))))
; (pred root-connected (= Node (join Root (* edges))))

; (test rooted-dag (acyclic root-connected) ([Node 0 5]) 'sat)

; (pred root-not-root (some ([x Node]) (in (-> x Root) edges )))
; (test root-fail (acyclic root-connected root-not-root) 'unsat)


; Example 2
; (sig A)
; (sig A1 #:extends A)
; (sig A11 #:extends A1)
; (sig A12 #:extends A1)
; (sig A2 #:one #:extends A)
; (pred f false)

; (run test-run (f) ([A 10] [A1 5] [A11 2 2] [A12 2]))


; (sat . #hash((r0 . ((0) (1) (2) (3) (4) (5) (6) (7) (8) (9) (10) (11) (12) (13) (14) (15))) 
;              (r1 . ((0 1) (1 2) (2 3) (3 4) (4 5) (5 6) (6 7) (7 8) (8 9) (9 10) (10 11) (11 12) (12 13) (13 14) (14 15))) 
;              (r2 . ((16) (17) (18) (25))) 
;              (r3 . ((16) (17) (25))) 
;              (r4 . ((16) (17))) 
;              (r5 . ()) 
;              (r6 . ((18)))))

; (unsat false 
;        ((#(r5) < 2) || (#(r5) = 2)) 
;        ((#(r3) < 5) || (#(r3) = 5)) 
;        (r3 in r2) (r5 in r3))


; Example 3
; (@if #t (let () (sig A)) (let () (sig B)))
; (run my-run)

; Example 4
; (sig A)
; (sig A1 #:extends A)
; (sig A11 #:extends A1)
; (sig A12 #:extends A1)
; (sig A2 #:one #:extends A)

; (run test-run)
; (display test-run)
; (display 1)

; Example 5
(sig Node)
(relation edges (Node Node))
(pred reflexive (all ([n Node]) (in (-> n n) edges)))
(pred symmetric (= edges (~ edges)))
(pred transitive (= (edges edges) edges))

(run nothing)
; (display nothing)

(run just-preds (reflexive (= edges (~ edges)) transitive))
; (display just-preds)

(run just-bounds ([Node 1 3]))
; (display just-bounds)

(run pred-bound (reflexive (= edges (~ edges)) transitive) ([Node 1 3]))
; (display pred-bound)

(run bound-pred ([Node 1 3]) (reflexive (= edges (~ edges)) transitive))
; (display bound-pred)

(test my-test (reflexive (= edges (~ edges)) transitive) ([Node 1 3]) 'sat)
(check my-check ((= Node Node)) ([Node 1 3]))
(check my-fail (reflexive))

; Bug: A not defined.
; (run my-run ([A 1 2]))
; (display my-run)



