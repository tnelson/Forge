#lang racket

(require "sigs.rkt")
(require (prefix-in @ racket))
(set-verbosity 10)

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
; (sig Node)
; (relation edges (Node Node))
; (pred reflexive (all ([n Node]) (in (-> n n) edges)))
; (pred symmetric (= edges (~ edges)))
; (pred transitive (= (edges edges) edges))

; (run nothing)
; (display nothing)

; (run just-preds #:preds [reflexive (= edges (~ edges)) transitive])
; (display just-preds)

; (run just-bounds #:bounds ([Node 1 3]))
; (display just-bounds)

; (run pred-bound #:preds [reflexive (= edges (~ edges)) transitive] #:bounds ([Node 1 3]))
; (display pred-bound)

; (run bound-pred #:bounds ([Node 1 3]) #:preds [reflexive (= edges (~ edges)) transitive])
; (display bound-pred)

;(test my-test #:preds [reflexive (= edges (~ edges)) transitive] #:bounds ([Node 1 3]) 'sat)
;(check my-check #:preds [(= Node Node)] #:bounds ([Node 1 3]))

; (instance my-inst
;     (= Node (+ Node0 Node1 Node2))
;     (= edges (+ (-> Node0 Node0) (-> Node1 Node1) (-> Node2 Node2))))

; (check inst-check #:preds [reflexive] #:inst my-inst)

; Example 6
; (sig A)
; (sig B)
; (relation R1 (A B))
; (relation R2 (A B))
; (instance my-inst
;     (= A (+ A1 A2 A3))
;     (= B (+ B1 B2))
;     (= R1 (-> (+ A1 A2) (+ B1 B2)))
;     (in R2 (+ R1 (-> A3 B1))))
; (display my-inst)

; my-inst


; Example 7

(sig Thing)
(sig SpecialThing #:one #:extends Thing)
(sig UnspecialThing #:extends Thing)

(sig Stuff)
(sig Stuff1 #:extends Stuff)
(sig Stuff11 #:extends Stuff1)
(sig Stuff12 #:extends Stuff1)
(sig Stuff2 #:extends Stuff)

(instance just-sigs
    (= UnspecialThing (+ UT1 UT2 UT3)) ; r9 : 27 28 29
    (= SpecialThing ST) ; r8 : 26
    (= Thing (+ UnspecialThing SpecialThing OT1 OT2)) ; r7 : 26 27 28 29 30 31

    (= Stuff11 (+ S11a S11b)) ; r4 : 16 17
    (= Stuff12 (+ S12a S12b)) ; r5 : 18 19
    (= Stuff1 (+ Stuff11 Stuff12 S1a S1b)) ; r3 : 16 17 18 19 20 21
    (= Stuff2 (+ S2a S2b)) ; r6 : 22 23
    (= Stuff (+ Stuff1 Stuff2 Sa Sb))) ; r2 : 16 17 18 19 20 21 22 23 24 25

; (run my-run #:inst just-sigs)


(relation R (Thing Stuff))

(instance and-rels
    (= UnspecialThing (+ UT1 UT2 UT3)) ; r9 : 27 28 29
    (= SpecialThing ST) ; r8 : 26
    (= Thing (+ UnspecialThing SpecialThing OT1 OT2)) ; r7 : 26 27 28 29 30 31

    (= Stuff11 (+ S11a S11b)) ; r4 : 16 17
    (= Stuff12 (+ S12a S12b)) ; r5 : 18 19
    (= Stuff1 (+ Stuff11 Stuff12 S1a S1b)) ; r3 : 16 17 18 19 20 21
    (= Stuff2 (+ S2a S2b)) ; r6 : 22 23
    (= Stuff (+ Stuff1 Stuff2 Sa Sb)) ; r2 : 16 17 18 19 20 21 22 23 24 25

    (= R (-> UnspecialThing Stuff1)))

(run my-run #:inst and-rels)




; Bug: A not defined.
; (run my-run ([A 1 2]))
; (display my-run)



