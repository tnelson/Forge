#lang forge/core

;; Bounds Partition Inspection Tests
;; Inspect kodkod bounds to verify that child upper bounds are disjoint
;; subsets of parent upper bounds when partition is feasible, and that
;; partition is correctly skipped when infeasible.

(require (prefix-in @ rackunit))
(set-option! 'verbose 0)

;; Helper: get the bound struct for a given sig/relation from a run
(define (get-bound run-obj rel)
  (first (filter (lambda (x) (equal? rel (forge:bound-relation x)))
                 (forge:Run-kodkod-bounds run-obj))))

;; Helper: extract upper-bound atoms (flatten arity-1 tuples to atoms)
(define (upper-atoms run-obj sig)
  (map car (forge:bound-upper (get-bound run-obj sig))))

;; Helper: extract lower-bound atoms
(define (lower-atoms run-obj sig)
  (map car (forge:bound-lower (get-bound run-obj sig))))

;; Helper: check that list A is a subset of list B (by member)
(define (subset-of? a b)
  (andmap (lambda (x) (and (member x b) #t)) a))

;; Helper: check that two lists share no elements
(define (disjoint? a b)
  (not (ormap (lambda (x) (member x b)) a)))

;; ================================================================
;; Test 1: Perfect partition (abstract parent, children sum = parent)
;; 6 Parent, 3 A, 3 B → partition should give A exactly 3, B exactly 3
;; ================================================================
(sig T1P #:abstract)
(sig T1A #:extends T1P)
(sig T1B #:extends T1P)

(run t1-run #:preds []
     #:scope ([T1P 6] [T1A 3] [T1B 3]))

(define t1p-upper (upper-atoms t1-run T1P))
(define t1a-upper (upper-atoms t1-run T1A))
(define t1b-upper (upper-atoms t1-run T1B))
(define t1a-lower (lower-atoms t1-run T1A))
(define t1b-lower (lower-atoms t1-run T1B))

(@check-equal? (length t1p-upper) 6 "T1: Parent upper should have 6 atoms")
(@check-equal? (length t1a-upper) 3 "T1: A upper should have exactly 3 atoms (partitioned)")
(@check-equal? (length t1b-upper) 3 "T1: B upper should have exactly 3 atoms (partitioned)")
(@check-true (subset-of? t1a-upper t1p-upper) "T1: A upper ⊆ Parent upper")
(@check-true (subset-of? t1b-upper t1p-upper) "T1: B upper ⊆ Parent upper")
(@check-true (disjoint? t1a-upper t1b-upper) "T1: A upper and B upper are disjoint")
(@check-equal? t1a-lower '() "T1: A lower should be empty (non-exact)")
(@check-equal? t1b-lower '() "T1: B lower should be empty (non-exact)")

(forge:close-run t1-run)

;; ================================================================
;; Test 2: Under-partition (non-abstract parent, slack atoms)
;; 6 Parent, 2 A, 2 B → A gets 2, B gets 2, 2 slack in parent only
;; ================================================================
(sig T2P)
(sig T2A #:extends T2P)
(sig T2B #:extends T2P)

(run t2-run #:preds []
     #:scope ([T2P 6] [T2A 2] [T2B 2]))

(define t2p-upper (upper-atoms t2-run T2P))
(define t2a-upper (upper-atoms t2-run T2A))
(define t2b-upper (upper-atoms t2-run T2B))

(@check-equal? (length t2p-upper) 6 "T2: Parent upper should have 6 atoms")
(@check-equal? (length t2a-upper) 2 "T2: A upper should have 2 atoms (partitioned)")
(@check-equal? (length t2b-upper) 2 "T2: B upper should have 2 atoms (partitioned)")
(@check-true (subset-of? t2a-upper t2p-upper) "T2: A upper ⊆ Parent upper")
(@check-true (subset-of? t2b-upper t2p-upper) "T2: B upper ⊆ Parent upper")
(@check-true (disjoint? t2a-upper t2b-upper) "T2: A and B uppers are disjoint")

(forge:close-run t2-run)

;; ================================================================
;; Test 3: Over-committed — partition skipped
;; 5 Parent, 3 A, 3 B → children need 6 atoms but only 5 exist.
;; Partition is infeasible; both children receive all parent atoms.
;; ================================================================
(sig T3P #:abstract)
(sig T3A #:extends T3P)
(sig T3B #:extends T3P)

(run t3-run #:preds []
     #:scope ([T3P 5] [T3A 3] [T3B 3]))

(define t3p-upper (upper-atoms t3-run T3P))
(define t3a-upper (upper-atoms t3-run T3A))
(define t3b-upper (upper-atoms t3-run T3B))

(@check-equal? (length t3p-upper) 5 "T3: Parent upper should have 5 atoms")
;; Partition skipped: both children get all parent atoms
(@check-equal? (sort t3a-upper symbol<?) (sort t3b-upper symbol<?)
               "T3: A and B should have identical uppers (no partition)")
(@check-equal? (sort t3a-upper symbol<?) (sort t3p-upper symbol<?)
               "T3: children upper = parent upper (no partition)")

(forge:close-run t3-run)

;; ================================================================
;; Test 4: one sig + normal sibling
;; 4 Parent, one A, 3 B → A claims 1 lower-bound atom; B gets 3 surplus atoms.
;; ================================================================
(sig T4P #:abstract)
(sig T4One #:one #:extends T4P)
(sig T4Norm #:extends T4P)

(run t4-run #:preds []
     #:scope ([T4P 4] [T4Norm 3]))

(define t4p-upper (upper-atoms t4-run T4P))
(define t4one-upper (upper-atoms t4-run T4One))
(define t4one-lower (lower-atoms t4-run T4One))
(define t4norm-upper (upper-atoms t4-run T4Norm))

(@check-equal? (length t4p-upper) 4 "T4: Parent upper should have 4 atoms")
(@check-equal? (length t4one-upper) 1 "T4: one sig upper should have 1 atom")
(@check-equal? (length t4one-lower) 1 "T4: one sig lower should have 1 atom")
(@check-equal? t4one-upper t4one-lower "T4: one sig should be exact-bounded")
(@check-equal? (length t4norm-upper) 3 "T4: Normal upper should have 3 atoms (partitioned)")
(@check-true (disjoint? t4one-upper t4norm-upper)
             "T4: one sig and normal sibling should have disjoint uppers")

(forge:close-run t4-run)

;; ================================================================
;; Test 5: 3-level hierarchy
;; 6 Root, 4 Mid, 2 LeafA, 2 LeafB, 2 Other
;; Partition at root level (Mid+Other) and at mid level (LeafA+LeafB)
;; ================================================================
(sig T5Root #:abstract)
(sig T5Mid #:abstract #:extends T5Root)
(sig T5LeafA #:extends T5Mid)
(sig T5LeafB #:extends T5Mid)
(sig T5Other #:extends T5Root)

(run t5-run #:preds []
     #:scope ([T5Root 6] [T5Mid 4] [T5LeafA 2] [T5LeafB 2] [T5Other 2]))

(define t5root-upper (upper-atoms t5-run T5Root))
(define t5mid-upper (upper-atoms t5-run T5Mid))
(define t5la-upper (upper-atoms t5-run T5LeafA))
(define t5lb-upper (upper-atoms t5-run T5LeafB))
(define t5other-upper (upper-atoms t5-run T5Other))

(@check-equal? (length t5root-upper) 6 "T5: Root upper = 6")
(@check-equal? (length t5mid-upper) 4 "T5: Mid upper = 4 (partitioned from root)")
(@check-equal? (length t5other-upper) 2 "T5: Other upper = 2 (partitioned from root)")
(@check-equal? (length t5la-upper) 2 "T5: LeafA upper = 2 (partitioned from mid)")
(@check-equal? (length t5lb-upper) 2 "T5: LeafB upper = 2 (partitioned from mid)")

;; Containment
(@check-true (subset-of? t5mid-upper t5root-upper) "T5: Mid ⊆ Root")
(@check-true (subset-of? t5other-upper t5root-upper) "T5: Other ⊆ Root")
(@check-true (subset-of? t5la-upper t5mid-upper) "T5: LeafA ⊆ Mid")
(@check-true (subset-of? t5lb-upper t5mid-upper) "T5: LeafB ⊆ Mid")

;; Disjointness at each level
(@check-true (disjoint? t5mid-upper t5other-upper) "T5: Mid and Other are disjoint")
(@check-true (disjoint? t5la-upper t5lb-upper) "T5: LeafA and LeafB are disjoint")
(@check-true (disjoint? t5la-upper t5other-upper) "T5: LeafA and Other are disjoint")
(@check-true (disjoint? t5lb-upper t5other-upper) "T5: LeafB and Other are disjoint")

(forge:close-run t5-run)

;; ================================================================
;; Test 6: Partition skipped at parent propagates to grandchildren
;; 6 A, 5 B, one C, 3 D, 2 E
;; At A level: B+E need 6 atoms but only 5 surplus → partition skipped.
;; At B level: D needs only 3 atoms, which fits numerically.
;;   BUT atoms passed to B now include C's lower-bound atom (mixed in from
;;   the skipped partition), so partitioning B's children is unsafe.
;;   D must still receive enough atoms to reach its scope of 3.
;; ================================================================
(sig T6A #:abstract)
(sig T6B #:abstract #:extends T6A)
(sig T6C #:one #:extends T6B)
(sig T6D #:extends T6B)
(sig T6E #:extends T6A)

(run t6-run #:preds []
     #:scope ([T6A 6] [T6B 5] [T6D 3] [T6E 2]))

(define t6a-upper (upper-atoms t6-run T6A))
(define t6b-upper (upper-atoms t6-run T6B))
(define t6c-upper (upper-atoms t6-run T6C))
(define t6d-upper (upper-atoms t6-run T6D))
(define t6e-upper (upper-atoms t6-run T6E))
(define t6c-lower (lower-atoms t6-run T6C))

(@check-equal? (length t6a-upper) 6 "T6: A upper = 6")
;; Partition skipped at A: B and E both receive all of A's atoms
(@check-equal? (sort t6b-upper symbol<?) (sort t6a-upper symbol<?)
               "T6: B upper = A upper (no partition at A)")
(@check-equal? (sort t6e-upper symbol<?) (sort t6a-upper symbol<?)
               "T6: E upper = A upper (no partition at A)")
;; D must still receive enough atoms to reach its scope of 3
(@check-true (>= (length t6d-upper) 3)
             "T6: D upper ≥ 3 (must not be limited by cascading skip)")
;; C is one-sig: exact bounded
(@check-equal? t6c-upper t6c-lower "T6: C is exact-bounded (one sig)")

(forge:close-run t6-run)

;; ================================================================
;; Test 7: Inst relation bound references parent-named surplus atoms
;; Binding t7link to (T7P0 → T7P1) uses atoms from the parent's surplus.
;; Partition is skipped to avoid assigning those atoms to the wrong child.
;; Both children receive all parent atoms.
;; ================================================================
(sig T7P #:abstract)
(sig T7A #:extends T7P)
(sig T7B #:extends T7P)
(relation t7link (T7A T7P))

;; Bind t7link with parent-named atoms T7P0→T7P1
(inst t7-inst
    (= t7link (-> (atom 'T7P0) (atom 'T7P1))))

(run t7-run #:preds []
     #:scope ([T7P 6] [T7A 3] [T7B 3])
     #:bounds [t7-inst])

(define t7p-upper (upper-atoms t7-run T7P))
(define t7a-upper (upper-atoms t7-run T7A))
(define t7b-upper (upper-atoms t7-run T7B))

(@check-equal? (length t7p-upper) 6 "T7: Parent upper = 6")
;; Inst references surplus atoms T7P0,T7P1 → partition skipped
(@check-equal? (sort t7a-upper symbol<?) (sort t7b-upper symbol<?)
               "T7: A and B identical (no partition, inst uses surplus atoms)")
(@check-equal? (sort t7a-upper symbol<?) (sort t7p-upper symbol<?)
               "T7: children upper = parent upper (no partition)")

(forge:close-run t7-run)

;; ================================================================
;; Test 8: Piecewise bound references parent-named surplus atoms
;; Piecewise bounds (atom.rel = expr) are stored separately from complete
;; relation bounds. Both must be checked for surplus-atom references.
;; ================================================================
(sig T8P #:abstract)
(sig T8A #:extends T8P)
(sig T8B #:extends T8P)
(relation t8link (T8A T8P))

;; Piecewise bind: T8P0.t8link = T8P1 (both are parent-named surplus atoms)
(inst t8-inst
    (= (join (atom 'T8P0) t8link) (atom 'T8P1)))

(run t8-run #:preds []
     #:scope ([T8P 6] [T8A 3] [T8B 3])
     #:bounds [t8-inst])

(define t8p-upper (upper-atoms t8-run T8P))
(define t8a-upper (upper-atoms t8-run T8A))
(define t8b-upper (upper-atoms t8-run T8B))

(@check-equal? (length t8p-upper) 6 "T8: Parent upper = 6")
;; Piecewise atoms T8P0, T8P1 are surplus atoms → partition skipped
(@check-equal? (sort t8a-upper symbol<?) (sort t8b-upper symbol<?)
               "T8: A and B identical (no partition, piecewise uses surplus atoms)")
(@check-equal? (sort t8a-upper symbol<?) (sort t8p-upper symbol<?)
               "T8: children upper = parent upper (no partition)")

(forge:close-run t8-run)

;; ================================================================
;; Test 9: Inst relation bound references only lower-bound atoms → partition fires
;; T9One0 is in lower bounds (from `one sig`), not among the surplus atoms.
;; Referencing it in an inst should NOT prevent partition.
;; ================================================================
(sig T9P #:abstract)
(sig T9One #:one #:extends T9P)
(sig T9Norm #:extends T9P)
(relation t9link (T9One T9P))

;; Bind t9link using only the one-sig lower-bound atom
(inst t9-inst
    (= t9link (-> (atom 'T9One0) (atom 'T9One0))))

(run t9-run #:preds []
     #:scope ([T9P 4] [T9Norm 3])
     #:bounds [t9-inst])

(define t9p-upper (upper-atoms t9-run T9P))
(define t9one-upper (upper-atoms t9-run T9One))
(define t9one-lower (lower-atoms t9-run T9One))
(define t9norm-upper (upper-atoms t9-run T9Norm))

(@check-equal? (length t9p-upper) 4 "T9: Parent upper = 4")
(@check-equal? (length t9one-upper) 1 "T9: one sig upper = 1")
(@check-equal? t9one-upper t9one-lower "T9: one sig is exact-bounded")
;; Inst atoms are all lower-bound atoms, so partition still fires
(@check-equal? (length t9norm-upper) 3
               "T9: Norm upper = 3 (partitioned despite inst)")
(@check-true (disjoint? t9one-upper t9norm-upper)
             "T9: one sig and Norm should have disjoint uppers")
(@check-true (subset-of? t9norm-upper t9p-upper) "T9: Norm ⊆ Parent")

(forge:close-run t9-run)

;; ================================================================
;; Test 10: Multiple piecewise bindings on different child-sig fields
;; Two fields on different children, each piecewise-bound with parent-named atoms.
;; The referenced atoms are surplus atoms → partition skipped.
;; ================================================================
(sig T10P #:abstract)
(sig T10A #:extends T10P)
(sig T10B #:extends T10P)
(relation t10la (T10A T10P))
(relation t10lb (T10B T10P))

;; Piecewise binds on two different child-sig fields, both referencing surplus atoms
(inst t10-inst
    (= (join (atom 'T10P0) t10la) (atom 'T10P1))
    (= (join (atom 'T10P2) t10lb) (atom 'T10P3)))

(run t10-run #:preds []
     #:scope ([T10P 6] [T10A 3] [T10B 3])
     #:bounds [t10-inst])

(define t10p-upper (upper-atoms t10-run T10P))
(define t10a-upper (upper-atoms t10-run T10A))
(define t10b-upper (upper-atoms t10-run T10B))

(@check-equal? (length t10p-upper) 6 "T10: Parent upper = 6")
;; Piecewise atoms T10P0-T10P3 are all surplus atoms → partition skipped
(@check-equal? (sort t10a-upper symbol<?) (sort t10b-upper symbol<?)
               "T10: A and B identical (no partition, piecewise uses surplus atoms)")
(@check-equal? (sort t10a-upper symbol<?) (sort t10p-upper symbol<?)
               "T10: children upper = parent upper (no partition)")

(forge:close-run t10-run)

;; ================================================================
;; Test 11: Parent has NO explicit scope (defaults to 4), children have explicit scopes
;; This tests the common case where a user declares child scopes but forgets the parent.
;; Parent defaults to 4. Children 2+2=4, exact fit → partition should fire.
;; ================================================================
(sig T11P #:abstract)
(sig T11A #:extends T11P)
(sig T11B #:extends T11P)

(run t11-run #:preds []
     #:scope ([T11A 2] [T11B 2]))  ; NOTE: no T11P in scope

(define t11p-upper (upper-atoms t11-run T11P))
(define t11a-upper (upper-atoms t11-run T11A))
(define t11b-upper (upper-atoms t11-run T11B))

(@check-equal? (length t11p-upper) 4 "T11: Parent upper = 4 (implicit default)")
(@check-equal? (length t11a-upper) 2 "T11: A upper = 2 (partitioned)")
(@check-equal? (length t11b-upper) 2 "T11: B upper = 2 (partitioned)")
(@check-true (subset-of? t11a-upper t11p-upper) "T11: A ⊆ Parent")
(@check-true (subset-of? t11b-upper t11p-upper) "T11: B ⊆ Parent")
(@check-true (disjoint? t11a-upper t11b-upper) "T11: A and B are disjoint (partitioned)")

(forge:close-run t11-run)

;; ================================================================
;; Test 12: Parent explicit scope, ONE child has no explicit scope (defaults to 4)
;; Parent=6, ChildA=2, ChildB=no scope (defaults to 4). Total need=2+4=6 ≤ 6.
;; Partition should fire, giving ChildB 4 surplus atoms.
;; ================================================================
(sig T12P #:abstract)
(sig T12A #:extends T12P)
(sig T12B #:extends T12P)

(run t12-run #:preds []
     #:scope ([T12P 6] [T12A 2]))  ; NOTE: no T12B in scope → defaults to 4

(define t12p-upper (upper-atoms t12-run T12P))
(define t12a-upper (upper-atoms t12-run T12A))
(define t12b-upper (upper-atoms t12-run T12B))

(@check-equal? (length t12p-upper) 6 "T12: Parent upper = 6")
(@check-equal? (length t12a-upper) 2 "T12: A upper = 2 (explicit)")
(@check-equal? (length t12b-upper) 4 "T12: B upper = 4 (implicit default)")
(@check-true (subset-of? t12a-upper t12p-upper) "T12: A ⊆ Parent")
(@check-true (subset-of? t12b-upper t12p-upper) "T12: B ⊆ Parent")
(@check-true (disjoint? t12a-upper t12b-upper) "T12: A and B are disjoint (partitioned)")

(forge:close-run t12-run)

;; ================================================================
;; Test 13: Neither parent nor children have explicit scopes (all default to 4)
;; Two children each default to need=4, total=8 > parent's 4 surplus → partition skipped.
;; ================================================================
(sig T13P #:abstract)
(sig T13A #:extends T13P)
(sig T13B #:extends T13P)

(run t13-run #:preds []
     #:scope ())  ; NOTE: no scopes at all

(define t13p-upper (upper-atoms t13-run T13P))
(define t13a-upper (upper-atoms t13-run T13A))
(define t13b-upper (upper-atoms t13-run T13B))

(@check-equal? (length t13p-upper) 4 "T13: Parent upper = 4 (global default)")
;; Each child defaults to 4, total 8 > 4 surplus → partition skipped
(@check-equal? (sort t13a-upper symbol<?) (sort t13b-upper symbol<?)
               "T13: A and B identical (no partition, all defaults)")
(@check-equal? (sort t13a-upper symbol<?) (sort t13p-upper symbol<?)
               "T13: children upper = parent upper (no partition)")

(forge:close-run t13-run)

;; ================================================================
;; Test 14: Mixed exact and inexact children
;; Parent=6. ChildA=exactly 3 (lower=upper=3). ChildB=3 (inexact, lower=0).
;; ChildA generates 3 self-named lower-bound atoms (ChildA0-2).
;; ChildA need=0 (already satisfied by lower). ChildB need=3.
;; 3 parent-named surplus atoms exist. Partition: ChildA gets 0, ChildB gets 3.
;; ================================================================
(sig T14P #:abstract)
(sig T14A #:extends T14P)
(sig T14B #:extends T14P)

(run t14-run #:preds []
     #:scope ([T14P 6] [T14A 3 3] [T14B 3]))  ; T14A is exact (lower=upper=3)

(define t14p-upper (upper-atoms t14-run T14P))
(define t14a-upper (upper-atoms t14-run T14A))
(define t14a-lower (lower-atoms t14-run T14A))
(define t14b-upper (upper-atoms t14-run T14B))
(define t14b-lower (lower-atoms t14-run T14B))

(@check-equal? (length t14p-upper) 6 "T14: Parent upper = 6")
;; ChildA is exact: lower = upper = 3 self-named atoms
(@check-equal? (length t14a-lower) 3 "T14: A lower = 3 (exact)")
(@check-equal? (length t14a-upper) 3 "T14: A upper = 3 (exact)")
(@check-equal? t14a-upper t14a-lower "T14: A is exact-bounded")
;; ChildB is inexact: no lower-bound atoms, gets surplus atoms from parent
(@check-equal? t14b-lower '() "T14: B lower is empty (inexact)")
(@check-equal? (length t14b-upper) 3 "T14: B upper = 3 (surplus atoms from parent)")
;; Key: A's self-named atoms and B's parent-named atoms should be disjoint
(@check-true (disjoint? t14a-upper t14b-upper) "T14: Exact A and inexact B are disjoint")
(@check-true (subset-of? t14a-upper t14p-upper) "T14: A ⊆ Parent")
(@check-true (subset-of? t14b-upper t14p-upper) "T14: B ⊆ Parent")

(forge:close-run t14-run)

;; ================================================================
;; Test 15: Single child (no sibling to partition against)
;; abstract Parent=4, single ChildA=3.
;; Partition gives all surplus atoms to the single child.
;; ================================================================
(sig T15P #:abstract)
(sig T15A #:extends T15P)

(run t15-run #:preds []
     #:scope ([T15P 4] [T15A 3]))

(define t15p-upper (upper-atoms t15-run T15P))
(define t15a-upper (upper-atoms t15-run T15A))

(@check-equal? (length t15p-upper) 4 "T15: Parent upper = 4")
(@check-equal? (length t15a-upper) 3 "T15: Single child A upper = 3 (partitioned)")
(@check-true (subset-of? t15a-upper t15p-upper) "T15: A ⊆ Parent")

(forge:close-run t15-run)

;; ================================================================
;; Test 16: No surplus atoms (all parent atoms claimed by children's lower bounds)
;; Two one-sig children fill the parent completely; nothing left to partition.
;; ================================================================
(sig T16P #:abstract)
(sig T16A #:one #:extends T16P)
(sig T16B #:one #:extends T16P)

(run t16-run #:preds []
     #:scope ([T16P 2]))  ; 2 = one + one, no surplus

(define t16p-upper (upper-atoms t16-run T16P))
(define t16a-upper (upper-atoms t16-run T16A))
(define t16b-upper (upper-atoms t16-run T16B))
(define t16a-lower (lower-atoms t16-run T16A))
(define t16b-lower (lower-atoms t16-run T16B))

(@check-equal? (length t16p-upper) 2 "T16: Parent upper = 2")
(@check-equal? (length t16a-upper) 1 "T16: A upper = 1 (one sig)")
(@check-equal? (length t16b-upper) 1 "T16: B upper = 1 (one sig)")
(@check-equal? t16a-upper t16a-lower "T16: A is exact-bounded")
(@check-equal? t16b-upper t16b-lower "T16: B is exact-bounded")
(@check-true (disjoint? t16a-upper t16b-upper) "T16: A and B are disjoint")

(forge:close-run t16-run)

;; ================================================================
;; Test 17: Multi-level with missing intermediate scope
;; Root=6, Mid has NO scope (defaults to 4), LeafA=2, LeafB=2, Other=2.
;; At root level: Mid need=4, Other need=2, total=6 ≤ 6 → partition fires.
;; At mid level: LeafA need=2, LeafB need=2, total=4 ≤ 4 → partition fires.
;; ================================================================
(sig T17Root #:abstract)
(sig T17Mid #:abstract #:extends T17Root)
(sig T17LeafA #:extends T17Mid)
(sig T17LeafB #:extends T17Mid)
(sig T17Other #:extends T17Root)

(run t17-run #:preds []
     #:scope ([T17Root 6] [T17LeafA 2] [T17LeafB 2] [T17Other 2]))
     ;; NOTE: no T17Mid in scope → defaults to 4

(define t17root-upper (upper-atoms t17-run T17Root))
(define t17mid-upper (upper-atoms t17-run T17Mid))
(define t17la-upper (upper-atoms t17-run T17LeafA))
(define t17lb-upper (upper-atoms t17-run T17LeafB))
(define t17other-upper (upper-atoms t17-run T17Other))

(@check-equal? (length t17root-upper) 6 "T17: Root upper = 6")
(@check-equal? (length t17mid-upper) 4 "T17: Mid upper = 4 (implicit default)")
(@check-equal? (length t17other-upper) 2 "T17: Other upper = 2 (explicit)")

;; At mid level, partition should assign 2 to each leaf
(@check-equal? (length t17la-upper) 2 "T17: LeafA upper = 2")
(@check-equal? (length t17lb-upper) 2 "T17: LeafB upper = 2")

;; Containment
(@check-true (subset-of? t17mid-upper t17root-upper) "T17: Mid ⊆ Root")
(@check-true (subset-of? t17other-upper t17root-upper) "T17: Other ⊆ Root")
(@check-true (subset-of? t17la-upper t17mid-upper) "T17: LeafA ⊆ Mid")
(@check-true (subset-of? t17lb-upper t17mid-upper) "T17: LeafB ⊆ Mid")

;; Disjointness
(@check-true (disjoint? t17mid-upper t17other-upper) "T17: Mid and Other are disjoint")
(@check-true (disjoint? t17la-upper t17lb-upper) "T17: LeafA and LeafB are disjoint")

(forge:close-run t17-run)

;; ================================================================
;; Test 18: one sig + normal child, parent has NO explicit scope
;; Parent defaults to 4. one-sig takes 1 lower-bound atom.
;; 3 surplus parent atoms exist. Normal child defaults to 4 → need=4 > 3 → partition skipped.
;; ================================================================
(sig T18P #:abstract)
(sig T18One #:one #:extends T18P)
(sig T18Norm #:extends T18P)

(run t18-run #:preds []
     #:scope ([T18Norm 3]))  ; Parent has no scope → default 4

(define t18p-upper (upper-atoms t18-run T18P))
(define t18one-upper (upper-atoms t18-run T18One))
(define t18norm-upper (upper-atoms t18-run T18Norm))

(@check-equal? (length t18p-upper) 4 "T18: Parent upper = 4 (implicit default)")
(@check-equal? (length t18one-upper) 1 "T18: one sig upper = 1")
;; Normal child needs 3 atoms; 3 surplus exist (4 - 1 one-sig) → partition fires.
(@check-equal? (length t18norm-upper) 3 "T18: Normal upper = 3 (partitioned)")
(@check-true (disjoint? t18one-upper t18norm-upper) "T18: one and Normal are disjoint")
(@check-true (subset-of? t18one-upper t18p-upper) "T18: one ⊆ Parent")
(@check-true (subset-of? t18norm-upper t18p-upper) "T18: Normal ⊆ Parent")

(forge:close-run t18-run)

;; ================================================================
;; Test 19: lone sig without explicit scope in hierarchy
;; Parent=5, lone child (no explicit scope, defaults to 1 from scope-with-ones),
;; normal child scope=3.
;; lone need = max(0, 1-0) = 1. Normal need = 3. total = 4 ≤ 5 → partition.
;; ================================================================
(sig T19P #:abstract)
(sig T19Lone #:lone #:extends T19P)
(sig T19Norm #:extends T19P)

(run t19-run #:preds []
     #:scope ([T19P 5] [T19Norm 3]))  ; lone has no explicit scope → defaults to (0,1)

(define t19p-upper (upper-atoms t19-run T19P))
(define t19lone-upper (upper-atoms t19-run T19Lone))
(define t19norm-upper (upper-atoms t19-run T19Norm))

(@check-equal? (length t19p-upper) 5 "T19: Parent upper = 5")
(@check-equal? (length t19lone-upper) 1 "T19: lone sig upper = 1")
(@check-equal? (length t19norm-upper) 3 "T19: Normal upper = 3 (partitioned)")
(@check-true (disjoint? t19lone-upper t19norm-upper) "T19: lone and Normal are disjoint")
(@check-true (subset-of? t19lone-upper t19p-upper) "T19: lone ⊆ Parent")
(@check-true (subset-of? t19norm-upper t19p-upper) "T19: Normal ⊆ Parent")

(forge:close-run t19-run)

;; ================================================================
;; Test 20: Exact parent with inexact children
;; exactly 6 Parent, 3 ChildA, 3 ChildB (inexact children).
;; Exact parent has lower=upper=6, so there are no surplus atoms.
;; Children need > 0 but nothing to partition → both get all parent atoms.
;; ================================================================
(sig T20P #:abstract)
(sig T20A #:extends T20P)
(sig T20B #:extends T20P)

(run t20-run #:preds []
     #:scope ([T20P 6 6] [T20A 3] [T20B 3]))  ; T20P is exact (lower=upper=6)

(define t20p-upper (upper-atoms t20-run T20P))
(define t20a-upper (upper-atoms t20-run T20A))
(define t20b-upper (upper-atoms t20-run T20B))

(@check-equal? (length t20p-upper) 6 "T20: Exact parent upper = 6")
;; No surplus atoms → partition skipped; both children get all parent atoms.
(@check-equal? (length t20a-upper) 6 "T20: A upper = 6 (no partition, exact parent)")
(@check-equal? (length t20b-upper) 6 "T20: B upper = 6 (no partition, exact parent)")
(@check-equal? (sort t20a-upper symbol<?) (sort t20b-upper symbol<?)
               "T20: A and B identical (no partition)")
(@check-true (subset-of? t20a-upper t20p-upper) "T20: A ⊆ Parent")
(@check-true (subset-of? t20b-upper t20p-upper) "T20: B ⊆ Parent")

(forge:close-run t20-run)

;; ================================================================
;; Test 21: Partition at root, skipped at mid due to cascading defaults
;; Root=8, Mid/LeafA/LeafB default to 4, Other=2.
;; Root level: 4+2=6 ≤ 8 → partition. Mid level: 4+4=8 > 4 → skipped.
;; Mid and Other are disjoint; leaves share Mid's atoms.
;; ================================================================
(sig T21Root #:abstract)
(sig T21Mid #:abstract #:extends T21Root)
(sig T21LeafA #:extends T21Mid)
(sig T21LeafB #:extends T21Mid)
(sig T21Other #:extends T21Root)

(run t21-run #:preds []
     #:scope ([T21Root 8] [T21Other 2]))
     ;; Mid, LeafA, LeafB all default to 4

(define t21root-upper (upper-atoms t21-run T21Root))
(define t21mid-upper (upper-atoms t21-run T21Mid))
(define t21la-upper (upper-atoms t21-run T21LeafA))
(define t21lb-upper (upper-atoms t21-run T21LeafB))
(define t21other-upper (upper-atoms t21-run T21Other))

(@check-equal? (length t21root-upper) 8 "T21: Root upper = 8")
(@check-equal? (length t21mid-upper) 4 "T21: Mid upper = 4 (default, partitioned from root)")
(@check-equal? (length t21other-upper) 2 "T21: Other upper = 2 (partitioned from root)")
(@check-true (disjoint? t21mid-upper t21other-upper) "T21: Mid and Other disjoint (root partitioned)")
;; Mid level: leaves each default to 4, total=8 > 4 → partition skipped
(@check-equal? (sort t21la-upper symbol<?) (sort t21lb-upper symbol<?)
               "T21: LeafA and LeafB identical (no partition at mid)")
(@check-equal? (sort t21la-upper symbol<?) (sort t21mid-upper symbol<?)
               "T21: Leaves = all Mid atoms (no partition at mid)")
;; Containment
(@check-true (subset-of? t21mid-upper t21root-upper) "T21: Mid ⊆ Root")
(@check-true (subset-of? t21other-upper t21root-upper) "T21: Other ⊆ Root")
(@check-true (subset-of? t21la-upper t21mid-upper) "T21: LeafA ⊆ Mid")

(forge:close-run t21-run)

;; ================================================================
;; Test 22: Non-abstract parent with two one-sig children
;; Parent (plain, not abstract) scope=4. Two one-sig children.
;; Both one-sigs need 0 surplus atoms; 2 surplus atoms remain in parent only
;; (non-abstract parent can have atoms not belonging to any child).
;; ================================================================
(sig T22P)
(sig T22OneA #:one #:extends T22P)
(sig T22OneB #:one #:extends T22P)

(run t22-run #:preds []
     #:scope ([T22P 4]))

(define t22p-upper (upper-atoms t22-run T22P))
(define t22a-upper (upper-atoms t22-run T22OneA))
(define t22b-upper (upper-atoms t22-run T22OneB))
(define t22a-lower (lower-atoms t22-run T22OneA))
(define t22b-lower (lower-atoms t22-run T22OneB))

(@check-equal? (length t22p-upper) 4 "T22: Non-abstract parent upper = 4")
(@check-equal? (length t22a-upper) 1 "T22: OneA upper = 1")
(@check-equal? (length t22b-upper) 1 "T22: OneB upper = 1")
(@check-equal? t22a-upper t22a-lower "T22: OneA is exact-bounded")
(@check-equal? t22b-upper t22b-lower "T22: OneB is exact-bounded")
(@check-true (disjoint? t22a-upper t22b-upper) "T22: OneA and OneB disjoint")
(@check-true (subset-of? t22a-upper t22p-upper) "T22: OneA ⊆ Parent")
(@check-true (subset-of? t22b-upper t22p-upper) "T22: OneB ⊆ Parent")

(forge:close-run t22-run)

;; ================================================================
;; Test 23: Exact child under parent with NO explicit scope
;; Parent: abstract, no scope (defaults to 4).
;; ChildA: exactly 3 (generates 3 self-named lower-bound atoms).
;; ChildB: no scope (defaults to 4).
;; Parent budget = max(3,4) = 4. Only 1 surplus atom exists.
;; ChildB needs 4 but only 1 surplus → partition skipped.
;; ================================================================
(sig T23P #:abstract)
(sig T23A #:extends T23P)
(sig T23B #:extends T23P)

(run t23-run #:preds []
     #:scope ([T23A 3 3]))  ; T23A exact; T23P and T23B have no scope

(define t23p-upper (upper-atoms t23-run T23P))
(define t23a-upper (upper-atoms t23-run T23A))
(define t23a-lower (lower-atoms t23-run T23A))
(define t23b-upper (upper-atoms t23-run T23B))

(@check-equal? (length t23p-upper) 4 "T23: Parent upper = 4 (max of lower=3 and default=4)")
(@check-equal? (length t23a-lower) 3 "T23: A lower = 3 (exact, self-named)")
(@check-equal? t23a-upper t23a-lower "T23: A is exact-bounded")
;; ChildB needs 4 but only 1 surplus → partition skipped, B gets all parent atoms
(@check-equal? (length t23b-upper) 4
               "T23: B upper = 4 (no partition)")
(@check-true (subset-of? t23a-upper t23p-upper) "T23: A ⊆ Parent")
(@check-true (subset-of? t23b-upper t23p-upper) "T23: B ⊆ Parent")

(forge:close-run t23-run)

;; ================================================================
;; Test 24: Multiple exact children summing to parent scope (no surplus)
;; Parent=6. Three children each exactly 2.
;; Each generates 2 self-named atoms (6 total = parent scope). No surplus.
;; Partition trivially succeeds (all needs=0). Each child keeps its own atoms.
;; ================================================================
(sig T24P #:abstract)
(sig T24A #:extends T24P)
(sig T24B #:extends T24P)
(sig T24C #:extends T24P)

(run t24-run #:preds []
     #:scope ([T24P 6] [T24A 2 2] [T24B 2 2] [T24C 2 2]))

(define t24p-upper (upper-atoms t24-run T24P))
(define t24a-upper (upper-atoms t24-run T24A))
(define t24b-upper (upper-atoms t24-run T24B))
(define t24c-upper (upper-atoms t24-run T24C))

(@check-equal? (length t24p-upper) 6 "T24: Parent upper = 6")
(@check-equal? (length t24a-upper) 2 "T24: A upper = 2 (exact)")
(@check-equal? (length t24b-upper) 2 "T24: B upper = 2 (exact)")
(@check-equal? (length t24c-upper) 2 "T24: C upper = 2 (exact)")
;; All three children should have disjoint self-named atoms
(@check-true (disjoint? t24a-upper t24b-upper) "T24: A and B disjoint")
(@check-true (disjoint? t24a-upper t24c-upper) "T24: A and C disjoint")
(@check-true (disjoint? t24b-upper t24c-upper) "T24: B and C disjoint")

(forge:close-run t24-run)

;; ================================================================
;; Test 25: Three children, one explicit scope, two default → partition fires
;; Parent=10, ChildA=2 (explicit), ChildB=no scope (4), ChildC=no scope (4).
;; Total need = 2 + 4 + 4 = 10 ≤ 10 → partition fires.
;; Each child gets a disjoint subset of surplus atoms.
;; ================================================================
(sig T25P #:abstract)
(sig T25A #:extends T25P)
(sig T25B #:extends T25P)
(sig T25C #:extends T25P)

(run t25-run #:preds []
     #:scope ([T25P 10] [T25A 2]))  ; T25B and T25C default to 4

(define t25p-upper (upper-atoms t25-run T25P))
(define t25a-upper (upper-atoms t25-run T25A))
(define t25b-upper (upper-atoms t25-run T25B))
(define t25c-upper (upper-atoms t25-run T25C))

(@check-equal? (length t25p-upper) 10 "T25: Parent upper = 10")
(@check-equal? (length t25a-upper) 2 "T25: A upper = 2 (explicit)")
(@check-equal? (length t25b-upper) 4 "T25: B upper = 4 (default, partitioned)")
(@check-equal? (length t25c-upper) 4 "T25: C upper = 4 (default, partitioned)")
(@check-true (disjoint? t25a-upper t25b-upper) "T25: A and B disjoint")
(@check-true (disjoint? t25a-upper t25c-upper) "T25: A and C disjoint")
(@check-true (disjoint? t25b-upper t25c-upper) "T25: B and C disjoint")
(@check-true (subset-of? t25a-upper t25p-upper) "T25: A ⊆ Parent")
(@check-true (subset-of? t25b-upper t25p-upper) "T25: B ⊆ Parent")
(@check-true (subset-of? t25c-upper t25p-upper) "T25: C ⊆ Parent")

(forge:close-run t25-run)

;; ================================================================
;; Test 26: one sig as parent with child
;; Unusual but syntactically valid. Parent has 1 atom, no surplus.
;; Child defaults to 4 but only 1 atom exists → gets parent's single atom.
;; ================================================================
(sig T26OneP #:one)
(sig T26Child #:extends T26OneP)

(run t26-run #:preds []
     #:scope ())  ; no explicit scopes

(define t26p-upper (upper-atoms t26-run T26OneP))
(define t26c-upper (upper-atoms t26-run T26Child))

(@check-equal? (length t26p-upper) 1 "T26: one parent upper = 1")
(@check-equal? (length t26c-upper) 1 "T26: Child upper = 1 (gets parent's single atom)")
(@check-equal? (sort t26c-upper symbol<?) (sort t26p-upper symbol<?)
               "T26: Child upper = parent upper")

(forge:close-run t26-run)

;; ================================================================
;; Test 27: lone sig as parent with child
;; lone parent has scope (0,1), so upper=1.
;; Child defaults to 4 but only 1 atom exists → gets parent's single atom.
;; ================================================================
(sig T27LoneP #:lone)
(sig T27Child #:extends T27LoneP)

(run t27-run #:preds []
     #:scope ())

(define t27p-upper (upper-atoms t27-run T27LoneP))
(define t27c-upper (upper-atoms t27-run T27Child))

(@check-equal? (length t27p-upper) 1 "T27: lone parent upper = 1")
(@check-equal? (length t27c-upper) 1 "T27: Child upper = 1 (gets lone parent's single atom)")
(@check-equal? (sort t27c-upper symbol<?) (sort t27p-upper symbol<?)
               "T27: Child upper = parent upper")

(forge:close-run t27-run)

;; ================================================================
;; Test 28: Multi-level cascade — skipping propagates down
;; L1=6 (abstract). L2A=4 (abstract), L2B=4: need 8 > 6 → partition skipped at L1.
;; L2A has children L3A=2, L3B=2: need 4 fits in 6 numerically.
;; BUT because partition was skipped at L1, the atoms passed down include
;; lower-bound atoms from siblings, so partition is also unsafe at L2A.
;; L3A and L3B should get IDENTICAL uppers (all L1 atoms).
;; ================================================================
(sig T28L1 #:abstract)
(sig T28L2A #:abstract #:extends T28L1)
(sig T28L3A #:extends T28L2A)
(sig T28L3B #:extends T28L2A)
(sig T28L2B #:extends T28L1)

(run t28-run #:preds []
     #:scope ([T28L1 6] [T28L2A 4] [T28L3A 2] [T28L3B 2] [T28L2B 4]))

(define t28l1-upper (upper-atoms t28-run T28L1))
(define t28l2a-upper (upper-atoms t28-run T28L2A))
(define t28l2b-upper (upper-atoms t28-run T28L2B))
(define t28l3a-upper (upper-atoms t28-run T28L3A))
(define t28l3b-upper (upper-atoms t28-run T28L3B))

(@check-equal? (length t28l1-upper) 6 "T28: L1 upper = 6")
;; L1: partition skipped → L2A and L2B get all L1 atoms
(@check-equal? (sort t28l2a-upper symbol<?) (sort t28l2b-upper symbol<?)
               "T28: L2A and L2B identical (no partition at L1)")
(@check-equal? (sort t28l2a-upper symbol<?) (sort t28l1-upper symbol<?)
               "T28: L2A = all L1 atoms (no partition at L1)")
;; L2A: partition also skipped because skip at L1 propagates down
(@check-equal? (sort t28l3a-upper symbol<?) (sort t28l3b-upper symbol<?)
               "T28: L3A and L3B identical (skip cascades from L1)")
(@check-equal? (length t28l3a-upper) 6
               "T28: L3A gets all 6 atoms (not 2) due to cascading skip")

(forge:close-run t28-run)

;; ================================================================
;; Test 29: one-sig child + lone-sig child + normal child under abstract parent
;; Parent=6. OneChild: exact 1. LoneChild: upper 1 (no lower). NormalChild: scope 3.
;; 1 lower-bound atom (from one), 5 surplus. Total need=0+1+3=4 ≤ 5 → partition.
;; ================================================================
(sig T29P #:abstract)
(sig T29One #:one #:extends T29P)
(sig T29Lone #:lone #:extends T29P)
(sig T29Norm #:extends T29P)

(run t29-run #:preds []
     #:scope ([T29P 6] [T29Norm 3]))
     ;; one → Range(1,1), lone → Range(0,1)

(define t29p-upper (upper-atoms t29-run T29P))
(define t29one-upper (upper-atoms t29-run T29One))
(define t29one-lower (lower-atoms t29-run T29One))
(define t29lone-upper (upper-atoms t29-run T29Lone))
(define t29norm-upper (upper-atoms t29-run T29Norm))

(@check-equal? (length t29p-upper) 6 "T29: Parent upper = 6")
(@check-equal? (length t29one-upper) 1 "T29: one child upper = 1")
(@check-equal? t29one-upper t29one-lower "T29: one child is exact-bounded")
(@check-equal? (length t29lone-upper) 1 "T29: lone child upper = 1 (partitioned)")
(@check-equal? (length t29norm-upper) 3 "T29: Normal child upper = 3 (partitioned)")
;; All three children should be pairwise disjoint
(@check-true (disjoint? t29one-upper t29lone-upper) "T29: one and lone disjoint")
(@check-true (disjoint? t29one-upper t29norm-upper) "T29: one and Normal disjoint")
(@check-true (disjoint? t29lone-upper t29norm-upper) "T29: lone and Normal disjoint")
(@check-true (subset-of? t29lone-upper t29p-upper) "T29: lone ⊆ Parent")
(@check-true (subset-of? t29norm-upper t29p-upper) "T29: Normal ⊆ Parent")

(forge:close-run t29-run)

;; ================================================================
;; Test 30: `is func` breaker on child-sig field — partition unaffected
;; Breakers constrain relation bounds, not sig bounds.
;; Partition should still fire normally.
;; ================================================================
(sig T30P #:abstract)
(sig T30A #:extends T30P)
(sig T30B #:extends T30P)
(relation t30f (T30A T30P))

(inst t30-inst (is t30f func))

(run t30-run #:preds []
     #:scope ([T30P 6] [T30A 3] [T30B 3])
     #:bounds [t30-inst])

(define t30p-upper (upper-atoms t30-run T30P))
(define t30a-upper (upper-atoms t30-run T30A))
(define t30b-upper (upper-atoms t30-run T30B))

(@check-equal? (length t30p-upper) 6 "T30: Parent upper = 6")
(@check-equal? (length t30a-upper) 3 "T30: A upper = 3 (partitioned despite func breaker)")
(@check-equal? (length t30b-upper) 3 "T30: B upper = 3 (partitioned despite func breaker)")
(@check-true (disjoint? t30a-upper t30b-upper) "T30: A and B disjoint (breaker doesn't block partition)")
(@check-true (subset-of? t30a-upper t30p-upper) "T30: A ⊆ Parent")
(@check-true (subset-of? t30b-upper t30p-upper) "T30: B ⊆ Parent")

(forge:close-run t30-run)

;; ================================================================
;; Test 31: Breaker (is linear) on child-sig field — partition unaffected
;; Same principle as T30 but with a linear (injective) breaker.
;; ================================================================
(sig T31P #:abstract)
(sig T31A #:extends T31P)
(sig T31B #:extends T31P)
(relation t31f (T31A T31P))

(inst t31-inst (is t31f linear))

(run t31-run #:preds []
     #:scope ([T31P 6] [T31A 3] [T31B 3])
     #:bounds [t31-inst])

(define t31p-upper (upper-atoms t31-run T31P))
(define t31a-upper (upper-atoms t31-run T31A))
(define t31b-upper (upper-atoms t31-run T31B))

(@check-equal? (length t31p-upper) 6 "T31: Parent upper = 6")
(@check-equal? (length t31a-upper) 3 "T31: A upper = 3 (partitioned despite linear breaker)")
(@check-equal? (length t31b-upper) 3 "T31: B upper = 3 (partitioned despite linear breaker)")
(@check-true (disjoint? t31a-upper t31b-upper) "T31: A and B disjoint (breaker doesn't block partition)")

(forge:close-run t31-run)

;; ================================================================
;; Test 32: `in` (upper-only) inst bound referencing surplus atoms
;; `(in rel expr)` sets an upper bound on the relation.
;; The referenced atoms T32P0, T32P1 are surplus → partition skipped.
;; ================================================================
(sig T32P #:abstract)
(sig T32A #:extends T32P)
(sig T32B #:extends T32P)
(relation t32link (T32A T32P))

;; Upper-only bound using surplus atoms T32P0, T32P1
(inst t32-inst
    (in t32link (-> (atom 'T32P0) (atom 'T32P1))))

(run t32-run #:preds []
     #:scope ([T32P 6] [T32A 3] [T32B 3])
     #:bounds [t32-inst])

(define t32p-upper (upper-atoms t32-run T32P))
(define t32a-upper (upper-atoms t32-run T32A))
(define t32b-upper (upper-atoms t32-run T32B))

(@check-equal? (length t32p-upper) 6 "T32: Parent upper = 6")
;; T32P0, T32P1 are surplus atoms → partition skipped
(@check-equal? (sort t32a-upper symbol<?) (sort t32b-upper symbol<?)
               "T32: A and B identical (no partition, `in` uses surplus atoms)")
(@check-equal? (sort t32a-upper symbol<?) (sort t32p-upper symbol<?)
               "T32: children upper = parent upper (no partition)")

(forge:close-run t32-run)

;; ================================================================
;; Test 33: `in` (upper-only) bound referencing only lower-bound atoms
;; Same as T32 but atoms reference only the one-sig lower-bound atom.
;; No surplus atoms referenced → partition fires.
;; ================================================================
(sig T33P #:abstract)
(sig T33One #:one #:extends T33P)
(sig T33Norm #:extends T33P)
(relation t33link (T33One T33P))

;; Upper-only bound using only the one-sig lower-bound atom
(inst t33-inst
    (in t33link (-> (atom 'T33One0) (atom 'T33One0))))

(run t33-run #:preds []
     #:scope ([T33P 4] [T33Norm 3])
     #:bounds [t33-inst])

(define t33p-upper (upper-atoms t33-run T33P))
(define t33one-upper (upper-atoms t33-run T33One))
(define t33norm-upper (upper-atoms t33-run T33Norm))

(@check-equal? (length t33p-upper) 4 "T33: Parent upper = 4")
(@check-equal? (length t33one-upper) 1 "T33: one sig upper = 1")
;; No surplus atoms referenced → partition fires
(@check-equal? (length t33norm-upper) 3
               "T33: Norm upper = 3 (partitioned despite `in` bound)")
(@check-true (disjoint? t33one-upper t33norm-upper) "T33: one and Norm disjoint")

(forge:close-run t33-run)

;; ================================================================
;; Test 34: `ni` (lower-only) inst bound referencing surplus atoms
;; `(ni rel expr)` sets a lower bound on the relation.
;; The referenced atoms T34P0, T34P1 are surplus → partition skipped.
;; ================================================================
(sig T34P #:abstract)
(sig T34A #:extends T34P)
(sig T34B #:extends T34P)
(relation t34link (T34A T34P))

;; Lower-only bound using surplus atoms T34P0, T34P1
(inst t34-inst
    (ni t34link (-> (atom 'T34P0) (atom 'T34P1))))

(run t34-run #:preds []
     #:scope ([T34P 6] [T34A 3] [T34B 3])
     #:bounds [t34-inst])

(define t34p-upper (upper-atoms t34-run T34P))
(define t34a-upper (upper-atoms t34-run T34A))
(define t34b-upper (upper-atoms t34-run T34B))

(@check-equal? (length t34p-upper) 6 "T34: Parent upper = 6")
;; T34P0, T34P1 are surplus atoms → partition skipped
(@check-equal? (sort t34a-upper symbol<?) (sort t34b-upper symbol<?)
               "T34: A and B identical (no partition, `ni` uses surplus atoms)")
(@check-equal? (sort t34a-upper symbol<?) (sort t34p-upper symbol<?)
               "T34: children upper = parent upper (no partition)")

(forge:close-run t34-run)

;; ================================================================
;; Test 35: `ni` (lower-only) bound referencing only lower-bound atoms
;; Same as T34 but atoms reference only the one-sig lower-bound atom.
;; No surplus atoms referenced → partition fires.
;; ================================================================
(sig T35P #:abstract)
(sig T35One #:one #:extends T35P)
(sig T35Norm #:extends T35P)
(relation t35link (T35One T35P))

;; Lower-only bound using only the one-sig lower-bound atom
(inst t35-inst
    (ni t35link (-> (atom 'T35One0) (atom 'T35One0))))

(run t35-run #:preds []
     #:scope ([T35P 4] [T35Norm 3])
     #:bounds [t35-inst])

(define t35p-upper (upper-atoms t35-run T35P))
(define t35one-upper (upper-atoms t35-run T35One))
(define t35norm-upper (upper-atoms t35-run T35Norm))

(@check-equal? (length t35p-upper) 4 "T35: Parent upper = 4")
(@check-equal? (length t35one-upper) 1 "T35: one sig upper = 1")
;; No surplus atoms referenced → partition fires
(@check-equal? (length t35norm-upper) 3
               "T35: Norm upper = 3 (partitioned despite `ni` bound)")
(@check-true (disjoint? t35one-upper t35norm-upper) "T35: one and Norm disjoint")

(forge:close-run t35-run)

;; ================================================================
;; Test 36: Field targeting Int with bound — partition unaffected
;; A field `f: OneChild -> Int` bound with `=`. The one-sig atom is in
;; lower bounds and the Int atom is an integer, so neither is a surplus
;; parent-named symbol. Partition fires normally.
;; ================================================================
(sig T36P #:abstract)
(sig T36One #:one #:extends T36P)
(sig T36Norm #:extends T36P)
(relation t36f (T36One Int))

;; Bind with one-sig atom → Int: T36One0 is lower-bound, (int 1) is integer
(inst t36-inst
    (= t36f (-> (atom 'T36One0) (sing (int 1)))))

(run t36-run #:preds []
     #:scope ([T36P 4] [T36Norm 3])
     #:bounds [t36-inst])

(define t36p-upper (upper-atoms t36-run T36P))
(define t36one-upper (upper-atoms t36-run T36One))
(define t36norm-upper (upper-atoms t36-run T36Norm))

(@check-equal? (length t36p-upper) 4 "T36: Parent upper = 4")
(@check-equal? (length t36one-upper) 1 "T36: one sig upper = 1")
;; Neither atom is a surplus parent-named symbol → partition fires
(@check-equal? (length t36norm-upper) 3
               "T36: Norm upper = 3 (partitioned, Int field bound didn't block)")
(@check-true (disjoint? t36one-upper t36norm-upper) "T36: one and Norm disjoint")

(forge:close-run t36-run)

;; ================================================================
;; Test 37: 3-level — partition fires at root, independently skips at mid
;; Root=6, Mid=3, Other=3. Root level: 3+3=6 ≤ 6 → partition fires.
;; Mid has LeafA=2, LeafB=2. Mid level: 2+2=4 > 3 → partition skips.
;; The skip at mid is due to mid's OWN over-commitment, not cascaded from root.
;; Mid received clean atoms from root's partition (clean-shared?=#t).
;; ================================================================
(sig T37Root #:abstract)
(sig T37Mid #:abstract #:extends T37Root)
(sig T37LeafA #:extends T37Mid)
(sig T37LeafB #:extends T37Mid)
(sig T37Other #:extends T37Root)

(run t37-run #:preds []
     #:scope ([T37Root 6] [T37Mid 3] [T37LeafA 2] [T37LeafB 2] [T37Other 3]))

(define t37root-upper (upper-atoms t37-run T37Root))
(define t37mid-upper (upper-atoms t37-run T37Mid))
(define t37la-upper (upper-atoms t37-run T37LeafA))
(define t37lb-upper (upper-atoms t37-run T37LeafB))
(define t37other-upper (upper-atoms t37-run T37Other))

(@check-equal? (length t37root-upper) 6 "T37: Root upper = 6")
;; Root level: partition fires → Mid and Other get disjoint slices
(@check-equal? (length t37mid-upper) 3 "T37: Mid upper = 3 (partitioned from root)")
(@check-equal? (length t37other-upper) 3 "T37: Other upper = 3 (partitioned from root)")
(@check-true (disjoint? t37mid-upper t37other-upper) "T37: Mid and Other are disjoint")
;; Mid level: partition skips (2+2=4 > 3) → LeafA and LeafB share all Mid atoms
(@check-equal? (length t37la-upper) 3 "T37: LeafA upper = 3 (gets all Mid atoms)")
(@check-equal? (length t37lb-upper) 3 "T37: LeafB upper = 3 (gets all Mid atoms)")
(@check-equal? (sort t37la-upper symbol<?) (sort t37lb-upper symbol<?)
               "T37: LeafA and LeafB identical (partition skipped at mid)")
(@check-equal? (sort t37la-upper symbol<?) (sort t37mid-upper symbol<?)
               "T37: Leaves = all Mid atoms (no partition at mid)")
;; Containment
(@check-true (subset-of? t37mid-upper t37root-upper) "T37: Mid ⊆ Root")
(@check-true (subset-of? t37other-upper t37root-upper) "T37: Other ⊆ Root")
(@check-true (subset-of? t37la-upper t37mid-upper) "T37: LeafA ⊆ Mid")

(forge:close-run t37-run)

;; ================================================================
;; Test 38: All four sibling types under one abstract parent
;; Parent=8 (abstract). Children:
;;   one child (1 lower-bound atom, need=0)
;;   lone child (scope 0..1, need=1)
;;   abstract child with grandchildren (scope 4, need=4)
;;   normal child (scope 2, need=2)
;; Surplus = 8 - 1 (one) = 7. Total need = 0+1+4+2 = 7 ≤ 7 → partition fires.
;; At abstract child level: Grand1(2)+Grand2(2)=4 ≤ 4 → partition fires.
;; ================================================================
(sig T38P #:abstract)
(sig T38One #:one #:extends T38P)
(sig T38Lone #:lone #:extends T38P)
(sig T38Abs #:abstract #:extends T38P)
(sig T38Grand1 #:extends T38Abs)
(sig T38Grand2 #:extends T38Abs)
(sig T38Norm #:extends T38P)

(run t38-run #:preds []
     #:scope ([T38P 8] [T38Abs 4] [T38Grand1 2] [T38Grand2 2] [T38Norm 2]))

(define t38p-upper (upper-atoms t38-run T38P))
(define t38one-upper (upper-atoms t38-run T38One))
(define t38one-lower (lower-atoms t38-run T38One))
(define t38lone-upper (upper-atoms t38-run T38Lone))
(define t38abs-upper (upper-atoms t38-run T38Abs))
(define t38g1-upper (upper-atoms t38-run T38Grand1))
(define t38g2-upper (upper-atoms t38-run T38Grand2))
(define t38norm-upper (upper-atoms t38-run T38Norm))

(@check-equal? (length t38p-upper) 8 "T38: Parent upper = 8")
(@check-equal? (length t38one-upper) 1 "T38: one child upper = 1")
(@check-equal? t38one-upper t38one-lower "T38: one child is exact-bounded")
(@check-equal? (length t38lone-upper) 1 "T38: lone child upper = 1 (partitioned)")
(@check-equal? (length t38abs-upper) 4 "T38: abstract child upper = 4 (partitioned)")
(@check-equal? (length t38norm-upper) 2 "T38: normal child upper = 2 (partitioned)")
;; All siblings pairwise disjoint
(@check-true (disjoint? t38one-upper t38lone-upper) "T38: one and lone disjoint")
(@check-true (disjoint? t38one-upper t38abs-upper) "T38: one and abs disjoint")
(@check-true (disjoint? t38one-upper t38norm-upper) "T38: one and norm disjoint")
(@check-true (disjoint? t38lone-upper t38abs-upper) "T38: lone and abs disjoint")
(@check-true (disjoint? t38lone-upper t38norm-upper) "T38: lone and norm disjoint")
(@check-true (disjoint? t38abs-upper t38norm-upper) "T38: abs and norm disjoint")
;; Grandchildren partitioned within abstract child
(@check-equal? (length t38g1-upper) 2 "T38: Grand1 upper = 2 (partitioned from abs)")
(@check-equal? (length t38g2-upper) 2 "T38: Grand2 upper = 2 (partitioned from abs)")
(@check-true (disjoint? t38g1-upper t38g2-upper) "T38: Grand1 and Grand2 disjoint")
(@check-true (subset-of? t38g1-upper t38abs-upper) "T38: Grand1 ⊆ abs")
(@check-true (subset-of? t38g2-upper t38abs-upper) "T38: Grand2 ⊆ abs")
;; All subsets of parent
(@check-true (subset-of? t38one-upper t38p-upper) "T38: one ⊆ Parent")
(@check-true (subset-of? t38lone-upper t38p-upper) "T38: lone ⊆ Parent")
(@check-true (subset-of? t38abs-upper t38p-upper) "T38: abs ⊆ Parent")
(@check-true (subset-of? t38norm-upper t38p-upper) "T38: norm ⊆ Parent")

(forge:close-run t38-run)

;; ================================================================
;; Test 39: 4-level hierarchy with varying partition decisions
;; Root(12) → BranchA(7) → MidA(5) → SubA(3) → {LeafA(2), LeafB(2)}
;;                        → SibA(2)  → SubB(3)
;;          → BranchB(5)
;; Level 1 (Root→children): 7+5=12 ≤ 12 → partition fires.
;; Level 2 (BranchA→children): 5+2=7 ≤ 7 → partition fires.
;; Level 3 (MidA→children): 3+3=6 > 5 → partition SKIPS (own over-commitment).
;; Level 4 (SubA→children): cascade skip (dirty shared from MidA).
;; ================================================================
(sig T39Root #:abstract)
(sig T39BrA #:abstract #:extends T39Root)
(sig T39MidA #:abstract #:extends T39BrA)
(sig T39SubA #:abstract #:extends T39MidA)
(sig T39LeafA #:extends T39SubA)
(sig T39LeafB #:extends T39SubA)
(sig T39SubB #:extends T39MidA)
(sig T39SibA #:extends T39BrA)
(sig T39BrB #:extends T39Root)

(run t39-run #:preds []
     #:scope ([T39Root 12] [T39BrA 7] [T39MidA 5] [T39SubA 3]
              [T39LeafA 2] [T39LeafB 2] [T39SubB 3] [T39SibA 2] [T39BrB 5]))

(define t39root-upper (upper-atoms t39-run T39Root))
(define t39bra-upper (upper-atoms t39-run T39BrA))
(define t39mida-upper (upper-atoms t39-run T39MidA))
(define t39suba-upper (upper-atoms t39-run T39SubA))
(define t39la-upper (upper-atoms t39-run T39LeafA))
(define t39lb-upper (upper-atoms t39-run T39LeafB))
(define t39subb-upper (upper-atoms t39-run T39SubB))
(define t39siba-upper (upper-atoms t39-run T39SibA))
(define t39brb-upper (upper-atoms t39-run T39BrB))

(@check-equal? (length t39root-upper) 12 "T39: Root upper = 12")
;; Level 1: partition fires
(@check-equal? (length t39bra-upper) 7 "T39: BranchA upper = 7 (partitioned from root)")
(@check-equal? (length t39brb-upper) 5 "T39: BranchB upper = 5 (partitioned from root)")
(@check-true (disjoint? t39bra-upper t39brb-upper) "T39: BranchA and BranchB disjoint")
;; Level 2: partition fires
(@check-equal? (length t39mida-upper) 5 "T39: MidA upper = 5 (partitioned from BranchA)")
(@check-equal? (length t39siba-upper) 2 "T39: SibA upper = 2 (partitioned from BranchA)")
(@check-true (disjoint? t39mida-upper t39siba-upper) "T39: MidA and SibA disjoint")
;; Level 3: partition SKIPS (3+3=6 > 5) — SubA and SubB share all MidA atoms
(@check-equal? (length t39suba-upper) 5 "T39: SubA upper = 5 (gets all MidA atoms, not 3)")
(@check-equal? (length t39subb-upper) 5 "T39: SubB upper = 5 (gets all MidA atoms)")
(@check-equal? (sort t39suba-upper symbol<?) (sort t39subb-upper symbol<?)
               "T39: SubA and SubB identical (partition skipped at MidA)")
(@check-equal? (sort t39suba-upper symbol<?) (sort t39mida-upper symbol<?)
               "T39: SubA = all MidA atoms (no partition)")
;; Level 4: cascade skip — LeafA and LeafB share all atoms passed through SubA
(@check-equal? (length t39la-upper) 5 "T39: LeafA upper = 5 (cascade skip)")
(@check-equal? (length t39lb-upper) 5 "T39: LeafB upper = 5 (cascade skip)")
(@check-equal? (sort t39la-upper symbol<?) (sort t39lb-upper symbol<?)
               "T39: LeafA and LeafB identical (cascade skip from MidA)")
;; Containment chain
(@check-true (subset-of? t39bra-upper t39root-upper) "T39: BranchA ⊆ Root")
(@check-true (subset-of? t39mida-upper t39bra-upper) "T39: MidA ⊆ BranchA")
(@check-true (subset-of? t39suba-upper t39mida-upper) "T39: SubA ⊆ MidA")
(@check-true (subset-of? t39la-upper t39suba-upper) "T39: LeafA ⊆ SubA")

(forge:close-run t39-run)

;; ================================================================
;; Test 40: Two independent top-level hierarchies — exact children vs non-exact
;; Hierarchy 1: abstract Root1=6, exact ChildA1=3, exact ChildB1=3.
;; Hierarchy 2: abstract Root2=6, non-exact ChildA2=3, non-exact ChildB2=3.
;; Partition decisions on each hierarchy should be completely independent.
;; ================================================================
(sig T40R1 #:abstract)
(sig T40A1 #:extends T40R1)
(sig T40B1 #:extends T40R1)
(sig T40R2 #:abstract)
(sig T40A2 #:extends T40R2)
(sig T40B2 #:extends T40R2)

(run t40-run #:preds []
     #:scope ([T40R1 6] [T40A1 3 3] [T40B1 3 3] [T40R2 6] [T40A2 3] [T40B2 3]))

(define t40r1-upper (upper-atoms t40-run T40R1))
(define t40a1-upper (upper-atoms t40-run T40A1))
(define t40a1-lower (lower-atoms t40-run T40A1))
(define t40b1-upper (upper-atoms t40-run T40B1))
(define t40b1-lower (lower-atoms t40-run T40B1))
(define t40r2-upper (upper-atoms t40-run T40R2))
(define t40a2-upper (upper-atoms t40-run T40A2))
(define t40b2-upper (upper-atoms t40-run T40B2))

;; Hierarchy 1: exact children
(@check-equal? (length t40r1-upper) 6 "T40: Root1 upper = 6")
(@check-equal? (length t40a1-upper) 3 "T40: A1 upper = 3 (exact)")
(@check-equal? (length t40a1-lower) 3 "T40: A1 lower = 3 (exact)")
(@check-equal? t40a1-upper t40a1-lower "T40: A1 is exact-bounded")
(@check-equal? (length t40b1-upper) 3 "T40: B1 upper = 3 (exact)")
(@check-equal? t40b1-upper t40b1-lower "T40: B1 is exact-bounded")
(@check-true (disjoint? t40a1-upper t40b1-upper) "T40: A1 and B1 disjoint")
;; Hierarchy 2: non-exact children (partition fires)
(@check-equal? (length t40r2-upper) 6 "T40: Root2 upper = 6")
(@check-equal? (length t40a2-upper) 3 "T40: A2 upper = 3 (partitioned)")
(@check-equal? (length t40b2-upper) 3 "T40: B2 upper = 3 (partitioned)")
(@check-true (disjoint? t40a2-upper t40b2-upper) "T40: A2 and B2 disjoint (partitioned)")
;; Cross-hierarchy: atoms should be completely separate
(@check-true (disjoint? t40r1-upper t40r2-upper) "T40: Root1 and Root2 atoms completely separate")

(forge:close-run t40-run)

(printf "All bounds inspection tests passed.~n")
