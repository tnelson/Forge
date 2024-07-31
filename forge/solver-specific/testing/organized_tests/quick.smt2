(reset)
(set-logic ALL)
(declare-sort Atom 0)
(declare-sort IntAtom 0)

(declare-fun Person () (Relation Atom))
(declare-fun age () (Relation Atom IntAtom))
(declare-fun IntAtom-to-Int (IntAtom) Int)

(define-fun reconcile-intatom ((aset (Relation IntAtom))) IntAtom ((_ tuple.select 0) (set.choose aset)))

(assert (forall ((x1 IntAtom) (x2 IntAtom)) (=> (not (= x1 x2)) (not (= (IntAtom-to-Int x1) (IntAtom-to-Int x2))))))

(assert (exists ((p Atom)) (and (and (set.member (tuple p) Person)) (> (IntAtom-to-Int (reconcile-intatom (rel.join (set.singleton (tuple p)) age))) 0))))
(assert (set.subset age (rel.product Person (as set.universe (Relation IntAtom)))))
(assert (forall ((pfunc5777 Atom)) (=> (and (set.member (tuple pfunc5777) Person)) (and (forall ((x1 IntAtom) (x2 IntAtom)) (=> (and (set.subset (set.singleton (tuple x1)) (rel.join (set.singleton (tuple pfunc5777)) age)) (set.subset (set.singleton (tuple x2)) (rel.join (set.singleton (tuple pfunc5777)) age))) (= x1 x2))) (exists ((x1 IntAtom)) (set.subset (set.singleton (tuple x1)) (rel.join (set.singleton (tuple pfunc5777)) age)))))))
(check-sat)
(get-model)


; add in declare-sort IntAtom
; add in function IntAtom-to-Int
; add in function for reconcile-intatom (TODO: what to do if empty?)
; add in assert for disjointness of IntAtom-to-Int
; add in IntAtom-To-Int at all locations where reconcile-int is called (and change the latter to reconcile-intatom)
; change in the set.universe ints to set.universe intAtom
; change atom-or-int to use intatom