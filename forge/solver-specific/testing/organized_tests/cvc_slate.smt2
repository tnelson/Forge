(reset)
(declare-sort Atom 0)
(declare-sort IntAtom 0)
(declare-fun IntAtom-to-Int (IntAtom) Int)
(define-fun sign ((x__sign Int)) Int (ite (< x__sign 0) -1 (ite (> x__sign 0) 1 0)))
(define-fun reconcile-int_atom ((aset (Relation IntAtom))) IntAtom ((_ tuple.select 0) (set.choose aset)))
(assert (forall ((x1 IntAtom) (x2 IntAtom)) (=> (not (= x1 x2)) (not (= (IntAtom-to-Int x1) (IntAtom-to-Int x2))))))


(declare-fun Person () (Relation Atom))

(declare-fun succ () (Relation IntAtom IntAtom))

(declare-fun parent () (Relation Atom Atom))

(declare-fun age () (Relation Atom IntAtom))







(declare-const g48061_atom IntAtom)
(declare-fun g48060 () (Relation IntAtom))
(assert (exists () (and (= (IntAtom-to-Int g48061_atom) g48059_atom) (set.subset (set.singleton (tuple g48061_atom)) g48060))))
(declare-const g48059_atom IntAtom)
(assert (= (IntAtom-to-Int g48059_atom) 5))
(declare-const g48058_atom IntAtom)
(declare-fun g48057 () (Relation IntAtom))
(assert (exists () (and (= (IntAtom-to-Int g48058_atom) g48056_atom) (set.subset (set.singleton (tuple g48058_atom)) g48057))))
(declare-const g48056_atom IntAtom)
(assert (= (IntAtom-to-Int g48056_atom) 5))
(declare-const g48053_atom IntAtom)
(declare-fun g48052 (Atom Atom) (Relation IntAtom))
(assert (exists ((p2 Atom) (p1 Atom)) (and (= (IntAtom-to-Int g48053_atom) (+ (IntAtom-to-Int (reconcile-int_atom (rel.join (set.singleton (tuple p1)) age))) 1)) (set.subset (set.singleton (tuple g48053_atom)) (g48052 p2 p1)))))
(assert (= g48057 g48060))
(assert (set.subset succ (rel.product (as set.universe (Relation IntAtom)) (as set.universe (Relation IntAtom)))))
(assert (set.subset parent (rel.product Person Person)))
(assert (set.subset age (rel.product Person (as set.universe (Relation IntAtom)))))
(assert (forall ((pfunc48054 Atom)) (=> (and (set.member (tuple pfunc48054) Person)) (and (forall ((x1 Atom) (x2 Atom)) (=> (and (set.subset (set.singleton (tuple x1)) (rel.join (set.singleton (tuple pfunc48054)) parent)) (set.subset (set.singleton (tuple x2)) (rel.join (set.singleton (tuple pfunc48054)) parent))) (= x1 x2))) (exists ((x1 Atom)) (set.subset (set.singleton (tuple x1)) (rel.join (set.singleton (tuple pfunc48054)) parent)))))))
(assert (forall ((pfunc48055 Atom)) (=> (and (set.member (tuple pfunc48055) Person)) (and (forall ((x1 IntAtom) (x2 IntAtom)) (=> (and (set.subset (set.singleton (tuple x1)) (rel.join (set.singleton (tuple pfunc48055)) age)) (set.subset (set.singleton (tuple x2)) (rel.join (set.singleton (tuple pfunc48055)) age))) (= x1 x2))) (exists ((x1 IntAtom)) (set.subset (set.singleton (tuple x1)) (rel.join (set.singleton (tuple pfunc48055)) age)))))))
(check-sat)
(get-model)