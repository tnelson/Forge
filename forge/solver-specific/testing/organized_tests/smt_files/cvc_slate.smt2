(reset)
(declare-sort Atom 0)
(declare-sort IntAtom 0)
(declare-fun IntAtom-to-Int (IntAtom) Int)
(define-fun sign ((x__sign Int)) Int (ite (< x__sign 0) -1 (ite (> x__sign 0) 1 0)))
(define-fun reconcile-int_atom ((aset (Relation IntAtom))) IntAtom ((_ tuple.select 0) (set.choose aset)))
(assert (forall ((x1 IntAtom) (x2 IntAtom)) (=> (not (= x1 x2)) (not (= (IntAtom-to-Int x1) (IntAtom-to-Int x2))))))
(declare-fun univInt () (Relation IntAtom))
(assert (= univInt (as set.universe (Relation IntAtom))))


(declare-fun Person () (Relation Atom))

(declare-fun Man () (Relation Atom))

(declare-fun Woman () (Relation Atom))

(declare-fun Eve () (Relation Atom))
(declare-const Eve_atom Atom)
(assert (= Eve (set.singleton (tuple Eve_atom))))

(declare-fun Adam () (Relation Atom))
(declare-const Adam_atom Atom)
(assert (= Adam (set.singleton (tuple Adam_atom))))

(declare-fun succ () (Relation IntAtom IntAtom))

(declare-fun spouse () (Relation Atom Atom))

(declare-fun parents () (Relation Atom Atom))


(assert (= Person (set.union Man Adam Woman Eve)))

(assert (= (set.inter Man Woman) (as set.empty (Relation Atom))))
(assert (= (set.inter Woman Man) (as set.empty (Relation Atom))))


(assert (= Man (set.union Man Adam)))


(assert (= Woman (set.union Woman Eve)))








(declare-fun g18954 (Atom) (Relation Atom Atom))
(assert (forall ((father Atom) (mother Atom) (p Atom)) (= (and (and (set.subset (set.singleton (tuple father)) Man) (set.subset (set.singleton (tuple mother)) Woman)) (= (rel.join (set.singleton (tuple p)) parents) (set.union (set.singleton (tuple mother)) (set.singleton (tuple father))))) (set.subset (set.singleton (tuple father mother)) (g18954 p)))))
(assert (not (or (not (forall ((p Atom)) (=> (and (set.member (tuple p) Person)) (or (not (set.subset (set.singleton (tuple p)) Person)) (and (forall ((x1 Atom Atom) (x2 Atom Atom)) (=> (and (set.subset (set.singleton (tuple x1)) (g18954 p)) (set.subset (set.singleton (tuple x2)) (g18954 p))) (= x1 x2))) (exists ((x1 Atom Atom)) (set.subset (set.singleton (tuple x1)) (g18954 p)))))))) (not (exists ((p Atom)) (and (and (set.member (tuple p) Person)) (and (set.subset (set.singleton (tuple p)) (set.minus Person (set.union Adam Eve))) (not (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple p)) spouse))))))))))
(assert (set.subset succ (rel.product (as set.universe (Relation IntAtom)) (as set.universe (Relation IntAtom)))))
(assert (set.subset spouse (rel.product Person Person)))
(assert (set.subset parents (rel.product Person Person)))
(assert (= Person (set.union Man Woman)))
(assert (= (as set.empty (Relation Atom)) (set.inter Man Woman)))
(assert (set.subset Adam Man))
(assert (set.subset Eve Woman))
(assert (forall ((pfunc18953 Atom)) (=> (and (set.member (tuple pfunc18953) Person)) (forall ((x1 Atom) (x2 Atom)) (=> (and (set.subset (set.singleton (tuple x1)) (rel.join (set.singleton (tuple pfunc18953)) spouse)) (set.subset (set.singleton (tuple x2)) (rel.join (set.singleton (tuple pfunc18953)) spouse))) (= x1 x2))))))
(check-sat)
(get-model)