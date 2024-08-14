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

(declare-fun Helper () (Relation Atom))
(declare-const Helper_atom Atom)
(assert (= Helper (set.singleton (tuple Helper_atom))))

(declare-fun succ () (Relation IntAtom IntAtom))

(declare-fun children () (Relation Atom Atom))

(declare-fun parents () (Relation Atom Atom))

(declare-fun siblings () (Relation Atom Atom))

(declare-fun spouse () (Relation Atom Atom Atom))


(assert (= Person (set.union Man Woman)))

(assert (= (set.inter Man Woman) (as set.empty (Relation Atom))))
(assert (= (set.inter Woman Man) (as set.empty (Relation Atom))))











(assert (= (set.inter Helper Person) (as set.empty (Relation Atom))))
(assert (= (set.inter Person Helper) (as set.empty (Relation Atom))))


(assert (not (or (not (forall ((p Atom)) (=> (and (set.member (tuple p) Person)) (not (not (= (as set.empty (Relation IntAtom)) (set.inter (rel.join (set.singleton (tuple p)) (set.union (rel.tclosure parents) (rel.iden (as set.universe (Relation Atom))))) (rel.join (rel.join (set.singleton (tuple p)) (rel.join Helper spouse)) (set.union (rel.tclosure parents) (rel.iden (as set.universe (Relation Atom)))))))))))) (forall ((p Atom)) (=> (and (set.member (tuple p) Person)) (not (not (= (as set.empty (Relation Atom)) (set.inter (rel.join (set.singleton (tuple p)) (rel.tclosure parents)) (rel.join (rel.join (set.singleton (tuple p)) (rel.join Helper spouse)) (rel.tclosure parents)))))))))))
(assert (set.subset succ (rel.product (as set.universe (Relation IntAtom)) (as set.universe (Relation IntAtom)))))
(assert (set.subset children (rel.product Person Person)))
(assert (set.subset parents (rel.product Person Person)))
(assert (set.subset siblings (rel.product Person Person)))
(assert (set.subset spouse (rel.product Helper (rel.product Person Person))))
(assert (= Person (set.union Man Woman)))
(assert (= (as set.empty (Relation Atom)) (set.inter Man Woman)))
(assert (forall ((pfunc12125 Atom)) (=> (and (set.member (tuple pfunc12125) Helper)) (forall ((pfunc12126 Atom)) (=> (and (set.member (tuple pfunc12126) Person)) (forall ((x_0 Atom) (x_1 Atom)) (=> (and (set.subset (set.singleton (tuple x_0)) (rel.join (set.singleton (tuple pfunc12126)) (rel.join (set.singleton (tuple pfunc12125)) spouse))) (set.subset (set.singleton (tuple x_1)) (rel.join (set.singleton (tuple pfunc12126)) (rel.join (set.singleton (tuple pfunc12125)) spouse)))) (and (= x_0 x_1)))))))))