(set-logic ALL)
(set-option :produce-models true)
(set-option :finite-model-find true)

(declare-sort Atom 0)


(declare-const Person0 Atom)
 (declare-const Person1 Atom)
 (declare-const Person2 Atom)
 (declare-const Person3 Atom)

(declare-fun Person () (Relation Atom))
(assert (set.member (tuple Person0) Person))
 (assert (set.member (tuple Person1) Person))
 (assert (set.member (tuple Person2) Person))
 (assert (set.member (tuple Person3) Person))

(assert (= Person (set.union (set.singleton (tuple Person0)) (set.singleton (tuple Person1)) (set.singleton (tuple Person2)) (set.singleton (tuple Person3)))))


(declare-const Animal0 Atom)
 (declare-const Animal1 Atom)
 (declare-const Animal2 Atom)
 (declare-const Animal3 Atom)

(declare-fun Animal () (Relation Atom))
(assert (set.member (tuple Animal0) Animal))
 (assert (set.member (tuple Animal1) Animal))
 (assert (set.member (tuple Animal2) Animal))
 (assert (set.member (tuple Animal3) Animal))

(assert (= Animal (set.union (set.singleton (tuple Animal0)) (set.singleton (tuple Animal1)) (set.singleton (tuple Animal2)) (set.singleton (tuple Animal3)))))


(declare-fun succ () (Relation Int Int))


(declare-fun parent () (Relation Atom Atom))
(assert (set.subset parent (set.union (set.singleton (tuple Person1 Person2)) (set.singleton (tuple Person2 Person1)) (set.singleton (tuple Person2 Person2)) (set.singleton (tuple Person1 Person1)) (set.singleton (tuple Person2 Person0)) (set.singleton (tuple Person0 Person3)) (set.singleton (tuple Person2 Person3)) (set.singleton (tuple Person3 Person0)) (set.singleton (tuple Person3 Person3)) (set.singleton (tuple Person1 Person3)) (set.singleton (tuple Person0 Person2)) (set.singleton (tuple Person1 Person0)) (set.singleton (tuple Person3 Person1)) (set.singleton (tuple Person3 Person2)) (set.singleton (tuple Person0 Person1)) (set.singleton (tuple Person0 Person0)))))


(declare-fun age () (Relation Atom Int))


(declare-fun owner () (Relation Atom Atom))
(assert (set.subset owner (set.union (set.singleton (tuple Animal1 Person1)) (set.singleton (tuple Animal0 Person3)) (set.singleton (tuple Animal2 Person3)) (set.singleton (tuple Animal0 Person2)) (set.singleton (tuple Animal0 Person1)) (set.singleton (tuple Animal1 Person3)) (set.singleton (tuple Animal2 Person0)) (set.singleton (tuple Animal0 Person0)) (set.singleton (tuple Animal3 Person1)) (set.singleton (tuple Animal2 Person2)) (set.singleton (tuple Animal2 Person1)) (set.singleton (tuple Animal1 Person2)) (set.singleton (tuple Animal1 Person0)) (set.singleton (tuple Animal3 Person3)) (set.singleton (tuple Animal3 Person2)) (set.singleton (tuple Animal3 Person0)))))


(declare-const $p_some17195 Atom)


(assert (= (set.inter Animal Person) (as set.empty (Relation Atom))))
(assert (= (set.inter Person Animal) (as set.empty (Relation Atom))))

(assert (forall ((pfunc17198 Atom)) (=> (and (set.member (tuple pfunc17198) Animal)) (or (or (= (rel.join (set.singleton (tuple pfunc17198)) owner) (set.singleton (tuple Person0))) (= (rel.join (set.singleton (tuple pfunc17198)) owner) (set.singleton (tuple Person1))) (= (rel.join (set.singleton (tuple pfunc17198)) owner) (set.singleton (tuple Person2))) (= (rel.join (set.singleton (tuple pfunc17198)) owner) (set.singleton (tuple Person3)))) (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple pfunc17198)) owner)))))
;(assert (forall ((pfunc17197 Atom)) (=> (and (set.member (tuple pfunc17197) Person)) (or (= (rel.join (set.singleton (tuple pfunc17197)) age) (set.singleton (tuple -8))) (= (rel.join (set.singleton (tuple pfunc17197)) age) (set.singleton (tuple -7))) (= (rel.join (set.singleton (tuple pfunc17197)) age) (set.singleton (tuple -6))) (= (rel.join (set.singleton (tuple pfunc17197)) age) (set.singleton (tuple -5))) (= (rel.join (set.singleton (tuple pfunc17197)) age) (set.singleton (tuple -4))) (= (rel.join (set.singleton (tuple pfunc17197)) age) (set.singleton (tuple -3))) (= (rel.join (set.singleton (tuple pfunc17197)) age) (set.singleton (tuple -2))) (= (rel.join (set.singleton (tuple pfunc17197)) age) (set.singleton (tuple -1))) (= (rel.join (set.singleton (tuple pfunc17197)) age) (set.singleton (tuple 0))) (= (rel.join (set.singleton (tuple pfunc17197)) age) (set.singleton (tuple 1))) (= (rel.join (set.singleton (tuple pfunc17197)) age) (set.singleton (tuple 2))) (= (rel.join (set.singleton (tuple pfunc17197)) age) (set.singleton (tuple 3))) (= (rel.join (set.singleton (tuple pfunc17197)) age) (set.singleton (tuple 4))) (= (rel.join (set.singleton (tuple pfunc17197)) age) (set.singleton (tuple 5))) (= (rel.join (set.singleton (tuple pfunc17197)) age) (set.singleton (tuple 6))) (= (rel.join (set.singleton (tuple pfunc17197)) age) (set.singleton (tuple 7)))))))
(assert (forall ((pfunc17196 Atom)) (=> (and (set.member (tuple pfunc17196) Person)) (or (or (= (rel.join (set.singleton (tuple pfunc17196)) parent) (set.singleton (tuple Person0))) (= (rel.join (set.singleton (tuple pfunc17196)) parent) (set.singleton (tuple Person1))) (= (rel.join (set.singleton (tuple pfunc17196)) parent) (set.singleton (tuple Person2))) (= (rel.join (set.singleton (tuple pfunc17196)) parent) (set.singleton (tuple Person3)))) (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple pfunc17196)) parent))))))
(assert (set.subset owner (rel.product Animal Person)))
;(assert (set.subset age (rel.product Person (as set.universe (Relation Int)))))
(assert (set.subset parent (rel.product Person Person)))
;(assert (set.subset succ (rel.product (as set.universe (Relation Int)) (as set.universe (Relation Int)))))
(assert (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple $p_some17195)) parent)))
(check-sat)
(get-model)