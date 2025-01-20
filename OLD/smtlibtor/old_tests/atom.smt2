(set-logic ALL)
(set-option :produce-models true)
(set-option :sets-ext true)
(set-option :interactive true)
(set-option :incremental true)

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

(declare-fun age () (Relation Atom Int))

(declare-fun friends () (Relation Atom Int))

(declare-fun owner () (Relation Atom Atom))

(declare-const $x_g4685 Int)

(declare-const $p_some4680 Atom)

(assert (= (set.inter Animal Person) (as set.empty (Relation Atom))))
(assert (= (set.inter Person Animal) (as set.empty (Relation Atom))))

(assert (and (or (or (= (rel.join (set.singleton (tuple Animal0)) owner) (set.singleton (tuple Person0))) (= (rel.join (set.singleton (tuple Animal0)) owner) (set.singleton (tuple Person1))) (= (rel.join (set.singleton (tuple Animal0)) owner) (set.singleton (tuple Person2))) (= (rel.join (set.singleton (tuple Animal0)) owner) (set.singleton (tuple Person3)))) (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple Animal0)) owner))) (or (or (= (rel.join (set.singleton (tuple Animal1)) owner) (set.singleton (tuple Person0))) (= (rel.join (set.singleton (tuple Animal1)) owner) (set.singleton (tuple Person1))) (= (rel.join (set.singleton (tuple Animal1)) owner) (set.singleton (tuple Person2))) (= (rel.join (set.singleton (tuple Animal1)) owner) (set.singleton (tuple Person3)))) (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple Animal1)) owner))) (or (or (= (rel.join (set.singleton (tuple Animal2)) owner) (set.singleton (tuple Person0))) (= (rel.join (set.singleton (tuple Animal2)) owner) (set.singleton (tuple Person1))) (= (rel.join (set.singleton (tuple Animal2)) owner) (set.singleton (tuple Person2))) (= (rel.join (set.singleton (tuple Animal2)) owner) (set.singleton (tuple Person3)))) (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple Animal2)) owner))) (or (or (= (rel.join (set.singleton (tuple Animal3)) owner) (set.singleton (tuple Person0))) (= (rel.join (set.singleton (tuple Animal3)) owner) (set.singleton (tuple Person1))) (= (rel.join (set.singleton (tuple Animal3)) owner) (set.singleton (tuple Person2))) (= (rel.join (set.singleton (tuple Animal3)) owner) (set.singleton (tuple Person3)))) (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple Animal3)) owner)))))
(assert (and true true true true))
(assert (and (or (or (= (rel.join (set.singleton (tuple Person0)) parent) (set.singleton (tuple Person0))) (= (rel.join (set.singleton (tuple Person0)) parent) (set.singleton (tuple Person1))) (= (rel.join (set.singleton (tuple Person0)) parent) (set.singleton (tuple Person2))) (= (rel.join (set.singleton (tuple Person0)) parent) (set.singleton (tuple Person3)))) (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple Person0)) parent))) (or (or (= (rel.join (set.singleton (tuple Person1)) parent) (set.singleton (tuple Person0))) (= (rel.join (set.singleton (tuple Person1)) parent) (set.singleton (tuple Person1))) (= (rel.join (set.singleton (tuple Person1)) parent) (set.singleton (tuple Person2))) (= (rel.join (set.singleton (tuple Person1)) parent) (set.singleton (tuple Person3)))) (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple Person1)) parent))) (or (or (= (rel.join (set.singleton (tuple Person2)) parent) (set.singleton (tuple Person0))) (= (rel.join (set.singleton (tuple Person2)) parent) (set.singleton (tuple Person1))) (= (rel.join (set.singleton (tuple Person2)) parent) (set.singleton (tuple Person2))) (= (rel.join (set.singleton (tuple Person2)) parent) (set.singleton (tuple Person3)))) (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple Person2)) parent))) (or (or (= (rel.join (set.singleton (tuple Person3)) parent) (set.singleton (tuple Person0))) (= (rel.join (set.singleton (tuple Person3)) parent) (set.singleton (tuple Person1))) (= (rel.join (set.singleton (tuple Person3)) parent) (set.singleton (tuple Person2))) (= (rel.join (set.singleton (tuple Person3)) parent) (set.singleton (tuple Person3)))) (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple Person3)) parent)))))
(assert (and true true true true))
(assert (set.subset owner (rel.product Animal Person)))
(assert (set.subset friends (rel.product Person (as set.universe (Relation Int)))))
(assert (set.subset age (rel.product Person (as set.universe (Relation Int)))))
(assert (set.subset parent (rel.product Person Person)))
(assert (set.subset succ (rel.product (as set.universe (Relation Int)) (as set.universe (Relation Int)))))
(assert (and (> $x_g4685 10) (= (set.singleton (tuple $x_g4685)) (rel.join (set.singleton (tuple $p_some4680)) age))))

(check-sat)
(get-model)