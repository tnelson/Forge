(reset)
(set-logic ALL)
(set-option :produce-unsat-cores true)
(set-option :print-cores-full true)
(define-fun sign ((x__sign Int)) Int (ite (< x__sign 0) -1 (ite (> x__sign 0) 1 0)))
(define-fun reconcile-int ((aset (Relation Int))) Int (ite (= (as set.empty (Relation Int)) aset) 0 ((_ tuple.select 0) (set.choose aset))))
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


(declare-fun succ () (Relation Int Int))

(declare-fun parent1 () (Relation Atom Atom))

(declare-fun parent2 () (Relation Atom Atom))

(declare-fun spouse () (Relation Atom Atom))

(declare-const $p_some35186 Atom)


(assert (and (or (set.is_singleton (rel.join (set.singleton (tuple Person0)) parent1)) (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple Person0)) parent1))) (or (set.is_singleton (rel.join (set.singleton (tuple Person1)) parent1)) (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple Person1)) parent1))) (or (set.is_singleton (rel.join (set.singleton (tuple Person2)) parent1)) (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple Person2)) parent1))) (or (set.is_singleton (rel.join (set.singleton (tuple Person3)) parent1)) (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple Person3)) parent1)))))
(assert (and (or (set.is_singleton (rel.join (set.singleton (tuple Person0)) spouse)) (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple Person0)) spouse))) (or (set.is_singleton (rel.join (set.singleton (tuple Person1)) spouse)) (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple Person1)) spouse))) (or (set.is_singleton (rel.join (set.singleton (tuple Person2)) spouse)) (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple Person2)) spouse))) (or (set.is_singleton (rel.join (set.singleton (tuple Person3)) spouse)) (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple Person3)) spouse)))))
(assert (and (or (set.is_singleton (rel.join (set.singleton (tuple Person0)) parent2)) (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple Person0)) parent2))) (or (set.is_singleton (rel.join (set.singleton (tuple Person1)) parent2)) (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple Person1)) parent2))) (or (set.is_singleton (rel.join (set.singleton (tuple Person2)) parent2)) (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple Person2)) parent2))) (or (set.is_singleton (rel.join (set.singleton (tuple Person3)) parent2)) (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple Person3)) parent2)))))
(assert (set.subset spouse (rel.product Person Person)))
(assert (set.subset parent2 (rel.product Person Person)))
(assert (set.subset parent1 (rel.product Person Person)))
(assert (set.subset succ (rel.product (as set.universe (Relation Int)) (as set.universe (Relation Int)))))
(assert (and (not (set.subset (set.singleton (tuple Person0)) (rel.join (set.singleton (tuple Person0)) (rel.tclosure (set.union parent1 parent2)))))
             (not (set.subset (set.singleton (tuple Person1)) (rel.join (set.singleton (tuple Person1)) (rel.tclosure (set.union parent1 parent2)))))
             (not (set.subset (set.singleton (tuple Person2)) (rel.join (set.singleton (tuple Person2)) (rel.tclosure (set.union parent1 parent2)))))
             (not (set.subset (set.singleton (tuple Person3)) (rel.join (set.singleton (tuple Person3)) (rel.tclosure (set.union parent1 parent2)))))))
(assert (= (rel.join (rel.join (rel.join (rel.join (set.singleton (tuple $p_some35186)) parent1) spouse) parent1) spouse) (set.singleton (tuple $p_some35186))))
(get-timeout-core)
;(check-sat)
;(get-model)