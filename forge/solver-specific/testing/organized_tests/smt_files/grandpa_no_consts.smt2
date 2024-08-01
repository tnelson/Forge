(reset)
(define-fun sign ((x__sign Int)) Int (ite (< x__sign 0) -1 (ite (> x__sign 0) 1 0)))
(define-fun reconcile-int ((aset (Relation Int))) Int (ite (= (as set.empty (Relation Int)) aset) 0 ((_ tuple.select 0) (set.choose aset))))
(set-logic ALL)
(declare-sort Atom 0)


(declare-const Person0 Atom)
 (declare-const Person1 Atom)
 (declare-const Person2 Atom)
 (declare-const Person3 Atom)

(declare-fun Person () (Relation Atom))

(declare-fun succ () (Relation Int Int))

(declare-fun parent1 () (Relation Atom Atom))

(declare-fun parent2 () (Relation Atom Atom))

(declare-fun spouse () (Relation Atom Atom))

(assert (and (and (forall ((p Atom)) (=> (and (set.member (tuple p) Person)) (and (not (= (rel.join (set.singleton (tuple p)) spouse) (set.singleton (tuple p)))) (not (set.subset (set.singleton (tuple p)) (rel.join (set.singleton (tuple p)) (rel.tclosure (set.union parent1 parent2))))) (or (not (not (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple p)) spouse)))) (= (rel.join (rel.join (set.singleton (tuple p)) spouse) spouse) (set.singleton (tuple p)))) (not (set.subset (set.singleton (tuple p)) (rel.join (rel.join (set.singleton (tuple p)) spouse) (rel.tclosure (set.union parent1 parent2)))))))) (forall ((p1 Atom) (p2 Atom)) (=> (and (set.member (tuple p1) Person) (set.member (tuple p2) Person)) (or (or (not (= (as set.empty (Relation Atom)) (set.inter (set.singleton (tuple p1)) (set.singleton (tuple p2))))) (not true)) (and (or (not (set.subset (set.singleton (tuple p2)) (rel.join (set.singleton (tuple p1)) (rel.tclosure (set.union parent1 parent2))))) (not (set.subset (set.singleton (tuple p2)) (rel.join (rel.join (set.singleton (tuple p1)) spouse) (rel.tclosure (set.union parent1 parent2)))))))))) (forall ((p1 Atom) (p2 Atom)) (=> (and (set.member (tuple p1) Person) (set.member (tuple p2) Person)) (or (or (not (= (as set.empty (Relation Atom)) (set.inter (set.singleton (tuple p1)) (set.singleton (tuple p2))))) (not true)) (and (or (and (not (set.subset (set.singleton (tuple p2)) (rel.join (rel.join (set.singleton (tuple p1)) parent1) (rel.tclosure (set.union parent1 parent2))))) (not (= (set.singleton (tuple p2)) (rel.join (set.singleton (tuple p1)) parent1)))) (and (not (set.subset (set.singleton (tuple p2)) (rel.join (rel.join (set.singleton (tuple p1)) parent2) (rel.tclosure (set.union parent1 parent2))))) (not (= (set.singleton (tuple p2)) (rel.join (set.singleton (tuple p1)) parent2))))))))))))
(assert (and (and (exists ((p Atom)) (and (and (set.member (tuple p) Person)) (and (or (or (or (or (or (or (or (or (or (or (or (= (rel.join (rel.join (rel.join (set.singleton (tuple p)) parent1) parent1) spouse) (set.singleton (tuple p))) (= (rel.join (rel.join (rel.join (set.singleton (tuple p)) parent1) parent2) spouse) (set.singleton (tuple p)))) (= (rel.join (rel.join (rel.join (set.singleton (tuple p)) parent1) spouse) parent1) (set.singleton (tuple p)))) (= (rel.join (rel.join (rel.join (rel.join (set.singleton (tuple p)) parent1) spouse) parent1) spouse) (set.singleton (tuple p)))) (= (rel.join (rel.join (rel.join (set.singleton (tuple p)) parent1) spouse) parent2) (set.singleton (tuple p)))) (= (rel.join (rel.join (rel.join (rel.join (set.singleton (tuple p)) parent1) spouse) parent2) spouse) (set.singleton (tuple p)))) (= (rel.join (rel.join (rel.join (set.singleton (tuple p)) parent2) parent1) spouse) (set.singleton (tuple p)))) (= (rel.join (rel.join (rel.join (set.singleton (tuple p)) parent2) parent2) spouse) (set.singleton (tuple p)))) (= (rel.join (rel.join (rel.join (set.singleton (tuple p)) parent2) spouse) parent1) (set.singleton (tuple p)))) (= (rel.join (rel.join (rel.join (rel.join (set.singleton (tuple p)) parent2) spouse) parent1) spouse) (set.singleton (tuple p)))) (= (rel.join (rel.join (rel.join (set.singleton (tuple p)) parent2) spouse) parent2) (set.singleton (tuple p)))) (= (rel.join (rel.join (rel.join (rel.join (set.singleton (tuple p)) parent2) spouse) parent2) spouse) (set.singleton (tuple p))))))))))
(assert (set.subset succ (rel.product (as set.universe (Relation Int)) (as set.universe (Relation Int)))))
(assert (set.subset parent1 (rel.product Person Person)))
(assert (set.subset parent2 (rel.product Person Person)))
(assert (set.subset spouse (rel.product Person Person)))
(assert (forall ((pfunc27893 Atom)) (=> (and (set.member (tuple pfunc27893) Person)) (= (set.card (rel.join (set.singleton (tuple pfunc27893)) spouse)) 1))))
(assert (forall ((pfunc27892 Atom)) (=> (and (set.member (tuple pfunc27892) Person)) (= (set.card (rel.join (set.singleton (tuple pfunc27892)) parent2)) 1))))
(assert (forall ((pfunc27891 Atom)) (=> (and (set.member (tuple pfunc27891) Person)) (= (set.card (rel.join (set.singleton (tuple pfunc27891)) parent1)) 1))))
(check-sat)
(get-model)