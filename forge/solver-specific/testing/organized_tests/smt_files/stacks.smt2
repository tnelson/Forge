(reset)
(define-fun sign ((x__sign Int)) Int (ite (< x__sign 0) -1 (ite (> x__sign 0) 1 0)))
(define-fun reconcile-int ((aset (Relation Int))) Int (ite (= (as set.empty (Relation Int)) aset) 0 ((_ tuple.select 0) (set.choose aset))))
(set-logic ALL)
(declare-sort Atom 0)


(declare-const Initial0 Atom)
 (declare-const Mid0 Atom)
 (declare-const End0 Atom)
 (declare-const State0 Atom)

(declare-fun State () (Relation Atom))
(assert (set.member (tuple Initial0) State))
 (assert (set.member (tuple Mid0) State))
 (assert (set.member (tuple End0) State))
 (assert (set.member (tuple State0) State))

(assert (= State (set.union (set.singleton (tuple Initial0)) (set.singleton (tuple Mid0)) (set.singleton (tuple End0)) (set.singleton (tuple State0)))))

(declare-fun Initial () (Relation Atom))
(assert (set.member (tuple Initial0) Initial))

(assert (= Initial (set.singleton (tuple Initial0))))

(declare-fun Mid () (Relation Atom))
(assert (set.member (tuple Mid0) Mid))

(assert (= Mid (set.singleton (tuple Mid0))))

(declare-fun End () (Relation Atom))
(assert (set.member (tuple End0) End))

(assert (= End (set.singleton (tuple End0))))


(declare-const StackElement0 Atom)
 (declare-const StackElement1 Atom)
 (declare-const StackElement2 Atom)
 (declare-const StackElement3 Atom)

(declare-fun StackElement () (Relation Atom))
(assert (set.member (tuple StackElement0) StackElement))
 (assert (set.member (tuple StackElement1) StackElement))
 (assert (set.member (tuple StackElement2) StackElement))
 (assert (set.member (tuple StackElement3) StackElement))

(assert (= StackElement (set.union (set.singleton (tuple StackElement0)) (set.singleton (tuple StackElement1)) (set.singleton (tuple StackElement2)) (set.singleton (tuple StackElement3)))))


(declare-fun succ () (Relation Int Int))

(declare-fun top () (Relation Atom Atom))

(declare-fun prev () (Relation Atom Atom))

(assert (= State (set.union Initial Mid End)))

(assert (= (set.inter Initial Mid) (as set.empty (Relation Atom))))
(assert (= (set.inter Initial End) (as set.empty (Relation Atom))))
(assert (= (set.inter Mid Initial) (as set.empty (Relation Atom))))

(assert (= (set.inter Mid End) (as set.empty (Relation Atom))))
(assert (= (set.inter End Initial) (as set.empty (Relation Atom))))
(assert (= (set.inter End Mid) (as set.empty (Relation Atom))))

(assert (= (set.inter StackElement State) (as set.empty (Relation Atom))))
(assert (= (set.inter State StackElement) (as set.empty (Relation Atom))))

(assert (and (and (forall ((s Atom)) (=> (and (set.member (tuple s) StackElement)) (and (or (not (set.subset (set.singleton (tuple s)) (rel.join Initial (rel.tclosure (set.union top prev))))) (not (set.subset (set.singleton (tuple s)) (rel.join (set.singleton (tuple s)) (rel.tclosure prev)))))))) (forall ((s Atom)) (=> (and (set.member (tuple s) StackElement)) (and (exists ((st Atom)) (and (and (set.member (tuple st) State)) (and (set.subset (set.singleton (tuple s)) (rel.join (set.singleton (tuple st)) (rel.tclosure (set.union top prev)))))))))))))
(assert (and (and (not (= (as set.empty (Relation Atom)) (rel.join Initial top))) (= (rel.join Mid top) (rel.join (rel.join Initial top) prev)))))
(assert (and (and (not (= (as set.empty (Relation Atom)) (rel.join Mid top))) (= (rel.join End top) (rel.join (rel.join Mid top) prev)))))
(assert (set.subset succ (rel.product (as set.universe (Relation Int)) (as set.universe (Relation Int)))))
(assert (set.subset top (rel.product State StackElement)))
(assert (set.subset prev (rel.product StackElement StackElement)))
(assert (= State (set.union Initial Mid End)))
(assert true)
(assert true)
(assert true)
(assert (forall ((pfunc19730 Atom)) (=> (and (set.member (tuple pfunc19730) State)) (or (or (= (rel.join (set.singleton (tuple pfunc19730)) top) (set.singleton (tuple StackElement0))) (= (rel.join (set.singleton (tuple pfunc19730)) top) (set.singleton (tuple StackElement1))) (= (rel.join (set.singleton (tuple pfunc19730)) top) (set.singleton (tuple StackElement2))) (= (rel.join (set.singleton (tuple pfunc19730)) top) (set.singleton (tuple StackElement3)))) (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple pfunc19730)) top))))))
(assert (forall ((pfunc19731 Atom)) (=> (and (set.member (tuple pfunc19731) StackElement)) (or (or (= (rel.join (set.singleton (tuple pfunc19731)) prev) (set.singleton (tuple StackElement0))) (= (rel.join (set.singleton (tuple pfunc19731)) prev) (set.singleton (tuple StackElement1))) (= (rel.join (set.singleton (tuple pfunc19731)) prev) (set.singleton (tuple StackElement2))) (= (rel.join (set.singleton (tuple pfunc19731)) prev) (set.singleton (tuple StackElement3)))) (= (as set.empty (Relation Atom)) (rel.join (set.singleton (tuple pfunc19731)) prev))))))
(check-sat)
(get-model)