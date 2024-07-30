(reset)

(set-logic ALL)
(set-option :sets-ext true)
(set-option :produce-models true)
(set-option :produce-difficulty true)
(set-option :mbqi true)

(declare-sort Atom 0)

(declare-fun Node () (Relation Atom))
(declare-fun edges () (Relation Atom Atom Int))

(assert 
  (forall ((a1 Atom) (a2 Atom) (w Int)) 
    (=> (set.member (tuple a1 a2 w) edges) 
             (and (set.member (tuple a1) Node)
                  (set.member (tuple a2) Node)))))

(assert 
  (exists ((x1 Atom) (x2 Atom))
    (and (not (= x1 x2))
         (not (exists ((w Int)) (set.member (tuple x1 x2 w) edges)))
         (not (exists ((w Int)) (set.member (tuple x2 x1 w) edges)))
         (set.member (tuple x1) (rel.join (set.singleton (tuple x2)) (rel.tclosure (rel.join edges (as set.universe (Relation Int)))))))))


(check-sat)
(get-model)