; NOTE: CVC5's interactive mode will not properly handle multi-line commands. Thus, one command per line.
; https://github.com/cvc5/cvc5/issues/10414

; sample program to test translating grandpa.frg into smtlib2
(set-logic ALL)

(set-option :produce-models true)
; we need finite model finding to answer sat problems with universal
; quantified formulas
(set-option :finite-model-find true)

(declare-sort Person 0)

; force there to be at least 4 people
(declare-const person1 Person)
(declare-const person2 Person)
(declare-const person3 Person)
(declare-const person4 Person)
(assert (distinct person1 person2 person3 person4))

(declare-fun parent1 () (Relation Person Person))
(declare-fun parent2 () (Relation Person Person))
(declare-fun spouse () (Relation Person Person))

;(declare-const viewtclosure (Relation Person Person))
;(assert (= viewtclosure (rel.tclosure (set.union parent1 parent2))))
;(declare-const jointotclosure (Relation Person))
;(assert (= jointotclosure (rel.join (set.singleton (tuple person2)) viewtclosure)))
;(declare-const checking Bool)
;(assert (= checking (set.subset jointotclosure spousejointotclosure)))

; if we wanted a relation to not be empty, we could do it this way:
; (assert (not (= parent1 (as set.empty (Relation Person Person)))))

; adding lone constraints to fields
(assert (forall ((x Person)) (forall ((y1 Person) (y2 Person)) (=> (and (set.member (tuple x y1) parent1) (set.member (tuple x y2) parent1)) (= y1 y2)))))
(assert (forall ((x Person)) (forall ((y1 Person) (y2 Person)) (=> (and (set.member (tuple x y1) parent2)  (set.member (tuple x y2) parent2)) (= y1 y2)))))
(assert (forall ((x Person)) (forall ((y1 Person) (y2 Person)) (=> (and (set.member (tuple x y1) spouse)  (set.member (tuple x y2) spouse)) (= y1 y2)))))

(define-fun FamilyFact () Bool (and (forall ((p Person)) (and (not (set.member (tuple p p) spouse)) (not (set.subset (set.singleton (tuple p)) (rel.join (set.singleton (tuple p)) (rel.tclosure (set.union parent1 parent2))))) (=> (not (= (as set.empty (Relation Person)) (rel.join (set.singleton (tuple p)) spouse))) (= (rel.join (rel.join (set.singleton (tuple p)) spouse) spouse) (set.singleton (tuple p)))) (not (set.subset (rel.join (set.singleton (tuple p)) spouse) (rel.join (set.singleton (tuple p)) (rel.tclosure (set.union parent1 parent2)))))))    (forall ((p1 Person) (p2 Person))    (=> (distinct p1 p2)      (=> (set.subset (set.singleton (tuple p2)) (rel.join (set.singleton (tuple p1)) (rel.tclosure (set.union parent1 parent2))))            (not (set.subset (set.singleton (tuple p2)) (rel.join (rel.join (set.singleton (tuple p1)) spouse) (rel.tclosure (set.union parent1 parent2)))))))    )    (forall ((p1 Person) (p2 Person))      (=> (distinct p1 p2)      (=> (or (set.subset (set.singleton (tuple p2)) (rel.join (rel.join (set.singleton (tuple p1)) parent1) (rel.tclosure (set.union parent1 parent2))))              (= (set.singleton (tuple p2)) (rel.join (set.singleton (tuple p1)) parent1)))        (not (or (set.subset (set.singleton (tuple p2)) (rel.join (rel.join (set.singleton (tuple p1)) parent2) (rel.tclosure (set.union parent1 parent2))))              (= (set.singleton (tuple p2)) (rel.join (set.singleton (tuple p1)) parent2))))))    )  ))
(define-fun ownGrandparent () Bool   (exists ((p Person))    (or       (= (rel.join (rel.join (rel.join (rel.join (set.singleton (tuple p)) parent1) spouse) parent1) spouse) (set.singleton (tuple p)))      (= (rel.join (rel.join (rel.join (rel.join (set.singleton (tuple p)) parent2) spouse) parent2) spouse) (set.singleton (tuple p)))      (= (rel.join (rel.join (rel.join (rel.join (set.singleton (tuple p)) parent1) spouse) parent2) spouse) (set.singleton (tuple p)))      (= (rel.join (rel.join (rel.join (rel.join (set.singleton (tuple p)) parent2) spouse) parent1) spouse) (set.singleton (tuple p)))    )  ))

(assert FamilyFact)
(assert ownGrandparent)
;(check-sat)
;(get-model)
