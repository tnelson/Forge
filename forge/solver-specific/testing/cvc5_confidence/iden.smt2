(set-logic ALL)
(set-option :sets-ext true)
(set-option :produce-models true)
(set-option :incremental true) 

(declare-sort Atom 0)
(declare-fun x () Atom)
(declare-fun i () Int)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; IMPORTANT WARNING, READ THIS:
; Note: this definition of x won't force x to be in (as set.universe (Relation Int))
; by itself, unless we define a relation for it to occupy or declare it as a relation.
; Forge should be OK and not experience this issue if every Atom is in a sig 
; relation, but it is a hazard for (at least) Int-valued Skolem functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare-fun Person () (Relation Atom))
(assert (set.member (tuple x) Person))

; Define univ and iden for Atoms
(declare-fun univAtom () (Relation Atom))
(assert (= univAtom (as set.universe (Relation Atom))))
(declare-fun idenAtom () (Relation Atom Atom))
(assert (= idenAtom (rel.iden univAtom)))

; Define univ and iden for Ints
(declare-fun univInt () (Relation Int))
(assert (= univInt (as set.universe (Relation Int))))
(declare-fun idenInt () (Relation Int Int))
(assert (= idenInt (rel.iden univInt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RELATED TO THE ABOVE IMPORTANT WARNING
; Note that for Ints, we need to explicitly add i to the universe,
; (unless we had declared i as a relation)
(assert (set.member (tuple i) univInt))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Confirm the definitions are satisfiable
(set-info :status sat)
(check-sat) 
(push)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Check iden behaves as expected

; Is it ever possible for x not to be in x.iden?
(assert (not (set.member (tuple x)
                         (rel.join (set.singleton (tuple x)) idenAtom))))
(set-info :status unsat)
(check-sat) 
(pop)
(push)
(assert (not (set.member (tuple i)
                         (rel.join (set.singleton (tuple i)) idenInt))))
(set-info :status unsat)
(check-sat) 
(pop)
(push)

; Is it ever possible for x not to be in iden.x?
(assert (not (set.member (tuple x)
                         (rel.join idenAtom (set.singleton (tuple x))))))
(set-info :status unsat)
(check-sat) 
(push)
(pop)
(assert (not (set.member (tuple i)
                         (rel.join idenInt (set.singleton (tuple i))))))
(set-info :status unsat)
(check-sat) 
(push)
(pop)