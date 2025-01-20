(set-logic ALL)
(set-option :produce-models true)
(set-option :incremental true)
(set-option :finite-model-find true)
(set-option :sets-ext true)
(declare-sort Atom 0)
(declare-sort UnaryIntTup 0)
(declare-sort BinaryIntTup 0)
(declare-sort TernaryIntTup 0)
(declare-fun value_of_unaryIntTup (UnaryIntTup) (Tuple Int))
(declare-fun value_of_binaryIntTup (BinaryIntTup) (Tuple Int Int))
(declare-fun value_of_ternaryIntTup (TernaryIntTup) (Tuple Int Int Int))
(declare-fun atomNone () (Set (Tuple Atom)))
(declare-fun atomUniv () (Set (Tuple Atom)))
(declare-fun atomIden () (Set (Tuple Atom Atom)))
(declare-fun this_Person () (Set (Tuple Atom)))
(declare-fun this_Man () (Set (Tuple Atom)))
(declare-fun this_Woman () (Set (Tuple Atom)))
(declare-fun this_Person_father () (Set (Tuple Atom Atom)))
(declare-fun this_Person_mother () (Set (Tuple Atom Atom)))
(declare-fun this_Man_wife () (Set (Tuple Atom Atom)))
(declare-fun this_Woman_husband () (Set (Tuple Atom Atom)))
(define-fun this_grandpas ((p (Set (Tuple Atom)))) (Set (Tuple Atom)) 
(let ((parent (union (union (union this_Person_mother this_Person_father) (join this_Person_father this_Man_wife)) (join this_Person_mother this_Woman_husband)))) (intersection (join (join p parent) parent) this_Man)))
(define-fun this_ownGrandpa ((p (Set (Tuple Atom)))) Bool 
(subset p (this_grandpas p)))
(define-fun this_SocialConvention1 () Bool 
(= (intersection (union this_Man_wife this_Woman_husband) (tclosure (union this_Person_mother this_Person_father))) (as emptyset (Set (Tuple Atom Atom)))))
(define-fun this_SocialConvention2 () Bool 
(let ((parent (union this_Person_mother this_Person_father))) (and (forall ((m Atom)) (=> (and true (member (mkTuple m) this_Man)) (not (and (exists ((_x13 Atom)) (member (mkTuple _x13) (join (singleton (mkTuple m)) this_Man_wife))) (subset (join (singleton (mkTuple m)) this_Man_wife) (join (join (singleton (mkTuple m)) (union (tclosure parent) atomIden)) this_Person_mother)))))) (forall ((w Atom)) (=> (and true (member (mkTuple w) this_Woman)) (not (and (exists ((_x14 Atom)) (member (mkTuple _x14) (join (singleton (mkTuple w)) this_Woman_husband))) (subset (join (singleton (mkTuple w)) this_Woman_husband) (join (join (singleton (mkTuple w)) (union (tclosure parent) atomIden)) this_Person_father)))))))))
(assert (subset this_Man this_Person))
(assert (subset this_Woman this_Person))
(assert (= (intersection this_Man this_Woman) atomNone))
(assert (= this_Person (union this_Man this_Woman)))
(assert (= this_Person atomUniv))
(assert (subset this_Person_father (product this_Person this_Man)))
; Multiplicities constraint
(assert (forall ((_x1 Atom)) (=> (member (mkTuple _x1) this_Person) (or (exists ((_x2 Atom)) (and (and (member (mkTuple _x2) this_Man) (member (mkTuple _x1 _x2) this_Person_father)) (forall ((_x3 Atom)) (=> (and (member (mkTuple _x3) this_Man) (distinct _x2 _x3)) (not (member (mkTuple _x1 _x3) this_Person_father)))))) (forall ((_x2 Atom)) (=> (and (member (mkTuple _x2) this_Man) (member (mkTuple _x1 _x2) this_Person_father)) (not (member (mkTuple _x1 _x2) this_Person_father))))))))
(assert (subset this_Person_mother (product this_Person this_Woman)))
; Multiplicities constraint
(assert (forall ((_x4 Atom)) (=> (member (mkTuple _x4) this_Person) (or (exists ((_x5 Atom)) (and (and (member (mkTuple _x5) this_Woman) (member (mkTuple _x4 _x5) this_Person_mother)) (forall ((_x6 Atom)) (=> (and (member (mkTuple _x6) this_Woman) (distinct _x5 _x6)) (not (member (mkTuple _x4 _x6) this_Person_mother)))))) (forall ((_x5 Atom)) (=> (and (member (mkTuple _x5) this_Woman) (member (mkTuple _x4 _x5) this_Person_mother)) (not (member (mkTuple _x4 _x5) this_Person_mother))))))))
(assert (subset this_Man_wife (product this_Man this_Woman)))
; Multiplicities constraint
(assert (forall ((_x7 Atom)) (=> (member (mkTuple _x7) this_Man) (or (exists ((_x8 Atom)) (and (and (member (mkTuple _x8) this_Woman) (member (mkTuple _x7 _x8) this_Man_wife)) (forall ((_x9 Atom)) (=> (and (member (mkTuple _x9) this_Woman) (distinct _x8 _x9)) (not (member (mkTuple _x7 _x9) this_Man_wife)))))) (forall ((_x8 Atom)) (=> (and (member (mkTuple _x8) this_Woman) (member (mkTuple _x7 _x8) this_Man_wife)) (not (member (mkTuple _x7 _x8) this_Man_wife))))))))
(assert (subset this_Woman_husband (product this_Woman this_Man)))
; Multiplicities constraint
(assert (forall ((_x10 Atom)) (=> (member (mkTuple _x10) this_Woman) (or (exists ((_x11 Atom)) (and (and (member (mkTuple _x11) this_Man) (member (mkTuple _x10 _x11) this_Woman_husband)) (forall ((_x12 Atom)) (=> (and (member (mkTuple _x12) this_Man) (distinct _x11 _x12)) (not (member (mkTuple _x10 _x12) this_Woman_husband)))))) (forall ((_x11 Atom)) (=> (and (member (mkTuple _x11) this_Man) (member (mkTuple _x10 _x11) this_Woman_husband)) (not (member (mkTuple _x10 _x11) this_Woman_husband))))))))
; Biology
(assert (forall ((p Atom)) (=> (and true (member (mkTuple p) this_Person)) (not (subset (singleton (mkTuple p)) (join (singleton (mkTuple p)) (tclosure (union this_Person_mother this_Person_father))))))))
; Terminology
(assert (= this_Man_wife (transpose this_Woman_husband)))
; SocialConvention
(assert (= (intersection (union this_Man_wife this_Woman_husband) (tclosure (union this_Person_mother this_Person_father))) (as emptyset (Set (Tuple Atom Atom)))))
; Empty unary relation definition for Atom
(assert (= atomNone (as emptyset (Set (Tuple Atom)))))
; Universe definition for Atom
(assert (= atomUniv (as univset (Set (Tuple Atom)))))
; Identity relation definition for Atom
(assert (forall ((_a1 Atom)(_a2 Atom)) (=> (and (member (mkTuple _a1) atomUniv) (member (mkTuple _a2) atomUniv)) (= (member (mkTuple _a1 _a2) atomIden) (= _a1 _a2)))))

(push)
; NoSelfFather
(assert (and (and (and (forall ((p Atom)) (=> (and true (member (mkTuple p) this_Person)) (not (subset (singleton (mkTuple p)) (join (singleton (mkTuple p)) (tclosure (union this_Person_mother this_Person_father))))))) (= this_Man_wife (transpose this_Woman_husband))) (= (intersection (union this_Man_wife this_Woman_husband) (tclosure (union this_Person_mother this_Person_father))) (as emptyset (Set (Tuple Atom Atom))))) (not (forall ((m Atom)) (=> (and true (member (mkTuple m) this_Man)) (not (= (singleton (mkTuple m)) (join (singleton (mkTuple m)) this_Person_father))))))))
(check-sat)
(get-model)
(pop)

(push)
; ownGrandpa
(assert (and (and (and (forall ((p Atom)) (=> (and true (member (mkTuple p) this_Person)) (not (subset (singleton (mkTuple p)) (join (singleton (mkTuple p)) (tclosure (union this_Person_mother this_Person_father))))))) (= this_Man_wife (transpose this_Woman_husband))) (= (intersection (union this_Man_wife this_Woman_husband) (tclosure (union this_Person_mother this_Person_father))) (as emptyset (Set (Tuple Atom Atom))))) (exists ((p Atom)) (and (and true (member (mkTuple p) this_Person)) (subset (singleton (mkTuple p)) (this_grandpas (singleton (mkTuple p))))))))
(check-sat)
(get-model)
(pop)

(push)
; Same
(assert (and (and (and (forall ((p Atom)) (=> (and true (member (mkTuple p) this_Person)) (not (subset (singleton (mkTuple p)) (join (singleton (mkTuple p)) (tclosure (union this_Person_mother this_Person_father))))))) (= this_Man_wife (transpose this_Woman_husband))) (= (intersection (union this_Man_wife this_Woman_husband) (tclosure (union this_Person_mother this_Person_father))) (as emptyset (Set (Tuple Atom Atom))))) (not (= this_SocialConvention1 this_SocialConvention2))))
(check-sat)
(get-model)
(pop)

