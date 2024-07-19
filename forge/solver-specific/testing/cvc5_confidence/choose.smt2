; Adapted by Tim from a CVC5 regression test. 
; Let's make sure "set.choose" works as we expect. We'll also move into
; the theory of _relations_ so we can spot any issues that might not appear
; in just the theory of sets. 

; From cvc5_kind.h:
;   * Select an element from a given set. For a set :math:`A = \{x\}`, the term
;   * (set.choose :math:`A`) is equivalent to the term :math:`x_1`. For an empty
;   * set, it is an arbitrary value. For a set with cardinality > 1, it will
;   * deterministically return an element in :math:`A`.
  

(set-logic ALL)
(set-option :sets-ext true)
(set-option :produce-models true)

; lets us use push and pop
(set-option :incremental true) 

; find ("small") models for recursively-defined functions; assumes they are admissible.
; Definition of admissible: https://homepage.divms.uiowa.edu/~ajreynol/pres-ijcar16.pdf
;   Note: Admissible is a _superset_ of Terminating.

(set-option :fmf-fun true) 

(declare-fun S () (Relation Int))
(declare-fun x () Int)
(declare-fun y () Int)
(assert (not (= x y)))

(declare-sort Atom 0)
(declare-fun age () (Relation Atom Int))
(declare-fun Alice () Atom)

; Unlike in the regression test, we won't say that S is a singleton. 

; Can choose something from S and get x
(assert (= (tuple x) (set.choose S)))
(set-info :status sat)
(check-sat) 
(push)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Can choose something from S and get y (!= x)
; Expect unsat; "choose" is *deterministic* on sets of cardinality >1
(assert (= (tuple y) (set.choose S)))
(set-info :status unsat)
(check-sat) 
(pop)
(push)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; If S is empty, get an arbitrary element. Is this deterministic? 
(assert (= (as set.empty (Relation Int)) S))
(assert (= (tuple x) (set.choose S)))
(assert (= (tuple y) (set.choose S)))
(set-info :status unsat) ; it seems so. 
(check-sat) 
(pop)
(push)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Because of this determinism, "choose" has an "always =" semantics, not 
; a "may =" semantics. Let's see if we can define a function that recurs 
; through the contents of a set. Note that this requires an option (above)
; and for the function to be admissible.

(define-fun-rec card/int-1 ((aset (Relation Int))) Int
  (ite (= (as set.empty (Relation Int)) aset) 
       0
       (+ 1 (card/int-1 (set.minus aset (set.singleton (set.choose aset)))))))

(assert (= (card/int-1 S) 2)) 
(check-sat) ; doesn't stop w/ current options
(pop) 
(push)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Can we redefine our own filter?
;; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Can we use set.filter (without HO_ALL logic) if we don't use lambdas?
;; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Can we use set.choose and (_ tuple.select i) to extract an int 
; from its enclosing relational context? Assume that the relation is 
; a singleton.

(assert (set.is_singleton S))
(assert (> ((_ tuple.select 0) (set.choose S)) 5))
(set-info :status sat) 
(check-sat)

; The above is well-typed. Let's try it on something more complex, like
; a "field access"-style relational join.

(assert (> (+ 1 ((_ tuple.select 0) (set.choose (rel.join (set.singleton (tuple Alice)) age)))) 5))
(set-info :status sat) 
(check-sat)

(pop)
(push)

; Can we use a helper function to impose Alloy semantics for the empty set as an Int?

(define-fun reconcile-int ((aset (Relation Int))) Int
  (ite (= (as set.empty (Relation Int)) aset) 
       0
       ((_ tuple.select 0) (set.choose aset))))

(assert (> (reconcile-int (rel.join (set.singleton (tuple Alice)) age)) 5))
(set-info :status sat) 
(check-sat)

(pop)
(push)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
