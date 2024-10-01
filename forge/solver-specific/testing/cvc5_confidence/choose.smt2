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
(declare-fun Bob () Atom)

; Don't neglect these constraints, to have a sig relation to put them in.
(assert (set.member (tuple Alice) (as set.universe (Relation Atom))))
(assert (set.member (tuple Bob) (as set.universe (Relation Atom))))

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

; Can we write "sum"?

; **Because choose is always deterministic, the two choose terms should be equal.**
(define-fun-rec int-sum ((aset (Relation Int))) Int
  (ite (= (as set.empty (Relation Int)) aset) 
       0
       (+ (int-sum (set.minus aset (set.singleton (set.choose aset))))
          ((_ tuple.select 0) (set.choose aset)))))

; Note we never said that age was functional in the constraints above. So:
(assert (= (rel.join (set.singleton (tuple Alice)) age) 
           (set.union (set.singleton (tuple 10))
                      (set.singleton (tuple 20)))))
(assert (> (int-sum (rel.join (set.singleton (tuple Alice)) age)) 21))
(set-info :status sat) 
(check-sat)
(assert (not (= (int-sum (rel.join (set.singleton (tuple Alice)) age)) 30)))
(set-info :status unsat) 
(check-sat)

(pop)
(push)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; How does this implementation of sum scale with larger sets, arithmetic, etc.?
; Poorly. Notice the slowdown even at just 7 elements in Alice's age set.

; **Because choose is always deterministic, the two choose terms should be equal.**
(define-fun-rec int-sum ((aset (Relation Int))) Int
  (ite (= (as set.empty (Relation Int)) aset) 
       0
       (+ (int-sum (set.minus aset (set.singleton (set.choose aset))))
          ((_ tuple.select 0) (set.choose aset)))))

(assert (= (rel.join (set.singleton (tuple Alice)) age) 
           (set.union (set.singleton (tuple 10))
                      (set.singleton (tuple 11))
                      (set.singleton (tuple 12))
                      (set.singleton (tuple 13))
                      (set.singleton (tuple 14))
                      (set.singleton (tuple 15))
                      (set.singleton (tuple 16)))))

(assert (> (int-sum (rel.join (set.singleton (tuple Alice)) age)) 21))

(set-info :status sat) 
(check-sat)

(pop)
(push)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; What about the sum "quantifier"? One approach might be to choose-iterate 
; over the domain, and hard-code the expression into the function. This seems
; limited, of course, and likely scales poorly.

; **Because choose is always deterministic, the two choose terms should be equal.**
(define-fun-rec int-sum ((aset (Relation Int))) Int
  (ite (= (as set.empty (Relation Int)) aset) 
       0
       (+ (int-sum (set.minus aset (set.singleton (set.choose aset))))
          ((_ tuple.select 0) (set.choose aset)))))
(define-fun-rec sum-q-dot-age ((domain (Relation Atom))) Int
  (ite (= (as set.empty (Relation Atom)) domain) 
       0
       (+
         ; Choose a domain element and do the join, then sum over its value(s)
         (int-sum (rel.join (set.singleton (set.choose domain)) age))
         ; Recursive call on remainder of domain 
         (sum-q-dot-age (set.minus domain (set.singleton (set.choose domain))))
        )))

; Alice total = 21
(assert (= (rel.join (set.singleton (tuple Alice)) age) 
           (set.union (set.singleton (tuple 10))
                      (set.singleton (tuple 11)))))
; Bob total = 221
(assert (= (rel.join (set.singleton (tuple Bob)) age) 
           (set.union (set.singleton (tuple 110))
                      (set.singleton (tuple 111)))))

; Only two Atoms: Alice and Bob 
(assert (= (as set.universe (Relation Atom))
        (set.union (set.singleton (tuple Alice))
                   (set.singleton (tuple Bob)))))

; Without this, they all go to 0! (Do we need to use the fun to have it available?)
; No, because if we leave this out but include the 2 lines below, all is well for 
; the individual values, but the total for the universe fails.
(assert (not (= (sum-q-dot-age (as set.universe (Relation Atom))) 0)))
(assert (not (= (sum-q-dot-age (set.singleton (tuple Alice)))  0)))
(assert (not (= (sum-q-dot-age (set.singleton (tuple Bob)))  0)))
; So it seems that the term (or some variant thereof?) to be used with get-value 
; must appear in a constraint pre-solving.

(check-sat)
(get-model)

; These look OK
(get-value ((as set.universe (Relation Atom))))
(get-value ((sum-q-dot-age (as set.universe (Relation Atom)))))
(get-value ((int-sum (rel.join (set.singleton (tuple Alice)) age))))
(get-value ((int-sum (rel.join (set.singleton (tuple Bob)) age))))

; These worry me; why are they evaluating to the same thing?
(get-value ((sum-q-dot-age (set.singleton (tuple Alice)))))
(get-value ((sum-q-dot-age (set.singleton (tuple Bob)))))
