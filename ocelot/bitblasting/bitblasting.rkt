#lang rosette

(require ocelot)
(require "ocelot/nextbutton.rkt")
;(require "bitblastlib.rkt")

; The i0 i1 i2 i3 represent indices in a bit vector.
; If an integer contains a particular index, then the bit at that index is 1.
; With four bits, the maximum value is 16 for these integers.
; The maximum int is the relation '((i0) (i1) (i2) (i3))
; The minimum int is the relation '()
(bind-universe U B S (b0 b1 i0 i1 i2 i3 i4 i5 i6 i7))


(define ints (declare-relation 1 "ints"))
(define ints-bound (make-exact-bound ints '((i0) (i1) (i2) (i3) (i4) (i5) (i6) (i7))))

(define bvals (declare-relation 1 "bvals"))
(define bvals-bound (make-exact-bound bvals '((b0) (b1))))

(define bxor (declare-relation 3 "bxor"))
(define bxor-bound (make-exact-bound bxor '((b0 b0 b0) (b0 b1 b1) (b1 b0 b1) (b1 b1 b0))))

(define band (declare-relation 3 "band"))
(define band-bound (make-exact-bound band '((b0 b0 b0) (b0 b1 b0) (b1 b0 b0) (b1 b1 b1))))

(define bor (declare-relation 3 "bor"))
(define bor-bound (make-exact-bound bor '((b0 b0 b0) (b0 b1 b1) (b1 b0 b1) (b1 b1 b1))))

(define ints-map (declare-relation 4 "ints-map"))
(define ints-map-bound (make-exact-bound ints-map '((i0 b0 b0 b0)
                                                    (i1 b0 b0 b1)
                                                    (i2 b0 b1 b0)
                                                    (i3 b0 b1 b1)
                                                    (i4 b1 b0 b0)
                                                    (i5 b1 b0 b1)
                                                    (i6 b1 b1 b0)
                                                    (i7 b1 b1 b1))))

; Not: (b0 b1) - x
; And: (b1) - x - y?
; need Or, And, either one.
; I think I can only use &, +, -
; WAIT, what if I reformulate again to make false absence, and truth presence?

; OK, make this faster (boolean op tricks? new map formulation?) and try to make cardinality.


; new formulation, return values, take out extraneous atoms
; relation branch cardinality?
; sum across tuple?
; Try the new formulation first, that's promising.
;

; look for number of primary variables. should be zero. all time spent in translation
; dont treat ocelot as blackbox
; see what i can print out by hacking ocelot, to find bottleneck

; primary variable for every tuple / relation symbol for every tupel that may or not be there.
; not for things that are in the lower bound, or are not in upper bound.

;secondary variables are things for anciliary reasons for converting to cnf, may be metadata stuff.
; amine linear blowup tseitin formylation, tsytin.
; ocelot probably uses tseytin

; ocelot pipeline, probably: sparse matrices?

; boolean circuit to z3. or tseytin to CNF to SAT solver.
; How to get to boolean circuit?
; Alloy/Forge goes to Ocelot formula/bounds.
; That goes to Boolean formula = Boolean circuit. How? formula/bounds become primary variabels, then sparse matrices.
; Goes through sparse matrices. What are they? mysterious. research

; emina does talk about sparse matrices, her thesis was basically kodkod.
; also did cool work on cores, and detecting symmetries. what are hte permutation of the graphs?
; work with solver that understands those. Just farm off to solver.
; OR convert symmetries to boolean symmetries, add boolean constraint to the formula that excludes anything that's not smallest model,
; by number conversion of bit vector as boolean integer.

; have to partition into equivalence classes. only want one model from each class, ideally, but that/s too expensive.
; but requirigin that is super hard, much harder than original sat probleM? reserach.
; want to rule out as many models as possible, but don't ever want to disallow an entire equivalence class.
; so we ensure that the lexicographically least model from each class is left in, as many as possible of others are left out.

; it's NP-hard to produce any expression of lex leadership without reordering variables.

;(define result1 (declare-relation 1 "result1"))
;(define result1-bound (make-upper-bound result1  '((i0) (i1) (i2) (i3) (i4) (i5) (i6) (i7))))

#|(define result2 (declare-relation 1 "result2"))
(define result2-bound (make-upper-bound result2  '((i0) (i1) (i2) (i3) (i4) (i5) (i6) (i7))))

(define result3 (declare-relation 1 "result3"))
(define result3-bound (make-upper-bound result3  '((i0) (i1) (i2) (i3) (i4) (i5) (i6) (i7))))

(define result4 (declare-relation 1 "result4"))
(define result4-bound (make-upper-bound result4  '((i0) (i1) (i2) (i3) (i4) (i5) (i6) (i7))))

(define result5 (declare-relation 1 "result5"))
(define result5-bound (make-upper-bound result5 '((i0) (i1) (i2) (i3) (i4) (i5) (i6) (i7))))|#

(define all-bounds (instantiate-bounds (bounds U (append B (list ints-bound bvals-bound bxor-bound band-bound bor-bound ints-map-bound)))))
;result1-bound))))); result2-bound result3-bound result4-bound result5-bound)))))

(define (bit i xstart)
  (define (bit-helper leftjoins rightjoins x)
    (cond
      [(> leftjoins 0) (bit-helper (- leftjoins 1) rightjoins (join univ x))]
      [(> rightjoins 0) (bit-helper leftjoins (- rightjoins 1) (join x univ))]
      [else x]))
  (bit-helper (- 2 i) i xstart))


; Replace these with functions that don't use the relations?
; How?
(define (bxor-func bit0 bit1)
  (join bit1 (join bit0 bxor)))

(define (band-func bit0 bit1)
  (join bit1 (join bit0 band)))

(define (bor-func bit0 bit1)
  (join bit1 (join bit0 bor)))

; returns (sum, carry)
(define (halfadd bit0 bit1)
  (-> (bxor-func bit0 bit1) (band-func bit0 bit1)))


; I think this one is wrong.
(define (fulladd bit0 bit1 carry)
  (define half1 (halfadd bit0 bit1))
  (define half1-carry (join univ half1))
  (define half1-sum (join half1 univ))

  (define half2 (halfadd half1-sum carry))
  (define half2-carry (join univ half2))
  (define half2-sum (join half2 univ))
  
  (define carry-out (bor-func half1-carry half2-carry))
  (-> half2-sum carry-out))

; Either I have some notion of carry, or I have half-add return multiple values.
(define (plus xsym ysym)
  (define x (sym-to-int xsym))
  (define y (sym-to-int ysym))
  (define x0 (bit 0 x))
  (define x1 (bit 1 x))
  (define x2 (bit 2 x))
  (define y0 (bit 0 y))
  (define y1 (bit 1 y))
  (define y2 (bit 2 y))
  ;(int-to-sym (-> x0 y1 x2)))
  
  (define t0 (fulladd x0 y0 b0))
  (define t1 (fulladd x1 y1 (join univ t0)))
  (define t2 (fulladd x2 y2 (join univ t1)))

  (int-to-sym (-> (join t2 univ) (join t1 univ) (join t0 univ) )))


#|(define (mult x y)
    (plus
     (mult x (bit 0 y))  
     (shiftleft 1 (mult x (bit 1 y)))
     (shiftleft 2 (mult x (bit 2 y)))|#


; x is an int atom, like i0 or i5
; uses the ints-map to return a singleton set containing the tuple (bval bval)
(define (sym-to-int x)
  (join x ints-map))

(define (int-to-sym tup)
  (join (join (join (& ints-map (-> ints tup)) univ) univ) univ))
;(set ([i ints]) (= (sym-to-int i) tup)))

#|
(define (arity r)
  (set ([i ints]) (or (and (= i i0) (= r none)) (= i (plus i1 (arity (join univ r)))))))|#

; Yeah try that, should work.
; The greatest numer this can return is 7, anyway. So put a depth limit on it.
(define (card r)
  (define (card-helper r depth)
    (case depth
      ([2] i0)
      (else (set ([i ints]) (or (and (no r) (= i i0))
                                (and (some r) (some ([x r]) (= i (plus i1 (card-helper (- r x) (+ depth 1))))))))))); there is some x in r such that (card r - x) + 1 = i
  (card-helper r 0))

;(define (card r)
;  (
; What if i set aside a special atom and add it to every tuple, so something special happens if its the empty relation?
;I need to be able to branch on the empty relation.

; comprehensions are expensive. I can't use comprehensions. none.
   ; maybe the other operators though.
   ; So how do I choose between zero and 1?
   ; a relation. How do I do that?
  
; How do I do without?


(define constraints (and 
                     [= (plus i0 i0) i0]
                     [= (plus i0 i1) i1]
                     [= (plus i0 i2) i2]
                     [= (plus i0 i3) i3]
                     [= (plus i0 i4) i4]
                     [= (plus i0 i5) i5]
                     [= (plus i0 i6) i6]
                     [= (plus i0 i7) i7]
                         
                     [= (plus i1 i0) i1]
                     [= (plus i1 i1) i2]
                     [= (plus i1 i2) i3]
                     [= (plus i1 i3) i4]
                     [= (plus i1 i4) i5]
                     [= (plus i1 i5) i6]
                     [= (plus i1 i6) i7]
                     [= (plus i1 i7) i0]

                     [= (plus i2 i0) i2]
                     [= (plus i2 i1) i3]
                     [= (plus i2 i2) i4]
                     [= (plus i2 i3) i5]
                     [= (plus i2 i4) i6]
                     [= (plus i2 i5) i7]
                     [= (plus i2 i6) i0]
                     [= (plus i2 i7) i1]

                     [= (plus i3 i0) i3]
                     [= (plus i3 i1) i4]
                     [= (plus i3 i2) i5]
                     [= (plus i3 i3) i6]
                     [= (plus i3 i4) i7]
                     [= (plus i3 i5) i0]
                     [= (plus i3 i6) i1]
                     [= (plus i3 i7) i2]
                         
                     [= (plus i4 i0) i4]
                     [= (plus i4 i1) i5]
                     [= (plus i4 i2) i6]
                     [= (plus i4 i3) i7]
                     [= (plus i4 i4) i0]
                     [= (plus i4 i5) i1]
                     [= (plus i4 i6) i2]
                     [= (plus i4 i7) i3]

                     [= (plus i5 i0) i5]
                     [= (plus i5 i1) i6]
                     [= (plus i5 i2) i7]
                     [= (plus i5 i3) i0]
                     [= (plus i5 i4) i1]
                     [= (plus i5 i5) i2]
                     [= (plus i5 i6) i3]
                     [= (plus i5 i7) i4]
                     
                     [= (plus i6 i0) i6]
                     [= (plus i6 i1) i7]
                     [= (plus i6 i2) i0]
                     [= (plus i6 i3) i1]
                     [= (plus i6 i4) i2]
                     [= (plus i6 i5) i3]
                     [= (plus i6 i6) i4]
                     [= (plus i6 i7) i5]

                     [= (plus i7 i0) i7]
                     [= (plus i7 i1) i0]
                     [= (plus i7 i2) i1]
                     [= (plus i7 i3) i2]
                     [= (plus i7 i4) i3]
                     [= (plus i7 i5) i4]
                     [= (plus i7 i6) i5]
                     [= (plus i7 i7) i6]))


(println "yooooooo")
(get-model constraints all-bounds S)
;(card bvals)
;(fulladd i0 i0 i0)
;(plus i0 i0)
;(card bvals)


; Hrm. relational branching in card might atually owrk.