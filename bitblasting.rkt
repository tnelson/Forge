#lang rosette

(require ocelot)
(require "ocelot/nextbutton.rkt")

; The i0 i1 i2 i3 represent indices in a bit vector.
; If an integer contains a particular index, then the bit at that index is 1.
; With four bits, the maximum value is 16 for these integers.
; The maximum int is the relation '((i0) (i1) (i2) (i3))
; The minimum int is the relation '()
(bind-universe U B S (a b c d e b0 b1 i0 i1 i2 i3 i4 i5 i6 i7))

(define ints (declare-relation 1 "ints"))
(define ints-bound (make-exact-bound ints '((i0) (i1) (i2) (i3) (i4) (i5) (i6) (i7))))

(define bvals (declare-relation 1 "bvals"))
(define bvals-bound (make-exact-bound bvals '((b0) (b1))))

(define bxor (declare-relation 3 "bxor"))
(define bxor-bound (make-exact-bound bxor '((b0 b0 b0) (b0 b1 b1) (b1 b0 b1) (b1 b1 b0))))

(define band (declare-relation 3 "band"))
(define band-bound (make-exact-bound band '((b0 b0 b0) (b0 b1 b0) (b1 b0 b0) (b1 b1 b1))))

#|
(define fand-bound (make-exact-bound fand-bound '((b0 b0 b0) (b0 b1 b0) (b1 b0 b0) (b1 b1 b1))))
(define for-bound)
(define fxor0bound)
(define fnot-bound)
|#

(define ints-map (declare-relation 4 "ints-map"))
(define ints-map-bound (make-exact-bound ints-map '((i0 b0 b0 b0)
                                                    (i1 b0 b0 b1)
                                                    (i2 b0 b1 b0)
                                                    (i3 b0 b1 b1)
                                                    (i4 b1 b0 b0)
                                                    (i5 b1 b0 b1)
                                                    (i6 b1 b1 b0)
                                                    (i7 b1 b1 b1))))

(define all-bounds (instantiate-bounds (bounds U (append B (list ints-bound bvals-bound bxor-bound band-bound ints-map-bound)))))


#|
(define (arity r)
  (set ([i ints]) (or (and (= i i0) (= r none)) (= i (plus i1 (arity (join univ r)))))))|#

; Yeah try that, should work.
#|
(define (card r)
  (set ([i ints]) (or (and (= i i0) (= r none)) ; there is some x in r such that (card r - x) + 1 = i
|#

; Get bit i of x (ith least significant bit)
; This is a three bit model. So if i is zero, we join twice from the left.
; If i is 1, we join once from the left, and once from the right.
; And if i is 2, we join twice from the right.
(define (bit i xstart)
  (define (bit-helper leftjoins rightjoins x)
    (case
        
        [(> leftjoins 0) (bit-helper (- leftjoins 1) rightjoins (join univ x))]
      [(> rightjoins 0) (bit-helper leftjoins (- rightjoins 1) (join x univ))]
      [else x]))
  (bit-helper (- 2 i) i xstart))

(-> b0 b1 b0)

(define (bxor-func bit0 bit1)
  (join bit1 (join bit0 bxor)))

(define (band-func bit0 bit1)
  (join bit1 (join bit0 band)))

; returns (sum, carry)
(define (halfadd bit0 bit1)
  (-> (bxor-func bit0 bit1) (band-func bit0 bit1)))


(define (fulladd bit0 bit1 carry)
  (define half1 (halfadd bit0 bit1))
  (define half1-carry (bit 0 half1))
  (define half1-sum (bit 1 half1))

  (define half2 (halfadd half1-sum carry))
  (define half2-carry (bit 0 half2))
  (define half2-sum (bit 1 half2))
  
  (define carry-out (bxor-func half1-carry half2-carry))
  (-> half2-sum carry-out))

; Either I have some notion of carry, or I have half-add return multiple values.
(define (plus xsym ysym)
  (define x (sym-to-int xsym))
  (define y (sym-to-int ysym))
  (define x0 (bit 0 x))
  (define x1 (bit 1 x))
  (define x2 (bit 1 x))
  (define y0 (bit 0 y))
  (define y1 (bit 1 y))
  (define y2 (bit 2 y))
  
  (define t0 (fulladd x0 y0 b0))
  (define t1 (fulladd x1 y1 (bit 0 t0)))
  (define t2 (fulladd x0 y0 (bit 0 t1)))

  (int-to-sym (-> (bit 1 t0) (bit 1 t1) (bit 1 t2))))


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
  (set ([i ints]) (= (sym-to-int i) tup)))

(define constraints [= (bit 0 (sym-to-int i2)) b0])

;(define constraints [= (sym-to-int i2) (-> b0 b1 b0)])
(get-model constraints all-bounds S)
