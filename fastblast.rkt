#lang rosette

(require ocelot)
(require "ocelot/nextbutton.rkt")

(bind-universe U B S (i0 i1 i2 z0 z1 z2 z3 z4 z5 z6 z7))

(define verum (= none none))
(define falsum (! verum))

(define bv0 (list falsum falsum falsum))
(define bv1 (list falsum falsum verum))
(define bv2 (list falsum verum falsum))
(define bv3 (list falsum verum verum))
(define bv4 (list verum falsum falsum))
(define bv5 (list verum falsum verum))
(define bv6 (list verum verum falsum))
(define bv7 (list verum verum verum))

(define indices (declare-relation 1 "indices"))
(define indices-bounds (make-exact-bound indices '((i0) (i1) (i2))))

#|(define r0 (declare-relation 1 "r0"))
(define r0-bounds (make-exact-bound r0 '((|#

(define ints (declare-relation 1 "ints"))
(define ints-bound (make-exact-bound ints '((z0) (z1) (z2) (z3) (z4) (z5) (z6) (z7))))

(define ints-map (declare-relation 2 "ints-map"))
(define ints-map-bound (make-exact-bound ints-map '((z1 i0)
                                                    (z2 i1)
                                                    (z3 i1) (z3 i0)
                                                    (z4 i2)
                                                    (z5 i2) (z5 i0)
                                                    (z6 i2) (z6 i1)
                                                    (z7 i2) (z7 i1) (z7 i0))))

(define all-bounds (instantiate-bounds (bounds U (append B (list indices-bounds
                                                                 ints-bound ints-map-bound)))))


#|
OK!!! how does this work.
We need to store ints in relations. How is that done? Just with the integer atoms. If they're in a relation,
they are stored as integer atoms. But they can be converted, also, to list form, pretty damn cheaply.
And if need be, they can be converted from list form back to atoms. But when do those things take place?
Diving in may be the only way to go.

It would be nice to have an automatic conversion. OK, so any time we involve ints with relations, we need an automatic conversion to atoms.
And any time we involve ints with arithmetic, or anything besides relations, we need an automatic conversion to lists.
Well, doesn't have to be automatic at first...
|#


(define (iff a b)
  (and (=> a b) (=> b a)))

(define (bv-to-atom bv)
  (define (bv-to-atom-helper b2 b1 b0)
    (set ([z ints]) (and (iff (in (-> z i0) ints-map) b0)
                         (iff (in (-> z i1) ints-map) b1)
                         (iff (in (-> z i2) ints-map) b2))))
  (apply bv-to-atom-helper bv))

(define (atom-to-bv atom)
  (list (in (-> atom i2) ints-map)
        (in (-> atom i1) ints-map)
        (in (-> atom i0) ints-map)))

; returns sum, carry
(define (halfadd b0 b1)
  (define band (and b0 b1))
  (values (and (or b0 b1) (not band))
          band))

; returns sum, carry
(define (fulladd b0 b1 carry)
  (define-values (h1-sum h1-carry) (halfadd b0 b1))
  (define-values (h2-sum h2-carry) (halfadd h1-sum carry))
  
  (define carry-out (or h1-carry h2-carry))
  (values h2-sum carry-out))

; returns atom representing x + y
(define (plus x y)
  (define x0 (third x))
  (define x1 (second x))
  (define x2 (first x))
  (define y0 (third y))
  (define y1 (second y))
  (define y2 (first y))
  
  (define-values (t0-sum t0-carry) (fulladd x0 y0 falsum))
  (define-values (t1-sum t1-carry) (fulladd x1 y1 t0-carry))
  (define-values (t2-sum t2-carry) (fulladd x2 y2 t1-carry))

  (list t2-sum t1-sum t0-sum))

(define (same-bv bva bvb)
  (and (iff (first bva) (first bvb))
       (iff (second bva) (second bvb))
       (iff (third bva) (third bvb))))


(define constraints (and
                     ;[= (bv-to-atom (plus bv0 bv0)) z0]
                     ;[= (bv-to-atom (plus bv0 bv1)) z1]
                     [same-bv (plus bv0 bv0) bv0]
                     [same-bv (plus bv0 bv1) bv1]
                     [same-bv (plus bv0 bv2) bv2]
                     [same-bv (plus bv0 bv3) bv3]
                     [same-bv (plus bv0 bv4) bv4]
                     [same-bv (plus bv0 bv5) bv5]
                     [same-bv (plus bv0 bv6) bv6]
                     [same-bv (plus bv0 bv7) bv7]

                     [same-bv (plus bv1 bv0) bv1]
                     [same-bv (plus bv1 bv1) bv2]
                     [same-bv (plus bv1 bv2) bv3]
                     [same-bv (plus bv1 bv3) bv4]
                     [same-bv (plus bv1 bv4) bv5]
                     [same-bv (plus bv1 bv5) bv6]
                     [same-bv (plus bv1 bv6) bv7]
                     [same-bv (plus bv1 bv7) bv0]

                     [same-bv (plus bv2 bv0) bv2]
                     [same-bv (plus bv2 bv1) bv3]
                     [same-bv (plus bv2 bv2) bv4]
                     [same-bv (plus bv2 bv3) bv5]
                     [same-bv (plus bv2 bv4) bv6]
                     [same-bv (plus bv2 bv5) bv7]
                     [same-bv (plus bv2 bv6) bv0]
                     [same-bv (plus bv2 bv7) bv1]

                     [same-bv (plus bv3 bv0) bv3]
                     [same-bv (plus bv3 bv1) bv4]
                     [same-bv (plus bv3 bv2) bv5]
                     [same-bv (plus bv3 bv3) bv6]
                     [same-bv (plus bv3 bv4) bv7]
                     [same-bv (plus bv3 bv5) bv0]
                     [same-bv (plus bv3 bv6) bv1]
                     [same-bv (plus bv3 bv7) bv2]

                     [same-bv (plus bv4 bv0) bv4]
                     [same-bv (plus bv4 bv1) bv5]
                     [same-bv (plus bv4 bv2) bv6]
                     [same-bv (plus bv4 bv3) bv7]
                     [same-bv (plus bv4 bv4) bv0]
                     [same-bv (plus bv4 bv5) bv1]
                     [same-bv (plus bv4 bv6) bv2]
                     [same-bv (plus bv4 bv7) bv3]

                     [same-bv (plus bv5 bv0) bv5]
                     [same-bv (plus bv5 bv1) bv6]
                     [same-bv (plus bv5 bv2) bv7]
                     [same-bv (plus bv5 bv3) bv0]
                     [same-bv (plus bv5 bv4) bv1]
                     [same-bv (plus bv5 bv5) bv2]
                     [same-bv (plus bv5 bv6) bv3]
                     [same-bv (plus bv5 bv7) bv4]

                     [same-bv (plus bv6 bv0) bv6]
                     [same-bv (plus bv6 bv1) bv7]
                     [same-bv (plus bv6 bv2) bv0]
                     [same-bv (plus bv6 bv3) bv1]
                     [same-bv (plus bv6 bv4) bv2]
                     [same-bv (plus bv6 bv5) bv3]
                     [same-bv (plus bv6 bv6) bv4]
                     [same-bv (plus bv6 bv7) bv5]
                     
                     [same-bv (plus bv7 bv0) bv7]
                     [same-bv (plus bv7 bv1) bv0]
                     [same-bv (plus bv7 bv2) bv1]
                     [same-bv (plus bv7 bv3) bv2]
                     [same-bv (plus bv7 bv4) bv3]
                     [same-bv (plus bv7 bv5) bv4]
                     [same-bv (plus bv7 bv6) bv5]
                     [same-bv (plus bv7 bv7) bv6]


                     ))

(println "yooooo")
(get-model constraints all-bounds S)