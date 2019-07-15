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
(define indices-bounds (make-exact-bound indices '((i0) (i1) (i2) )))

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

(define all-bounds (instantiate-bounds (bounds U (append B (list indices-bounds ints-bound ints-map-bound)))))

(define (iff a b)
(and (=> a b) (=> b a)))

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
(define x0 (list-ref x 2))
(define x1 (list-ref x 1))
(define x2 (list-ref x 0))
(define y0 (list-ref y 2))
(define y1 (list-ref y 1))
(define y2 (list-ref y 0))
(define-values (b0-sum b0-carry) (fulladd x0 y0 falsum))
(define-values (b1-sum b1-carry) (fulladd x1 y1 b0-carry))
(define-values (b2-sum b2-carry) (fulladd x2 y2 b1-carry))
(list b2-sum b1-sum b0-sum))

(define (same-bv bva bvb)
(and 
(iff (list-ref bva 0)(list-ref bvb 0))
(iff (list-ref bva 1)(list-ref bvb 1))
(iff (list-ref bva 2)(list-ref bvb 2))))

(define constraints (and
[same-bv (plus (plus (plus bv0 bv4) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv4 bv1) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv3 bv1) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv0 bv3) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv4 bv0) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv0 bv5) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv3 bv0) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv4 bv1) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv3 bv0) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv3 bv2) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv3 bv1) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv2 bv0) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv3 bv0) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv0 bv5) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv3 bv1) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv3 bv1) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv1 bv2) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv4 bv0) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv1 bv0) bv1) bv1) bv3]
[same-bv (plus (plus (plus bv2 bv0) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv1 bv0) bv1) bv1) bv3]
[same-bv (plus (plus (plus bv4 bv1) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv0 bv1) bv1) bv1) bv3]
[same-bv (plus (plus (plus bv2 bv1) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv0 bv4) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv1 bv4) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv0 bv1) bv1) bv1) bv3]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv0 bv2) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv1 bv0) bv1) bv1) bv3]
[same-bv (plus (plus (plus bv1 bv3) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv3 bv1) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv4 bv1) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv0 bv0) bv1) bv1) bv2]
[same-bv (plus (plus (plus bv4 bv0) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv2 bv3) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv3 bv1) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv0 bv4) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv3 bv1) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv1 bv1) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv0 bv2) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv2 bv2) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv2 bv0) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv0 bv4) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv0 bv2) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv0 bv3) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv0 bv5) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv3 bv1) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv4 bv0) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv2 bv0) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv1 bv4) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv0 bv3) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv2 bv3) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv2 bv2) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv3 bv1) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv2 bv1) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv1 bv4) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv3 bv1) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv3 bv1) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv2 bv0) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv2 bv3) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv1 bv1) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv2 bv2) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv2 bv0) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv2 bv3) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv4 bv0) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv0 bv4) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv3 bv0) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv0 bv2) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv0 bv1) bv1) bv1) bv3]
[same-bv (plus (plus (plus bv2 bv1) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv0 bv3) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv2 bv0) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv0 bv5) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv2 bv3) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv2 bv1) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv1 bv2) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv3 bv0) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv2 bv0) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv4 bv1) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv3 bv2) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv3 bv2) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv4 bv0) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv2 bv1) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv0 bv5) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv2 bv0) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv1 bv3) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv0 bv2) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv1 bv3) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv1 bv2) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv2 bv2) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv0 bv1) bv1) bv1) bv3]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv4 bv1) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv1 bv4) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv3 bv2) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv1 bv3) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv0 bv1) bv1) bv1) bv3]
[same-bv (plus (plus (plus bv4 bv0) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv0 bv2) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv1 bv1) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv2 bv0) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv1 bv1) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv3 bv2) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv2 bv1) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv4 bv0) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv0 bv2) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv0 bv4) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv0 bv2) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv4 bv0) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv1 bv1) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv2 bv0) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv4 bv1) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv1 bv1) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv1 bv1) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv2 bv1) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv4 bv0) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv4 bv1) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv4 bv1) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv1 bv0) bv1) bv1) bv3]
[same-bv (plus (plus (plus bv4 bv1) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv4 bv1) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv0 bv5) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv0 bv3) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv4 bv1) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv3 bv1) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv3 bv0) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv1 bv1) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv4 bv1) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv1 bv4) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv3 bv1) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv0 bv0) bv1) bv1) bv2]
[same-bv (plus (plus (plus bv0 bv5) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv1 bv2) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv0 bv2) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv2 bv0) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv4 bv0) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv2 bv1) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv2 bv3) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv2 bv3) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv2 bv1) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv1 bv0) bv1) bv1) bv3]
[same-bv (plus (plus (plus bv3 bv0) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv3 bv1) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv3 bv1) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv2 bv1) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv1 bv4) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv4 bv1) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv2 bv2) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv3 bv1) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv2 bv3) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv1 bv4) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv0 bv5) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv4 bv0) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv3 bv2) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv3 bv0) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv0 bv1) bv1) bv1) bv3]
[same-bv (plus (plus (plus bv4 bv1) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv0 bv3) bv1) bv1) bv5]
[same-bv (plus (plus (plus bv5 bv0) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv4 bv0) bv1) bv1) bv6]
[same-bv (plus (plus (plus bv2 bv0) bv1) bv1) bv4]
[same-bv (plus (plus (plus bv3 bv2) bv1) bv1) bv7]
[same-bv (plus (plus (plus bv0 bv4) bv1) bv1) bv6]))

(println "Finished constraint interpretation, beginning translation.")
(get-model constraints all-bounds S)
