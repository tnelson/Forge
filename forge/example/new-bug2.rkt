#lang forge/core


; /*
; (sig A)
; (sig B)
; (relation friend (A Int B))

; (pred (P a1 a2) (= (join (+ a1 a2) friend) (-> Int B)))

; (run my-run #:preds [(some ([a1 A] [a2 A]) (P a1 a2))])
; (display my-run)
; */

; /*
; sig A { friend: set Int -> B }
; sig B {}

; pred P[a1, a2: A] {
;     A.friend = Int->B
; }

; run {
;     some a1, a2: A | {
;         P[a1, a2]
;     }
; }
; */

; /*
; abstract sig Char {}

; one sig A extends Char {}
; one sig B extends Char {}
; one sig C extends Char {}
; one sig D extends Char {}
; one sig E extends Char {}
; one sig F extends Char {}
; one sig G extends Char {}

; sig seqChar {
;     r: set Int -> Char
; }

; run {
;     #seqChar = 4
;     r = seqChar->Int->Char
; }
; */

(sig Char #:abstract)
(sig A #:one #:extends Char)
(sig B #:one #:extends Char)
(sig C #:one #:extends Char)
(sig D #:one #:extends Char)
(sig E #:one #:extends Char)
; (sig F #:one #:extends Char)
; (sig G #:one #:extends Char)

(sig seqChar)

(relation r (seqChar Int Char))

(run my-run #:preds [(= (card seqChar) (node/int/constant 3)) (= r (-> seqChar Int Char))])
(display my-run)
