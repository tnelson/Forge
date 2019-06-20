#lang racket

(require "smtlib2.rkt")

;(set-option :produce-unsat-cores true)

(define-const o1 Int 1)
(define-const o2 Int 2)
(define-const o3 Int 3)

(define-const p1 Int 4)
(define-const p2 Int 5)
(define-const p3 Int 6)

(define-fun owns ((o Int)) Int
  (if (= o 1) 4
    (if (= o 2) 5
    (if (= o 3) 6
    -1)))
)

(define-fun owned ((p Int)) Int
  (if (= p 4) 1
    (if (= p 5) 2
    (if (= p 6) 3
    -1)))
)

(declare-fun near (Int Int) Int)

(define-fun no-owners ((t Int) (p Int)) Bool
  (if (= (near t p) 0)
    (and (= 1 (near t o1)) (= 1 (near t o2)) (= 1 (near t o3)))
    (and (not (= 1 (near t o1))) (not (= 1 (near t o2))) (not (= 1 (near t o3))))
))

(declare-const end-time Int)

(define-fun with-owner ((t Int) (p Int)) Bool
  (= (near t p) (near t (owned p)))
)

(define-fun diff ((t Int) (p Int)) Int
  (if (= (mod t 2) 0)
    (- (near t p) (near (+ t 1) p))
    (- (near (+ t 1) p) (near t p))
  )
)

(define-fun no-stealing ((t Int)) Bool
  (and
    (forall ((p Int))
      (=> (and (> p 3) (< p 7))
        (or (no-owners t p) (with-owner t p))
      )
    )
  )
)

(define-fun diff-sum ((t Int)) Int
  (+ (diff t p1) (diff t p2) (diff t p3) (diff t o1) (diff t o2) (diff t o3))
)

(define-fun diffs+ ((t Int)) Bool
  (and
    (>= (diff t p1) 0)
    (>= (diff t p2) 0)
    (>= (diff t p3) 0)
    (>= (diff t o1) 0)
    (>= (diff t o2) 0)
    (>= (diff t o3) 0)
  )
)

(assert (forall ((t Int))
  (=> (and
    (>= t 0)
    (<= t end-time))
    (no-stealing t))))

(assert (forall ((t Int))
  (=> (and (>= t 0) (< t end-time))
  (and
;    (>= (orig-owners-diff t) 0)
;    (>= (orig-pets-diff t) 0)
;    (<= (+ (orig-owners-diff t) (orig-pets-diff t)) 2)
;    (> (+ (orig-owners-diff t) (orig-pets-diff t)) 0)
    (forall ((p Int))
      (=> (and (<= 1 p) (>= 6 p))
      (or (= (near t p) 1) (= (near t p) 0))
      )
    )
    (diffs+ t)
    (> (diff-sum t) 0)
    (<= (diff-sum t) 2)
    )
)))


(assert (= 1 (near 0 p1)))
(assert (= 1 (near 0 p2)))
(assert (= 1 (near 0 p3)))
(assert (= 1 (near 0 o1)))
(assert (= 1 (near 0 o2)))
(assert (= 1 (near 0 o3)))

(assert (= 0 (near end-time p1)))
(assert (= 0 (near end-time p2)))
(assert (= 0 (near end-time p3)))
(assert (= 0 (near end-time o1)))
(assert (= 0 (near end-time o2)))
(assert (= 0 (near end-time o3)))

(assert (> end-time 0))
(assert (< end-time 12))

(check-sat)
(get-model)


(exit)
