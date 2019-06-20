#lang rosette

(define-symbolic end-time integer?)

(define-symbolic near-goats (~> integer? integer?))
(define-symbolic far-goats (~> integer? integer?))

(define-symbolic near-wolves (~> integer? integer?))
(define-symbolic far-wolves (~> integer? integer?))

(define (orig-wolves t)
  (if (= (modulo t 2) 0)
      (near-wolves t)
      (far-wolves t)))

(define (orig-goats t)
  (if (= (modulo t 2) 0)
      (near-goats t)
      (far-goats t)))

(define (dst-wolves t)
  (if (= (modulo t 2) 0)
      (far-wolves t)
      (near-wolves t)))

(define (dst-goats t)
  (if (= (modulo t 2) 0)
      (far-goats t)
      (near-goats t)))

(define (orig-goats-diff t)
   (- (orig-goats t) (- 3 (orig-goats (+ t 1)))))


(define (orig-wolves-diff t)
   (- (orig-wolves t) (- 3 (orig-wolves (+ t 1)))))


(define (goat-bound time)
  (= (+ (near-goats time) (far-goats time)) 3))

(define (wolf-bound time)
  (= (+ (near-wolves time) (far-wolves time)) 3))

(define (no-eating time)
  (and
    (or (= (near-goats time) 0) (>= (near-goats time) (near-wolves time)))
    (or (= (far-goats time) 0) (>= (far-goats time) (far-wolves time)))))

(define-symbolic t integer?)

(assert (forall (list t)
  (=> (and
    (>= t 0)
    (<= t end-time))
    (and
      (wolf-bound t)
      (goat-bound t)
      (no-eating t)))))

(assert (forall (list t)
  (=> (and (>= t 0) (< t end-time))
  (and
    (>= (orig-goats-diff t) 0)
    (>= (orig-wolves-diff t) 0)
    (<= (+ (orig-goats-diff t) (orig-wolves-diff t)) 2)
    (> (+ (orig-goats-diff t) (orig-wolves-diff t)) 0))
)))

(assert (= 3 (near-wolves 0)))
(assert (= 3 (near-goats 0)))

(assert (= 3 (far-wolves end-time)))
(assert (= 3 (far-goats end-time)))

(assert (> end-time 0))
;(assert (< end-time 43824509))

(define sol (solve (assert #t)))

(define ng (evaluate near-goats sol))
(evaluate far-goats sol)
(evaluate near-wolves sol)
(evaluate far-wolves sol)
(evaluate end-time sol)