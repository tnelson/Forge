(set-logic ALL)
(set-option :produce-models true)
(set-option :finite-model-find true)

; sig A {} 
(declare-sort A 0)

; sig B { b_a : one A }
(declare-sort B 0)
(declare-fun b_a (B) A)

(declare-const a0 A)
(declare-const a1 A)
(declare-const a2 A)
(declare-const a3 A)
(assert (distinct a0 a1 a2 a3))

(declare-const b0 B)
(declare-const b1 B)
(declare-const b2 B)
(declare-const b3 B)
(assert (distinct b0 b1 b2 b3))

(check-sat)
(get-model)

;; --incremental
;; --quiet (doesn't avoid the license spam...)
