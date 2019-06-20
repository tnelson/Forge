#lang reader "testlang.rkt"

; Modeling sequential code in SSA form
;; Buggy swap
; int x, y;
; int t = x;
; x = y;
; y = x;

(set-logic QF_UFLIA)
(set-option :produce-models true)
(declare-fun x (Int) Int)  (declare-fun y (Int) Int)
(declare-fun t (Int) Int)
(assert (= (t 0) (x 0)))
(assert (= (y 1) (t 0)))
(assert (= (x 1) (y 1)))

(assert (not 
  (and (= (x 1) (y 0)) 
       (= (y 1) (x 0)))))
(check-sat)
(get-value ((x 0) (y 0) (x 1) (y 1)))
; possible returned valuation:
; (((x 0) (- 1)) ((y 0) 2) ((x 1) (- 1)) ((y 1) (- 1)))
(get-model)
; possible returned model:
; (
;  (define-fun x ((_ufmt_1 Int)) Int (- 1))
;  (define-fun y ((_ufmt_1 Int)) Int (ite (= _ufmt_1 1) (- 1) 2))
;  (define-fun t ((_ufmt_1 Int)) Int (- 1))
; )
(exit)