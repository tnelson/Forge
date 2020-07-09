#lang forge/core

(set-verbosity 10)

; (sig Person #:abstract)

; (sig A #:extends Person)
; (sig B #:extends Person)

; (relation friend (A B))

; (sig AKing #:one #:extends A)
; (sig BKing #:one #:extends B)

; (pred (AreFriends a b)
;   (in (-> a b) friend))

; (fun (getFriends a)
;   (join a friend))

; (run everyoneHasFriends #:preds [
;   (all ([a A]) (some (getFriends a)))
;   (all ([b B]) (some (join friend b)))
;   (AreFriends AKing BKing)])
; (display everyoneHasFriends)


(sig A)
(sig B)
(relation R (A B))

(inst my-inst
  (= A (+ Albert Anna))
  (= B (+ Bob (+ Brianna Beth)))
  (= R (+ (-> Albert Bob) (-> Anna Brianna))))

(inst my-inst2
  (ni A (+ Albert Anna)))

(run my-run #:inst my-inst2)
(display my-run)





; (instance my-inst
;     (no R)
;     (one R)
;     (two R)
;     (lone R)
;     (= (card R) 5)
;     (<= (card R) 5)
;     (<= 3 (card R) 5)
;     (is (~ R) linear)
;     (is R linear)

;     (= R (-> A0 B0 (+ C0 C1)))
;     (in R (-> A0 B0 (+ C0 C1)))
;     (ni R (-> A0 B0 (+ C0 C1)))

;     (= (join A R) (-> B0 (+ C0 C1)))
;     (in (join A R) (-> B0 (+ C0 C1)))
;     (ni (join A R) (-> B0 (+ C0 C1)))

;     (and (no R) (one R1))



  ; [(_ (QualName f)) (syntax/loc stx (f bindings))]

  ; [(_ (_ (QualName Int)) "[" (_ (_ (Const (Number i)))) "]")
  ;  (quasisyntax/loc stx (set-bitwidth #,(string->number (syntax-e #'i))))]