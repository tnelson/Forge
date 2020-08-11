#lang forge/core

(set-verbosity 10)


(sig IntSet #:abstract)
(sig S1 #:one #:extends IntSet)
(sig S2 #:one #:extends IntSet)
(sig S3 #:one #:extends IntSet)
(sig S4 #:one #:extends IntSet)
(sig S5 #:one #:extends IntSet)

(relation ints (IntSet Int))

(inst SumInst (= ints (+ (+ (+ (-> S20 (node/int/constant 6)) 
                               (-> S30 (+ (+ (node/int/constant 1) 
                                             (node/int/constant 2)) 
                                             (node/int/constant 3)))) 
                               (-> S40 (+ (+ (node/int/constant -5) 
                                          (node/int/constant -1)) 
                                          (node/int/constant 3)))) 
                               (-> S50 (+ (node/int/constant 7) 
                                          (node/int/constant 1))))))
; NEED SING
(pred Sum
    (all ([i Int]) (= i (sing (sum i))))
    (int= (sum (join S1 ints)) (node/int/constant 0))
    (int= (sum (join S2 ints)) (node/int/constant 6))
    (int= (sum (join S3 ints)) (node/int/constant 6))
    (int= (sum (join S4 ints)) (node/int/constant -3))
    (int= (sum (join S5 ints)) (node/int/constant 8)))

(run sums #:preds ((and (not Sum))) #:scope ((IntSet 0 5)) #:bounds (SumInst))
(display sums)

; (sig A)

; (relation r (A Int) #:is func)

; (pred sum-is-one
;       (int= (sum-quant ([a A]) (sum (join a r))) (node/int/constant 1)))

; (run my-run
;      #:preds [sum-is-one]
;      #:scope ([Int 4]))
;(display my-run)
