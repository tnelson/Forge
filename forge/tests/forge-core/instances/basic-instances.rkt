#lang forge/core

(set-verbosity 10)

(sig A)
(sig B)
(relation R (A B))

(inst inst1
    (= A (+ Aaron (+ Alice Andy)))
    (= B (+ Betty (+ Bob Bennett)))
    (= R (+ (-> (+ Aaron Alice) Bob)
            (-> Andy (+ Betty Bennett)))))

(test basic1 #:bounds [inst1] sat)
(check basic-sizes
       #:preds [(int= (card A) (node/int/constant 3))]
       #:bounds [inst1])


(inst inst2
    (= A (+ A1 (+ A2 A3))))

(test basic2 #:bounds [inst2] sat)
(check basic-set-size
       #:preds [(int= (card A) (node/int/constant 3))]
       #:bounds [inst2])
(test basic-unset-size
      #:preds [(int= (card B) (node/int/constant 2))
               (int= (card R) (node/int/constant 5))]
      #:bounds [inst2]
      sat)

(inst inst3
    (= A John)
    (= B John))

; (test basic3 #:bounds [inst3] unsat) ; BUGGED
