#lang forge/core

; (set-verbosity 10)

(sig A)
(sig B)
(relation R (A B))

(inst inst1
    (= A (+ Aaron (+ Alice Andy)))
    (= B (+ Betty (+ Bob Bennett)))
    (= R (+ (-> (+ Aaron Alice) Bob)
            (-> Andy (+ Betty Bennett)))))

(test basic1 #:bounds [inst1] #:expect sat)
(test basic-sizes
      #:preds [(int= (card A) (int 3))]
      #:bounds [inst1]
      #:expect theorem)


(inst inst2
    (= A (+ A1 (+ A2 A3))))

(test basic2 #:bounds [inst2] #:expect sat)
(test basic-set-size
      #:preds [(int= (card A) (int 3))]
      #:bounds [inst2]
      #:expect theorem)
(test basic-unset-size
      #:preds [(int= (card B) (int 2))
               (int= (card R) (int 5))]
      #:bounds [inst2]
      #:expect sat)

(inst inst3
    (= A John)
    (= B John))

; (test basic3 #:bounds [inst3] #:expect unsat) ; BUGGED
