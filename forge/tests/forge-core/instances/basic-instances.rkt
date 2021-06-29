#lang forge/core

(set-option! 'verbose 0)
; (set-verbosity 10)

(sig A)
(sig B)
(relation R (A B))

(inst inst1
    (= A (+ (atom 'Aaron) (+ (atom 'Alice) (atom 'Andy))))
    (= B (+ (atom 'Betty) (+ (atom 'Bob) (atom 'Bennett))))
    (= R (+ (-> (+ (atom 'Aaron) (atom 'Alice)) (atom 'Bob))
            (-> (atom 'Andy) (+ (atom 'Betty) (atom 'Bennett))))))

(test basic1 #:bounds [inst1] #:expect sat)
(test basic-sizes
      #:preds [(int= (card A) (int 3))]
      #:bounds [inst1]
      #:expect theorem)

(inst inst2
    (= A (+ (atom 'A1) (+ (atom 'A2) (atom 'A3)))))

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
    (= A (atom 'John))
    (= B (atom 'John)))

; (test basic3 #:bounds [inst3] #:expect unsat) ; BUGGED
