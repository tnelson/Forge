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
    (= A (+ A1 A2)))

(test basic2 #:bounds [inst2] sat)

; #hash((A . #(struct:bound (relation 1 "A" (A) univ) () ((A0) (A1) (A2) (A3))))
;       (B . #(struct:bound (relation 1 "B" (B) univ) () ((B0) (B1) (B2) (B3))))
;       (Int . #(struct:bound (relation 1 "Int" (Int) univ) ((-8) (-7) (-6) (-5) (-4) (-3) (-2) (-1) (0) (1) (2) (3) (4) (5) (6) (7)) 
;                                                           ((-8) (-7) (-6) (-5) (-4) (-3) (-2) (-1) (0) (1) (2) (3) (4) (5) (6) (7)))))