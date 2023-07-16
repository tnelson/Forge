#lang forge/core

(sig A)
(relation R (A A))

(inst my-inst
    (is R plinear))

(run my-run #:bounds [my-inst (<= 1 (card A) 3)] #:scope ([A 2 4]))
(display my-run)