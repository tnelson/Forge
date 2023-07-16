#lang reader "kkcli-reader.rkt"



(configure :bitwidth 4
           :solver SAT4J 
           :max-solutions 5
           :verbosity 1)

(univ 10)

(ints [(1 0)(2 1) (4 2) (-8 3)])

(r0 r1 r2 [ none :: (+ ints { (4) ... (6) })])
(r3 [iden :: (-> univ univ)])

(i0 (sum r0))
(i1 (sum r1))
(f0 (< i0 i1))
(e0 (- r2 ints))
(e1 (. e0 r3))
(f1 (lone e1))

(assert f0)
(assert f1)

(solve)