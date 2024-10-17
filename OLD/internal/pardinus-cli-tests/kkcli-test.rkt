#lang reader "kkcli-reader.rkt"



(configure :bitwidth 4
           :solver SAT4J 
           :max-solutions 5
           :verbosity 1)

(univ 10)

(ints [(1 0)(2 1) (4 2) (-8 3)])

(r:a0 r:a1 r:a2 [ none :: (+ ints { (4) ... (6) })])
(r:a3 [iden :a:a (-> univ univ)])

(i:a0 (sum r:a0))
(i:a1 (sum r:a1))
(f:a0 (< i:a0 i:a1))
(e:a0 (- r:a2 ints))
(e:a1 (. e:a0 r:a3))
(f:a1 (lone e:a1))

(assert f:a0)
(assert f:a1)

(solve)