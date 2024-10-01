#lang reader "kkcli-reader.rkt"

(configure :bitwidth 4 :produce-cores false :solver SAT4J :max-solutions 100 :verbosity 3)
(univ 21)
(ints [
    (0 0) (1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) 
    (9 9) (10 10) (11 11) (12 12) (13 13) (14 14) (15 15)
])

;(r0 [(-> none none) :: {
;    (16 16) (16 17) (16 18) (16 19) (16 20) 
;    (17 16) (17 17) (17 18) (17 19) (17 20) 
;    (18 16) (18 17) (18 18) (18 19) (18 20) 
;    (19 16) (19 17) (19 18) (19 19) (19 20) 
;    (20 16) (20 17) (20 18) (20 19) (20 20)
;}])
(r:a0 [{(16 17) (17 18) (18 19) (19 20)} :: {(16 17) (17 18) (18 19) (19 20)}])

(r:a1 [none :: {(16) (17) (18) (19) (20)}])
(f:a0(&& (some ([v:a1 : one r:a1 ]) (&& (some ([v:a2 : one r:a1 ]) (&& (no (. r:a0 v:a1 ))(all ([v:a3 : one (- r:a1 v:a1 )]) (&& (one (. r:a0 v:a3 ))))(no (. v:a2 r:a0 ))(all ([v:a3 : one (- r:a1 v:a2 )]) (&& (one (. v:a3 r:a0 ))))))))))
(assert f:a0)
(f:a1(in r:a0 (-> r:a1 r:a1 )))
(assert f:a1)
(solve)
