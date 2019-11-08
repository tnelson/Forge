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
(r0 [(-> none none) :: {(16 17) (17 18) (18 19) (19 20)}])

(r1 [none :: {(16) (17) (18) (19) (20)}])
(f0(&& (some ([v1 : one r1 ]) (&& (some ([v2 : one r1 ]) (&& (no (. r0 v1 ))(all ([v3 : one (- r1 v1 )]) (&& (one (. r0 v3 ))))(no (. v2 r0 ))(all ([v3 : one (- r1 v2 )]) (&& (one (. v3 r0 ))))))))))
(assert f0)
(f1(in r0 (-> r1 r1 )))
(assert f1)
(solve)
