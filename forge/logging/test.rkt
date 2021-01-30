#lang forge/core "forge1" "thomas_del_vecchio@brown.edu"

(sig A)
(sig B)
(relation r (A B))

(test theorem-test #:preds [(in (join A r) B)]
                   #:expect theorem)

(test sat-test #:preds [(= (join A r) B)]
               #:expect sat)

(test unsat-test #:preds [(some A) (= A B)]
                 #:expect unsat)

(run my-run)
(define x (stream-first (forge:get-result my-run)))

