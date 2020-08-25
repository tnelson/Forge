#lang forge/core

(set-verbosity 10)

#|
(sig A)
(sig B)
(relation r (A B))

(run my-run #:preds [(and (and (some A) (some B)) (some r))]
            #:backend pardinus
            #:solver TargetSATSolver
            #:target `(sat . ,(hash A '() B '((B0) (B1) (B2) (B3))))
            #:target-distance far)
(display my-run)
|#

(sig A)
(sig A* #:one #:extends A)
(sig B)

(relation r (A B))

(run my-run #:preds [(= (join A* r) B)
                     (= A (join r B))])
(display my-run)
