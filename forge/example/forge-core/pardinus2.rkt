#lang forge/core

(set-verbosity 10)

#|
>>>>>>> fb1e34f850d8e831625a576094e20c04133230e8
(sig A)
(sig B)
(relation r (A B))

<<<<<<< HEAD
(run my-run
     #:backend pardinus)
(stream-first (forge:get-result my-run))

(evaluate my-run #f (join A r))
(evaluate my-run #f (join (atom 'A3) r))
(evaluate my-run #f (+ (atom 'B0) (join r (atom 'B0))))
=======
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
