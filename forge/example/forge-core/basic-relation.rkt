#lang forge/core

(set-verbosity 10)

(sig A)
(sig B)
(relation r (A B))

(pred P
  (some ([a A]) (= (join a r) B)))

(run my-run
     #:preds [P] 
     #:backend pardinus)
(define f (stream-first (forge:get-result my-run)))

(printf "RESULT:~n~a~n" f)
