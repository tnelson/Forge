#lang forge/core

(set-verbosity 10)

(sig A)
(sig B)
(relation r (A B))

(pred P
  (= A (join r B)))
(pred Q
  (some ([a A]) (= (join a r) B)))

(run my-run
     #:preds [P Q])
(display my-run)
