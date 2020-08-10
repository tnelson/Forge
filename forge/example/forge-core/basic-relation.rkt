#lang forge/core

(set-verbosity 10)

(sig A)
(sig B)
(relation r (A B))

(pred P
  (= A (join r B)))

(run my-run
     #:preds P)
(display my-run)
