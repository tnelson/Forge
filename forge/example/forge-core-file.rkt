#lang forge/core

(sig Person #:abstract)

(sig A #:extends Person)
(sig B #:extends Person)

(relation friend (A B))

(sig AKing #:one #:extends A)
(sig BKing #:one #:extends B)

(pred (AreFriends a b)
  (in (-> a b) friend))

(fun (getFriends a)
  (join a friend))

(run everyoneHasFriends #:preds [
  (all ([a A]) (some (getFriends a)))
  (all ([b B]) (some (join friend b)))
  (AreFriends AKing BKing)])
(display everyoneHasFriends)