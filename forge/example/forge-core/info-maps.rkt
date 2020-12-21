#lang forge/core

(sig A)
(sig B)
(relation r (A B))

(pred (spans-B? a)
      (= B (join a r)))

(fun (points-to a)
     (join a r))

(const a-span (join A r))

((forge:get-pred forge:curr-state 'spans-B?) B)
((forge:get-fun forge:curr-state 'points-to) A)
(forge:get-const forge:curr-state 'a-span)

(run my-run)

((forge:get-pred my-run 'spans-B?) B)
((forge:get-fun my-run 'points-to) A)
(forge:get-const my-run 'a-span)
