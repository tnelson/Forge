#lang forge

(declare-sig abstract-cat ((friends abstract-cat)))

(declare-sig kitty #:extends abstract-cat)

(declare-one-sig kittyBacon #:extends abstract-cat)

(pred should-be-true (> 4 3))

(run "floopdedoop" (should-be-true) ((kitty 2 3)))