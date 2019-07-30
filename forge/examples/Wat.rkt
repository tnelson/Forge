#lang forge

(declare-sig node ((graph node)))

(pred should-be-true (> 4 3))

(run "floopdedoop" (should-be-true))