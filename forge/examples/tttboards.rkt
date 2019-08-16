#lang forge

; TODO: abstractness optional
(declare-sig Player)
(declare-one-sig X #:extends Player)
(declare-one-sig O #:extends Player)

(declare-sig Index)
(declare-one-sig A #:extends Index)
(declare-one-sig B #:extends Index)
(declare-one-sig C #:extends Index)

(declare-sig Board ([places Index Index Player]))

; TODO join syntax is pretty awkward
; TODO: let?
; TODO: error message if we swap X/O and, e.g., index
(pred (xturn b)
      (int= (card (join (join b places) X))
            (card (join (join b places) O))))

; TODO: how do i control which to run?
;(run "raw" () ((player 2 2) (index 3 3) (board 1 1)))
; TODO: error message from this
;(run "xturn" (xturn) ((player 2 2) (index 3 3) (board 1 1)))

; TODO: run a predicate with args directly (auto-gen closed fmla w/ existentials)
(pred somexturn
      (some ([b Board]) (xturn b)))
;(run "xturn" (somexturn) ((player 2 2) (index 3 3) (board 1 1)))

(pred at-most-one-mark-per-square
      (all ([b Board] [r Index] [c Index])
           (or (int= (card (join b (join (join places r) c))) 1)
               (int= (card (join b (join (join places r) c))) 0))))


(run "xturn" (at-most-one-mark-per-square (some ([b Board]) (xturn b))) ((Player 2 2) (Index 3 3) (Board 1 1)))

