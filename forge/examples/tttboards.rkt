#lang forge

; TODO: abstractness optional
(declare-sig player)
(declare-one-sig X #:extends player)
(declare-one-sig O #:extends player)

(declare-sig index)
(declare-one-sig A #:extends index)
(declare-one-sig B #:extends index)
(declare-one-sig C #:extends index)

(declare-sig board ((places index index player)))

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
      (some ([b board]) (xturn b)))
;(run "xturn" (somexturn) ((player 2 2) (index 3 3) (board 1 1)))

(run "xturn" ((some ([b board]) (xturn b))) ((player 2 2) (index 3 3) (board 1 1)))
