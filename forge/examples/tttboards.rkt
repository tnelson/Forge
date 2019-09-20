#lang forge

; TODO: abstractness optional
(declare-sig Player)
(declare-one-sig X #:extends Player)
(declare-one-sig O #:extends Player)

(declare-sig Index ([inverse Index]))
(declare-one-sig A #:extends Index)
(declare-one-sig B #:extends Index)
(declare-one-sig C #:extends Index)

(pred fix-inverses
      (= inverse (+ (-> A C) (-> B B) (-> C A))))

(declare-sig Board ([places Index Index Player]))

; TODO join syntax is pretty awkward
; TODO: let?
; TODO: error message if we swap X/O and, e.g., index
(pred (xturn b)
      (int= (card (join (join b places) X))
            (card (join (join b places) O))))

(pred (oturn b)
      (int= (subtract (card (join (join b places) X)) 1)
            (card (join (join b places) O))))


; TODO: how do i control which to run?
;(run "raw" () ((player 2 2) (index 3 3) (board 1 1)))
; TODO: error message from this
;(run "xturn" (xturn) ((player 2 2) (index 3 3) (board 1 1)))

; TODO: run a predicate with args directly (auto-gen closed fmla w/ existentials)
(pred somexturn
      (some ([b Board]) (xturn b)))
;(run "xturn" (somexturn) ((player 2 2) (index 3 3) (board 1 1)))

; TODO: dont force unfolded quantifiers
(pred at-most-one-mark-per-square
      ;(all ([b Board] [r Index] [c Index])
      (all ([b Board])
           (all ([r Index])
                (all ([c Index])
                     (or (int= (card (join b (join (join places r) c))) 1)
                         (int= (card (join b (join (join places r) c))) 0))))))

(pred (winH b p)
      (some ([r Index])
            (all ([c Index])
                 ;(in b (join places p))))) ; TODO arity problem--error quality
                 (in b (join (join (join places p) c) r)))))
(pred (winV b p)
      (some ([c Index])
            (all ([r Index])
                 (in b (join (join (join places p) c) r)))))

(pred (winD b p)
      (all ([i Index])
           (or 
            (in b (join (join (join places p) i) i))
            (in b (join (join (join places p) i) (join i inverse))))))

(pred (winning b p)
      (or (winH b p) (winV b p) (winD b p)))

(pred (valid-board b)
      (or (oturn b) (xturn b)))

(run "xturn" (at-most-one-mark-per-square (some ([b Board]) (xturn b))) ((Player 2 2) (Index 3 3) (Board 1 1)))
;(run "oturn" (at-most-one-mark-per-square (some ([b Board]) (oturn b))) ((Player 2 2) (Index 3 3) (Board 1 1)))
(run "owinningDiag" (at-most-one-mark-per-square
                     fix-inverses
                     (some ([b Board]) (and (valid-board b) (winD b O))))
     ((Player 2 2) (Index 3 3) (Board 1 1)))


; pred management
; error quality
; UI + modes
;  RUN ONE AT A TIME
; functions
; type annotations? check for *use*?