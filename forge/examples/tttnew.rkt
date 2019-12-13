#lang forge

sig Player {}
one sig X,O extends Player {}

sig Index {inverse: Index }
one sig A, B, C extends Index {}

pred fixInverses[] { 
   inverse = A->C + B->B + C->A 
}

//sig Board {places: Index -> Index -> Player}
/*$
(declare-sig Board ([places Index Index Player]))
*/




pred xturn[b: Board] {
  #b.places.X = #b.places.O
} 
 
pred oturn[b: Board] { 
  subtract[#b.places.X,1] = #b.places.O
}

pred somexturn[] {
  //some b: Board | xturn[b]
 some b: Board | #(b.places.X) = #(b.places.O)
}                  

try_xturn : run somexturn for 2 Player, 3 Index, 1 Board



       /*
pred atMostOneMarkPerSquare[] {
  all b: Board, r: Index, c: Index |
    #b.places[r][c] = 1 or
    #b.places[r][c] = 0 // inefficient
}*/
/*
pred winH[b: Board, p: Player] {
  some r: Index | all c: Index | p in b.places[r][c]
}

pred winV[b: Board, p: Player] {
  some c: Index | all r: Index | p in b.places[r][c]
}
*/
/*
pred winDBroken[b: Board, p: Player] {
  all i: Index | {p in b.places[i][i] or p in b.places[i][i.inverse]}
}

pred winD[b: Board, p: Player] {
  {all i: Index | p in b.places[i][i]}
  or
  {all i: Index | p in b.places[i][i.inverse]}
}

       */
/*
diags_equiv : run {
  at_most_one_mark_per_square
  fix_inverses
  some b: Board | {winD[b, O] and not winDBroken[b, O]} or {not winD[b, O] and winDBroken[b, O]}
} for 2 Player, 3 Index, 1 Board
*/
/*
pred winning[b: Board, p: Player] {
  winH[b, p] or winV[b, p] or winD[b, p]
}
*/
/*
pred valid_board[b: Board] {
  oturn[b] or xturn[b]
}
*/

--/*$
/*
; TODO: abstractness optional
;(declare-sig Player)
;(declare-one-sig X #:extends Player)
;(declare-one-sig O #:extends Player)

;(declare-sig Index ([inverse Index]))
;(declare-one-sig A #:extends Index)
;(declare-one-sig B #:extends Index)
;(declare-one-sig C #:extends Index)

;(pred fix-inverses
;      (= inverse (+ (-> A C) (-> B B) (-> C A))))

;(declare-sig Board ([places Index Index Player]))

; TODO join syntax is pretty awkward
; TODO: let?
; TODO: error message if we swap X/O and, e.g., index
;(pred (xturn b)
;      (int= (card (join (join b places) X))
;            (card (join (join b places) O))))

;(pred (oturn b)
;      (int= (subtract (card (join (join b places) X)) 1)
;            (card (join (join b places) O))))


; TODO: how do i control which to run?
;(run "raw" () ((player 2 2) (index 3 3) (board 1 1)))
; TODO: error message from this
;(run "xturn" (xturn) ((player 2 2) (index 3 3) (board 1 1)))

; TODO: run a predicate with args directly (auto-gen closed fmla w/ existentials)
;(pred somexturn
;      (some ([b Board]) (xturn b)))
;(run "xturn" (somexturn) ((player 2 2) (index 3 3) (board 1 1)))

; TODO: dont force unfolded quantifiers
;(pred at-most-one-mark-per-square
;      ;(all ([b Board] [r Index] [c Index])
;      (all ([b Board])
;           (all ([r Index])
;                (all ([c Index])
;                     (or (int= (card (join b (join (join places r) c))) 1)
;                         (int= (card (join b (join (join places r) c))) 0))))))
;
;(pred (winH b p)
;      (some ([r Index])
;            (all ([c Index])
;                 ;(in b (join places p))))) ; TODO arity problem--error quality
;                 (in b (join (join (join places p) c) r)))))
;(pred (winV b p)
;      (some ([c Index])
;            (all ([r Index])
;                 (in b (join (join (join places p) c) r)))))
;
;(pred (winDBroken b p)
;      (all ([i Index])
;           (or 
;            (in b (join (join (join places p) i) i))
;            (in b (join (join (join places p) i) (join i inverse))))))
;
;(pred (winD b p)
;      (or 
;       (all ([i Index]) (in b (join (join (join places p) i) i)))
;       (all ([i Index]) (in b (join (join (join places p) i) (join i inverse))))))

;(run "DiagsEquiv?" (at-most-one-mark-per-square
;                    fix-inverses
;                    (some ([b Board])
;                          (or (and (winD b O) (not (winDBroken b O)))
;                              (and (winDBroken b O) (not (winD b O))))))
;     ((Player 2 2) (Index 3 3) (Board 1 1)))
;
;
;(pred (winning b p)
;      (or (winH b p) (winV b p) (winD b p)))
;
;(pred (valid-board b)
;      (or (oturn b) (xturn b)))

;(run "xturn" (at-most-one-mark-per-square (some ([b Board]) (xturn b))) ((Player 2 2) (Index 3 3) (Board 1 1)))
;(run "oturn" (at-most-one-mark-per-square (some ([b Board]) (oturn b))) ((Player 2 2) (Index 3 3) (Board 1 1)))
;(run "owinningDiag" (at-most-one-mark-per-square
;                     fix-inverses
;                     (some ([b Board]) (and (valid-board b) (winD b O))))
;     ((Player 2 2) (Index 3 3) (Board 1 1)))


; pred management
; error quality
; UI + modes
;  RUN ONE AT A TIME
; functions
; type annotations? check for *use*?
*/