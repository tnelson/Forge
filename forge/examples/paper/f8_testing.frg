#lang forge

-- Here, we switch to Relational Forge for brevity (e.g., fullFirstRow predicate uses join).

open "f2_ttt.frg" -- import 

pred fullFirstRow { 
	-- TN TODO: final version, add parentheses to Figure 8
	(Board.board[0]).X = (0+1+2) 
}
pred someMoveTaken { 
	some Board.board 
}

-----------------------------------------------------------
-- These predicates were omitted from the figure for space.
-----------------------------------------------------------

pred winH[b: Board, p: Player] {
    some r: Int | all c: Int | ( c >= 0 and c <= 2) implies
        b.board[r][c] = p
}

pred winV[b: Board, p: Player] {
	some c: Int | all r: Int | ( r >= 0 and r <= 2) implies
    	b.board[r][c] = p
}

pred winD[b: Board, p: Player] {
    (b.board[0][0] = p and 
     b.board[1][1] = p and
     b.board[2][2] = p)
    or
    (b.board[0][2] = p and 
     b.board[1][1] = p and
     b.board[2][0] = p)
}
       
pred winning {
    some p: Player | {
  		winH[Board, p] or winV[Board, p] or winD[Board, p]
	}
}

----- New Constructs : -----

inst wellformed_instance {
	Board = `Board0
    Player = `X + `O
	-- TN TODO: add these 2 trivial lines to final version, or infer for brevity
	X = `X
	O = `O
    `Board0 . board = (1 , 1) -> `X + (1 , 2) -> `O
}

example moveMiddleFirst is { wellformed } for wellformed_instance


test expect { 
	{ someMoveTaken } for wellformed_instance is sat 
}
test suite for winning {
	-- TN TODO: add "for 1 Board" to these lines in the final version
	assert fullFirstRow is sufficient for winning for 1 Board
	assert someMoveTaken is necessary for winning for 1 Board
}