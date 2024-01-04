#lang forge/bsl
option verbose 0

-------------------------------------------------
-- Tests for the Froglet partial-instance syntax.
-------------------------------------------------

abstract sig Player {}
one sig X, O extends Player {}
sig Board {
  board: pfunc Int -> Int -> Player
}

-- TODO: can we get rid of the awful parser error if a comma is left at the end of a field decl?

pred wellformed {
  all b: Board | all row, col: Int | {
    (row < 0 or row > 2 or col < 0 or col > 2) implies
      no b.board[row][col]
  }
}

sig Thing {} -- for testing parse/expand of #X = N

-------------------------------------------------

inst inst_piecewise
{
  Board = `Board0 + `Board1 + `Board2 
  Player = `X + `O
  -- TODO: infer these
  X = `X
  O = `O
  
  -- TODO: Froglet errors, not just errors of last resort
  `Board0.board = (1, 1)   -> `X +
                  (1 -> 2) -> `O +
                  1 -> 0   -> `X 
  `Board1.board = (2, 2)   -> `X +
                  (1 -> 2) -> `O +
                  1 -> 0   -> `X
                                    
  -- This is an error (cannot combine piecewise bounds with complete bounds)
  -- TODO add test
  -- no board 

  -- This is an error (rebinding detected)
  -- TODO add test
  -- `Board0.board = (1, 1)   -> `X

  -- This is an error (no piecewise bounds for sigs, just fields)
  -- TODO: add test (actually, an error of last resort ATM)
  --no `Board0.Board 
  
  -- Confirm cardinality
  #Thing = 1
}

inst inst_piecewise_in
{
  Board = `Board0 + `Board1 
  Player = `X + `O
  X = `X
  O = `O
    
  `Board0.board in (1, 1)   -> `X +
                   (1 -> 2) -> `O                    
}

inst inst_piecewise_ni
{
  Board = `Board0 + `Board1 
  Player = `X + `O
  X = `X
  O = `O
    
  `Board0.board ni (1, 1)   -> `X +
                   (1 -> 2) -> `O                    
}

inst inst_piecewise_no
{
  Board = `Board0 + `Board1 
  Player = `X + `O
  X = `X
  O = `O
    
  no `Board0.board                  
}



example moveMiddleFirst is {wellformed} for inst_piecewise

-- test that semantics of piecewise-bounds syntax are consistent, cardinality works, etc.
test expect {
  inst_piecewise_sat: {} for inst_piecewise is sat
  inst_piecewise_in_sat: {} for inst_piecewise_in is sat
  inst_piecewise_ni_sat: {} for inst_piecewise_ni is sat
  
  card_semantics_eq: {#Thing = 1} for inst_piecewise is theorem
  
  piecewise_semantics_eq: {
    some disj b0, b1: Board | {
      -- piecewise defn of "board" field
      b0.board[1][1] = X
      b0.board[1][2] = O
      b0.board[1][0] = X
      b1.board[2][2] = X
      b1.board[1][2] = O
      b1.board[1][0] = X
      #b0.board = 3
      #b1.board = 3
    }
  } for inst_piecewise is theorem
  piecewise_semantics_eq_unconstrained_maybe_empty: {
    some b2: Board | #b2.board = 0
  } for inst_piecewise is sat
  piecewise_semantics_eq_unconstrained_maybe_nonempty: {
    some b2: Board | #b2.board > 0
  } for inst_piecewise is sat

  piecewise_semantics_in: {
    some b: Board | {
      #b.board <= 2
    }
  } for inst_piecewise_in is theorem
  piecewise_semantics_in_other_unaffected: {
    some b: Board | {
      #b.board > 2
    }
  } for inst_piecewise_in is sat

    
  piecewise_semantics_ni: {
    some b: Board | {      
      b.board[1][1] = X
      b.board[1][2] = O
    }
  } for inst_piecewise_ni is theorem
  piecewise_semantics_ni_other_unaffected: {
    some b: Board | {
      #b.board < 2
    }
  } for inst_piecewise_ni is sat

         
  piecewise_semantics_no: {
    some b: Board | {
      #b.board = 0                                     
    }
  } for inst_piecewise_no is theorem
  piecewise_semantics_no_other_unaffected: {
    some b: Board | {
      #b.board > 0                          
    }
  } for inst_piecewise_no is sat

    
}