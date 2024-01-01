#lang forge/bsl
option verbose 0

abstract sig Player {}
one sig X, O extends Player {}
sig Board {
  board: pfunc Int -> Int -> Player
}
pred wellformed {
  all b: Board | all row, col: Int | {
    (row < 0 or row > 2 or col < 0 or col > 2) implies
      no b.board[row][col]
  }
}

sig Thing {} -- for testing parse/expand of #X = N

inst my_instance
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
                                    
  -- The above should be equivalent to:
  --board = `Board0 -> (1, 1)   -> `X +
  --        `Board0 -> (1 -> 2) -> `O +
  --        `Board0 -> 1 -> 0   -> `X
  --        +
  --        `Board1 -> (2, 2)   -> `X +
  --        `Board1 -> (1 -> 2) -> `O +
  --        `Board1 -> 1 -> 0   -> `X    

  -- This is an error (cannot combine piecewise bounds with complete bounds)
  -- no board 

  -- This is an error (rebinding detected)
  -- `Board0.board = (1, 1)   -> `X

  -- `Board2.board is left unspecified, can vary

  -- TODO: support "no `Board2.board"
  -- TODO: test for in, ni
  
  #Thing = 1
}




example moveMiddleFirst is {wellformed} for my_instance

--option verbose 5

-- test that semantics of piecewise-bounds syntax are consistent, cardinality works, etc.
test expect {
  my_instance_sat: {} for my_instance is sat
  card_semantics: {#Thing = 1} for my_instance is theorem
  piecewise_semantics: {
    some disj b0, b1: Board | {
      b0.board[1][1] = X
      b0.board[1][2] = O
      b0.board[1][0] = X
      b1.board[2][2] = X
      b1.board[1][2] = O
      b1.board[1][0] = X
      #b0.board = 3
      #b1.board = 3
    }
  } for my_instance is theorem

  piecewise_semantics_unconstrained_a: {
    some b2: Board | #b2.board = 0
  } for my_instance is sat
  piecewise_semantics_unconstrained_b: {
    some b2: Board | #b2.board > 0
  } for my_instance is sat

}