#lang forge

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

example moveMiddleFirst is {wellformed} for
{
  Board = `Board0 + `Board1 -- Board1 is just for confirming that grouping works.
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

  -- Q: do we support "in"/"ni" in this form?
  

  #Thing = 3
} 