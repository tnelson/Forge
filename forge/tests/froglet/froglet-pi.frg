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

example moveMiddleFirst is {wellformed} for
{
  Board = `Board0
  Player = `X + `O
  -- TODO: infer these
  X = `X
  O = `O
  -- TODO: limited dots
  -- TODO: comma as product  
  `Board0.board = (1, 1)   -> `X +
                  (1 -> 2) -> `O +
                  1 -> 0   -> `X 
          

} 

/*
  Parsing notes

  If a Bound has a block, which is a sequence of 0 or more Exprs, and it's possible to end
  an Expr with ;, this must be ambiguous:

  board = 
    `Board0 -> 1 -> 1 -> `X ;
    `Board0 -> 1 -> 2 -> `O ;

  (board = 
    `Board0 -> 1 -> 1 -> `X ;)
    `Board0 -> 1 -> 2 -> `O ;
  vs. 
  (board = 
    `Board0 -> 1 -> 1 -> `X ;
    `Board0 -> 1 -> 2 -> `O ;)

*/
