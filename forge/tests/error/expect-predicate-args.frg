#lang forge/bsl
option verbose 0 
option run_sterling off
abstract sig Player {}
one sig X, O extends Player {}
sig Board {board: pfunc Int -> Int -> Player}

pred wellformed[b: Board] {
  all row, col: Int | {
    (row < 0 or row > 2 or col < 0 or col > 2) 
      implies no b.board[row][col] }}

test expect { should_error: { wellformed } is sat }

