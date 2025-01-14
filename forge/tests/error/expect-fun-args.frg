#lang forge/froglet
option verbose 0 
option run_sterling off
abstract sig Player {}
one sig X, O extends Player {}
sig Board {board: pfunc Int -> Int -> Player}

fun playerAt[b: Board, row, col: Int]: lone Player {
  b.board[row][col] }

test expect { should_error: { some playerAt } is sat }

