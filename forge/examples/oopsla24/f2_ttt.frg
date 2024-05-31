#lang froglet 
-- ^ A.K.A. #lang forge/bsl in many tests, etc.

abstract sig Player {}
one sig X , O extends Player {}
sig Board { board : pfunc Int -> Int -> Player }
pred wellformed {
  all b : Board | all row , col : Int | { 
	( row < 0 or row > 2 or col < 0 or col > 2) implies no b.board [row][col] 
  } 
}

