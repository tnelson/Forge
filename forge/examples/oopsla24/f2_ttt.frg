#lang forge/froglet 

// NOTE WELL: This was "froglet" in the OOPSLA24 paper; we have since renamed the 
// starter language to be named consistently with Racket style.

abstract sig Player {}
one sig X , O extends Player {}
sig Board { board : pfunc Int -> Int -> Player }
pred wellformed {
  all b : Board | all row , col : Int | { 
	( row < 0 or row > 2 or col < 0 or col > 2) implies no b.board [row][col] 
  } 
}

