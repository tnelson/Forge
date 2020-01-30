#lang forge

-- SUDOKU
-- Find me unique-solution boards

sig N {neighbors: set N}
one sig N1 extends N {}
one sig N2 extends N {}
one sig N3 extends N {}
one sig N4 extends N {}
one sig N5 extends N {}
one sig N6 extends N {}
one sig N7 extends N {}
one sig N8 extends N {}
one sig N9 extends N {}


sig Board {
  --places: set Int -> Int -> Int
  places: set N -> N -> N
}

/*
pred intbounds {
  all b: Board | all i: Int | all j: Int | all k: Int | i->j->k in b.places implies {
    i > 0
    j > 0
    k > 0
    i <= 9
    j <= 9
    k <= 9
  }
}*/

-- 3 int = [-4, 3]
-- 4 int = [-8, 7]
-- 5 int = [-16, 15]
--run {intbounds} for 2 Board, 5 Int

pred structural {
  -- lone number per cell
  all b: Board | all i: N | all j: N | lone i.(j.(b.places))

  -- neighbors
  N1.neighbors = N1+N2+N3
  N2.neighbors = N1+N2+N3
  N3.neighbors = N1+N2+N3
  N4.neighbors = N4+N5+N6
  N5.neighbors = N4+N5+N6
  N6.neighbors = N4+N5+N6
  N7.neighbors = N7+N8+N9
  N8.neighbors = N7+N8+N9
  N9.neighbors = N7+N8+N9
}

pred filled[b: Board, n: Int] {
  #b.places = n
}
pred tenFilled {
  some b: Board | filled[b, 10]  
}
pred solved[b: Board] {  
    all i: N | N.(i.(b.places)) = N // every row, taking all columns
    all i: N | i.(N.(b.places)) = N // every column, taking all rows  //N in {x : N | some j : N | j->i->x in b.places } 
    // and every sub-block (inefficient way of phrasing it)
    all i: N | all j: N | (j.neighbors).((i.neighbors).(b.places)) = N
}
pred someSolved {
  some b: Board | solved[b]
}
--base: run {structural} for 2 Board, 9 N
-- tenfilled: run {tenFilled structural} for 2 Board, 9 N, 8 Int
--solved: run {someSolved structural} for 2 Board, 9 N, 8 Int

pred generatePuzzle {
  structural
  some init: Board |
  some final: Board | {
      -- lesson on performance: try commenting this out...
    init != final -- massively helpful, even if it's a consequence of the other constraints
    -- ? maybe even faster if we constrain init to be pre-valid? not =N, but no dupes?    
    filled[init, 10]
    init.places in final.places
    solved[final]     
  }
}
run {generatePuzzle} for 2 Board, 9 N, 8 Int

-- Finding a VALID puzzle would be awesome (unique solution up to isomorphism) but that seems to require 2nd order logic + cleverness around isomorphism