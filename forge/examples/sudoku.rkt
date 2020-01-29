#lang forge

-- SUDOKU
-- Find me unique-solution boards

sig N {}
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

pred loneNumberPerCell {
  all b: Board | all i: N | all j: N | lone i.(j.(b.places))
}

pred filled[b: Board, n: Int] {
  #b.places = n
}
pred tenFilled {
  some b: Board | filled[b, 10]  
}
pred solved {
  some b: Board | {
    all i: N | i.(b.places) = N
    all i: N | {x : N | some j : N | j->i->x in b.places} = N
    ^ // bad syntax???
  }
}

--base: run {oneNumberPerCell} for 2 Board, 9 N
-- tenfilled: run {tenFilled loneNumberPerCell} for 2 Board, 9 N, 8 Int
solved: run {solved loneNumberPerCell} for 2 Board, 9 N, 8 Int
