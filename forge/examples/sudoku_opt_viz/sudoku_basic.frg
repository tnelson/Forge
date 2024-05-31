#lang forge

/*
  Example for exploring various optimizations
  VERSION 1: a first try
*/

abstract sig BoardState {
    board: pfunc Int -> Int -> Int
}
one sig PuzzleState extends BoardState {}
one sig SolvedState extends BoardState {}

pred wellformed {
    -- No <= 0 or >9 values used for indexing or values
    all s: BoardState | 
        all i: Int | (i <= 0 or i > 9) implies {
            no s.board[i] 
            no s.board[Int][i]
            no s.board.i
        }
}


fun get_grid[s: BoardState, row_offset, col_offset: Int]: set Int {
    let rows = row_offset + add[row_offset, 1] + add[row_offset, 2] |
    let cols = col_offset + add[col_offset, 1] + add[col_offset, 2] |
        s.board[rows][cols]
}

fun values: set Int {
    1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9
}

pred solution[s: BoardState] {
    -- ** Rows and Columns **
    -- don't use #... = 9 here; instead something like:
    all r: values | s.board[r][Int] = values
    all c: values | s.board[Int][c] = values
    -- note this is a situation where the relational perspective can
    -- help make your constraints more readable, rather than less so.

    -- ** Subgrids **
    all row_offset, col_offset: (1 + 4 + 7) | 
        get_grid[s, row_offset, col_offset] = values

}

pred solve {
    PuzzleState.board in SolvedState.board
    solution[SolvedState]
}

/*
  #vars: (size-variables 458692); #primary: (size-primary 65536); #clauses: (size-clauses 1399152)
  Transl (ms): (time-translation 4527); Solving (ms): (time-solving 26934)
*/
/*run {
    wellformed
    solve
    #PuzzleState.board = 7 // 7 pre-populated cells
} for 2 BoardState, 5 Int
*/

/* You might think the core problem is wellformed: we dont NEED 
   those constraints; we can just ignore entries outside [1,9]
  Unfortunately:
#vars: (size-variables 457409); #primary: (size-primary 65536); #clauses: (size-clauses 1256181)
Transl (ms): (time-translation 4062); Solving (ms): (time-solving 1901)
*/
/*run {
    -- without wellformed
    solve
    #PuzzleState.board = 7 // 7 pre-populated cells
} for 2 BoardState, 5 Int
*/

/*
  This *would* be more efficient if you were willing to use 
  4 Int (the interval [-7, 8]) and translate in the visualizer.
  but that increases complexity of the model and makes it harder
  to understand. What we can do instead is tell the translation
  process that the `board` relation can't use any negative indexes
  or contain negative values. 
*/

/*
  Let's try solving a "hard" Sudoku puzzle

#vars: (size-variables 232156); #primary: (size-primary 65536); #clauses: (size-clauses 367445)
Transl (ms): (time-translation 1815); Solving (ms): (time-solving 645)

*/ 
pred puzzle1 {
    PuzzleState.board = 
      (1 -> 1 -> 6) + (1 -> 4 -> 9) + (1 -> 9 -> 8) +
      (2 -> 7 -> 4) + (2 -> 8 -> 3) +
      (3 -> 1 -> 1) + (3 -> 5 -> 4) + (3 -> 8 -> 9) +
      (4 -> 1 -> 5) + (4 -> 4 -> 8) + (4 -> 9 -> 7) +
      (6 -> 2 -> 1) + (6 -> 6 -> 2) + (6 -> 9 -> 9) +
      (7 -> 2 -> 7) + (7 -> 4 -> 5) + (7 -> 9 -> 1) +
      (8 -> 2 -> 3) + (8 -> 4 -> 4) +
      (9 -> 2 -> 2) + (9 -> 5 -> 8) + (9 -> 7 -> 7) + (9 -> 9 -> 3)
}

run {
    solve
    puzzle1
} for 2 BoardState, 5 Int

