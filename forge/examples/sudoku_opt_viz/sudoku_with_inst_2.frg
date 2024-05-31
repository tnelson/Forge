#lang forge

/*
  Example for exploring various optimizations
  VERSION 3: extending the optimizer instance a bit
*/

-- Pre-load the visualizer script in Sterling
option run_sterling "sudoku.js"
-- For ease of debugging and demoing, use a constant Forge server port
option sterling_port 17100


abstract sig BoardState {
    board: pfunc Int -> Int -> Int
}
one sig PuzzleState extends BoardState {}
one sig SolvedState extends BoardState {}

-- Useful way to add helper relations 
one sig Helper {
    -- encode the set of values in bounds, not constraints
    values: set Int
}

inst optimizer {
    -- Just 2 board states (don't name the atoms the same as the sigs)
    PuzzleState = `PuzzleState0
    SolvedState = `SolvedState0
    BoardState = PuzzleState + SolvedState
    -- Upper-bound on the board relation: don't even try to use
    -- a row, column, or value that's outside the interval [1, 9]
    board in BoardState -> 
             (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9) -> 
             (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9) -> 
             (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9)

    -- encode the set of values in bounds, not constraints
    Helper = `Helper0
    values = Helper -> (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9)
}

fun get_grid[s: BoardState, row_offset, col_offset: Int]: set Int {
    let rows = row_offset + add[row_offset, 1] + add[row_offset, 2] |
    let cols = col_offset + add[col_offset, 1] + add[col_offset, 2] |
        s.board[rows][cols]
}

pred solution[s: PuzzleState] {
    -- ** Rows and Columns **
    -- don't use #... = 9 here; instead something like:
    all r: Helper.values | s.board[r][Int] = Helper.values
    all c: Helper.values | s.board[Int][c] = Helper.values
    -- note this is a situation where the relational perspective can
    -- help make your constraints more readable, rather than less so.

    -- ** Subgrids **
    all row_offset, col_offset: (1 + 4 + 7) | 
        get_grid[s, row_offset, col_offset] = Helper.values

}

pred solve {
    PuzzleState.board in SolvedState.board
    solution[SolvedState]
}


/*
  IN THIS CASE, we don't even need wellformedness anymore, 
  if we're using optimizer. Normally, I'd keep the wellformed
  predicate around (because it's not always the case that 
  we want to use optimizer for all runs/tests).

Old optimizer:
  #vars: (size-variables 10943); #primary: (size-primary 1458); #clauses: (size-clauses 29422)
  Transl (ms): (time-translation 472); Solving (ms): (time-solving 6321)
New optimizer:
  #vars: (size-variables 10943); #primary: (size-primary 1458); #clauses: (size-clauses 29422)
  Transl (ms): (time-translation 445); Solving (ms): (time-solving 5370)

Interesting! The bounds aren't helping reduce the size of the problem.
Forge already had the constant (1 + ... + 9) in place. 
*/
run {
    solve
    #PuzzleState.board = 7 // 7 pre-populated cells
} for 2 BoardState, 5 Int for optimizer

/*
  The "#PuzzleState.board = 7" is far more suspicious. Cardinality
  is expensive to encode into SAT, and 7 is fairly high. Just how much 
  of the translation and solving time is due to the "populate 
  exactly 7 squares in the puzzle" constraint?

  #vars: (size-variables 5480); #primary: (size-primary 1458); #clauses: (size-clauses 7261)
  Transl (ms): (time-translation 225); Solving (ms): (time-solving 6747)

  The problem got simpler, but the solver time went up. :-( 
*/
run {
    solve
    --#PuzzleState.board = 7 
} for 2 BoardState, 5 Int for optimizer
