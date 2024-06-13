#lang forge

/*
  Example for exploring various optimizations
  VERSION 2: adding an optimizer instance for `board`
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
Just by adding the optimizer partial instance, we see:
#vars: (size-variables 11096); #primary: (size-primary 1458); #clauses: (size-clauses 29422)
Transl (ms): (time-translation 356); Solving (ms): (time-solving 10463)
* major reduction in problem size
* >2 performance improvement: ~30sec -> ~11sec.
*/
run {
    wellformed
    solve
    #PuzzleState.board = 7 // 7 pre-populated cells
} for 2 BoardState, 5 Int for optimizer

/*
  We don't even need wellformedness anymore, if we're using optimizer

#vars: (size-variables 10943); #primary: (size-primary 1458); #clauses: (size-clauses 29422)
Transl (ms): (time-translation 472); Solving (ms): (time-solving 6321)

*/
run {
    solve
    #PuzzleState.board = 7 // 7 pre-populated cells
} for 2 BoardState, 5 Int for optimizer
