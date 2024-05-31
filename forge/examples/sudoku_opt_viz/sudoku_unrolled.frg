#lang forge

/*
  Example for exploring various optimizations
  VERSION 3: extending the optimizer instance a bit
*/

abstract sig BoardState {
    board: pfunc Int -> Int -> Int
}
one sig PuzzleState extends BoardState {}
one sig SolvedState extends BoardState {}

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

-- There are 9 subgrids; define them as constants
fun subgrids: set Int -> Int -> Int {
    (1 -> (1 + 2 + 3) -> (1 + 2 + 3)) +
    (2 -> (1 + 2 + 3) -> (4 + 5 + 6)) +
    (3 -> (1 + 2 + 3) -> (7 + 8 + 9)) +
    (4 -> (4 + 5 + 6) -> (1 + 2 + 3)) +
    (5 -> (4 + 5 + 6) -> (4 + 5 + 6)) +
    (6 -> (4 + 5 + 6) -> (7 + 8 + 9)) +
    (7 -> (7 + 8 + 9) -> (1 + 2 + 3)) +
    (8 -> (7 + 8 + 9) -> (4 + 5 + 6)) +
    (9 -> (7 + 8 + 9) -> (7 + 8 + 9))
}

fun values: set Int {
    (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9)
}

fun get_grid[s: BoardState, subgrid: Int]: set Int {
    let indexes = subgrids[subgrid] |
    let rows = indexes.Int |
    let cols = indexes[Int] |
        s.board[rows][cols]
}

pred solution[s: PuzzleState] {
    -- ** Rows and Columns **
    -- don't use #... = 9 here; instead something like:
    all r: values | s.board[r][Int] = values
    all c: values | s.board[Int][c] = values
    -- note this is a situation where the relational perspective can
    -- help make your constraints more readable, rather than less so.

    -- ** Subgrids **
    all subgrid: values | 
        get_grid[s, subgrid] = values

}

pred solve {
    PuzzleState.board in SolvedState.board
    solution[SolvedState]
}


/*
  
Old optimizer:
  #vars: (size-variables 10943); #primary: (size-primary 1458); #clauses: (size-clauses 29422)
  Transl (ms): (time-translation 472); Solving (ms): (time-solving 6321)
New (with grids):
  #vars: (size-variables 10943); #primary: (size-primary 1458); #clauses: (size-clauses 29422)
  Transl (ms): (time-translation 271); Solving (ms): (time-solving 3146)

Notice that the problem size is still the same. Why? 
If I had to guess, I'd say that perhaps Forge handled the arithmetic
in the translation process. That time did decrease. But it's tough
to rely on timing like this, since it's not always deterministic. 
To be honest, I'm not immediately sure why the solving time also 
decreased. I think this is a better way of encoding the "neighborhood"
of each cell, regardless---since there's less risk of solver impact.
*/

/*run {
    solve
    #PuzzleState.board = 7 // 7 pre-populated cells
} for 2 BoardState, 5 Int for optimizer
*/
/*
  Let's try solving a "hard" Sudoku puzzle
Basic version was:
  #vars: (size-variables 232156); #primary: (size-primary 65536); #clauses: (size-clauses 367445)
  Transl (ms): (time-translation 1815); Solving (ms): (time-solving 645)
Now:
  #vars: (size-variables 5483); #primary: (size-primary 1458); #clauses: (size-clauses 7993)
  Transl (ms): (time-translation 318); Solving (ms): (time-solving 1708)

Note that even though the time-to-solve is comparable, the work has shifted
from the translation to the solver. 

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
// run {
//     solve
//     puzzle1
// } for 2 BoardState, 5 Int for optimizer

/*
    We'd expect 9*9*9=729 variables per board. We've got 1458 for 
    2 boards (one is fully defined, but not via bounds). That's
    exactly what we'd expect. If it wasn't, we could investigate     
    by looking at the bounds passed to the solver via `option verbose`.

    But wait---why do we have variables for the first board at all?
    Because `puzzle` is a predicate. Ideally, we'd give it as a bound
    via `inst`. But this is challenging:
      * the PuzzleState's board is known exactly; but
      * the SolvedState's board is only UPPER bounded.
    We can solve this in a few ways. Here's one:
*/


inst optimizer_puzzle1 {
    -- Just 2 board states (don't name the atoms the same as the sigs)
    PuzzleState = `PuzzleState0
    SolvedState = `SolvedState0
    BoardState = PuzzleState + SolvedState
    
    -- UPPER BOUND
    board in 
           -- flexible for SolvedState, but on interval [1,9]:
           (SolvedState -> 
             (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9) -> 
             (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9) -> 
             (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9))
           +
           -- fully known for PuzzleState:
           (PuzzleState ->
             ((1 -> 1 -> 6) + (1 -> 4 -> 9) + (1 -> 9 -> 8) +
             (2 -> 7 -> 4) + (2 -> 8 -> 3) +
             (3 -> 1 -> 1) + (3 -> 5 -> 4) + (3 -> 8 -> 9) +
             (4 -> 1 -> 5) + (4 -> 4 -> 8) + (4 -> 9 -> 7) +
             (6 -> 2 -> 1) + (6 -> 6 -> 2) + (6 -> 9 -> 9) +
             (7 -> 2 -> 7) + (7 -> 4 -> 5) + (7 -> 9 -> 1) +
             (8 -> 2 -> 3) + (8 -> 4 -> 4) +
             (9 -> 2 -> 2) + (9 -> 5 -> 8) + (9 -> 7 -> 7) + (9 -> 9 -> 3)))

    -- LOWER BOUND
    board ni 
      -- both PuzzleState and SolvedState need to contain these:
      (BoardState ->
             ((1 -> 1 -> 6) + (1 -> 4 -> 9) + (1 -> 9 -> 8) +
             (2 -> 7 -> 4) + (2 -> 8 -> 3) +
             (3 -> 1 -> 1) + (3 -> 5 -> 4) + (3 -> 8 -> 9) +
             (4 -> 1 -> 5) + (4 -> 4 -> 8) + (4 -> 9 -> 7) +
             (6 -> 2 -> 1) + (6 -> 6 -> 2) + (6 -> 9 -> 9) +
             (7 -> 2 -> 7) + (7 -> 4 -> 5) + (7 -> 9 -> 1) +
             (8 -> 2 -> 3) + (8 -> 4 -> 4) +
             (9 -> 2 -> 2) + (9 -> 5 -> 8) + (9 -> 7 -> 7) + (9 -> 9 -> 3)))
}

/*
  Better (in terms of problem size):
#vars: (size-variables 2296); #primary: (size-primary 706); #clauses: (size-clauses 2786)
Transl (ms): (time-translation 528); Solving (ms): (time-solving 1869)
  
  But still not really ideal! There's a lot of branching happening.
  

*/

run {
    solve
} for 2 BoardState, 5 Int for optimizer_puzzle1
