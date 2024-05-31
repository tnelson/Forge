#lang forge

/*
  Example for exploring various optimizations
  VERSION 4: rethinking the model

  Pure SAT is known to solve hard sudoku puzzles in 10-100ms.
  We're taking an order of magnitude longer than we should be!
  Let's try a different way of encoding the problem: only *one* board.
*/

one sig Board {
    board: pfunc Int -> Int -> Int
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

fun get_grid[subgrid: Int]: set Int {
    let indexes = subgrids[subgrid] |
    let rows = indexes.Int |
    let cols = indexes[Int] |
        Board.board[rows][cols]
}

pred solution {

    -- ** Rows and Columns **
    -- don't use #... = 9 here; instead something like:
    all r: values | Board.board[r][Int] = values
    all c: values | Board.board[Int][c] = values
    -- note this is a situation where the relational perspective can
    -- help make your constraints more readable, rather than less so.

    -- ** Subgrids **
    all subgrid: values | 
        get_grid[subgrid] = values

}

inst optimizer_puzzle1 {
    Board = `Board0
    
    -- There's a small additional chance for optimization here, but 
    --  it seems like a lot of work: RELATIONS are a problem here! 
    --  The upper bound is too big---unaware of the fact that nothing
    --  else can co-exist in the cells we already have values for.

    -- UPPER BOUND
    board in            
           (Board -> 
             (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9) -> 
             (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9) -> 
             (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9))

    -- LOWER BOUND
    board ni 
      (Board ->
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
  Interestingly, even this small change -- one board only -- helps

  #vars: (size-variables 2296); #primary: (size-primary 706); #clauses: (size-clauses 2786)
  Transl (ms): (time-translation 244); Solving (ms): (time-solving 758)

  It's possible some of this is setup/teardown cost for the solver or 
  the translation infrastructure. But process startup shouldn't be 
  included. So this still seems quite high.
*/
/*
run {
    solution
} for 1 Board, 5 Int for optimizer_puzzle1
*/
/*
  Let's try a different way to express the problem:

#vars: (size-variables 5292); #primary: (size-primary 706); #clauses: (size-clauses 7155)
Transl (ms): (time-translation 330); Solving (ms): (time-solving 153)

  The problem got bigger---in terms of clauses. But the solver
    had much more luck solving it. 

  What's the lesson here? While smaller problems are *often* better, especially
  for the translation time, an encoding with more clauses can give the solver
  the structure it needs to solve the problem more efficiently. Don't be afraid
  to try different ways of expressing your goal.
*/

pred solution2 {
    -- There is an entry in every cell
    --   (In a separate file, this would just mean changing the type
    --      of board to func, rather than pfunc)
    all r, c: values | some Board.board[r][Int]
    -- Each row and col contain at most one of every value
    all val, r: values | lone c : values | Board.board[r][c] = val
    all val, c: values | lone r : values | Board.board[r][c] = val

    all subgrid, val: values | val in get_grid[subgrid]

}
run {
    solution2
} for 1 Board, 5 Int for optimizer_puzzle1


