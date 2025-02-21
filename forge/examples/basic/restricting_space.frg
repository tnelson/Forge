#lang forge

/*
  Example of restricting search space in Forge. 
  The Sudoku example contains a more involved version of this. 
*/

one sig Example {
    f: pfunc Int -> Int
}

run { } 
  for 10 Int // [-512,511]
  for {
    Example = `Example
    // Relational Forge will allow this sort of binding annotation:
    `Example.f in (0+1+2+3+4+5+6+7+8+9) -> (0+1+2+3+4+5+6+7+8+9)
  }