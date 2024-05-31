# Sudoku Solver / Synthesizer Example

In this example, I try to optimize a sudoku model. I pass through multiple versions:
* sudoku_basic.frg
* sudoku_with_inst.frg
* sudoku_with_inst2.frg
* sudoku_unrolled.frg
* sudoku_rethink.frg

Most files contain the first thing I tried: just generating a pair of unsolved/solved boards, with the unsolved board has 7 squares populated. The first and last file contain a run command to solve a hard sudoku puzzle.

The repo also contains an example of a (rough) visualization:
* sudoku.js 

Note the visualization is meant for the two-board model, not the single-board model! It should illustrate how to get at fields, draw lines, print relations, etc.
