#lang forge

/*
The 'varlinear strategy will break one A's r by bounds,
but use the default formulas to make sure all As' rs are linear.

Since there are 3 Xs, there are 3! = 6 ways the unfixed A could
linearly order its Xs relative to the fixed A's, so we get 6 instances.
*/

sig X {}
--sig A { r: X->X }
/*$ (declare-sig A ((r X X))) */

fact r: varlinear

run {} for exactly 2 A, exactly 3 X
