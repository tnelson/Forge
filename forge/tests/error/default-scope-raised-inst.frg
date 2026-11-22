#lang forge
option run_sterling off

/*
  Test handling of scopes that are just `for N`, augmented with partial bounds. 
  Here, `#A >= 6` in the inst needs 6 atoms.
*/
 
sig A {}

test expect { raised: { #A = 6 } for 5 for { #A >= 6 } is sat }
