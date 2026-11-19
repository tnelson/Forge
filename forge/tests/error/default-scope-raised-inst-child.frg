#lang forge
option run_sterling off

/*
  Test handling of scopes that are just `for N`, augmented with partial bounds.
  The `for 3` applies to P, but the partial-instance specifies `#C >= 5`. 
  For raises the bound on `P` but emits a warning.
*/

sig P {}
sig C extends P {}

test expect { raised: { #C = 5 } for 3 for { #C >= 5 } is sat }
