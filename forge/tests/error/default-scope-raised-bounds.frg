#lang forge
option run_sterling off

/*
  Test handling of scopes that are just `for N`. 
  Here, three `one` children need 3 atoms in P.
*/


sig P {}
one sig A, B, C extends P {}

test expect { raised: { } for 2 is sat }
