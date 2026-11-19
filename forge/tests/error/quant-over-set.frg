#lang forge

option run_sterling off

-- This should error: A `set` quantifier that is not effectively existential. 
sig A {}
run { all X: set A | #X > 1 }
