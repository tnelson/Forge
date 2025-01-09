#lang forge

option run_sterling off
option verbose 0
option test_keep last


sig N {children: set N}

pred p {

}


// Quantified assertion
assert all n : N | p is necessary for (no children)
