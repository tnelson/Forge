#lang forge

option run_sterling off
option verbose 0
option test_keep last


sig N {children: set N}

pred p {

}


// Consistency
assert p is consistent with {}
