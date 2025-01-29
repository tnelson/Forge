#lang forge 
option verbose 0

// Detect when sufficient bounds have not been provided. 

sig Match {}
sig B, C extends Match {} 

test expect {
    should_error: {} for exactly 4 B, 3 C is sat
}

