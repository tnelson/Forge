#lang forge

option run_sterling off

option problem_type target
option solver PMaxSAT4J
option verbose 0

-- stay as far as possible from first instance
-- TODO: currently not functional without a provided target
option target_mode far_noretarget
sig Node { edges: set Node }
// test expect { 
//   cover_runs: {} for 3 is sat
// }

tomf_far_fixed: run {}
target_pi {no Node} far_noretarget 
