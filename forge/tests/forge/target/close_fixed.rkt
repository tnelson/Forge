#lang forge
option problem_type target
option solver PMaxSAT4J
option verbose 0
-- stay as close as possible to first instance
option target_mode close_noretarget
sig Node { edges: set Node }
test expect { 
  cover_runs: {} for 3 is sat
}

--run {} for 3
