#lang forge
option problem_type target
option solver PMaxSAT4J
option verbose 0
-- stay as close as possible to the *PRIOR* instance
option target_mode close_retarget
sig Node { edges: set Node }
test expect {
  cover_runs: {} for 3 is sat
}

-- run {} for 3

