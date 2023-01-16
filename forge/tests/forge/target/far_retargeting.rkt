#lang forge

option run_sterling off

option problem_type target
option solver PMaxSAT4J
option verbose 0
-- stay as far as possible from the *PRIOR* instance
--   without visiting a previously seen instance
-- TODO: this is currently not working without a provided target
option target_mode far_retarget
sig Node { edges: set Node }
test expect {
  cover_runs: {} for 3 is sat
}


