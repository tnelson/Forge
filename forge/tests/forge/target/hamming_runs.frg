#lang forge

option run_sterling off

option problem_type target
option solver PMaxSAT4J
option verbose 0
option target_mode hamming_cover
sig Node { edges: set Node }
test expect {
  cover_runs: {} for 3 is sat
}
