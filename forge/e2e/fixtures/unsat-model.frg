#lang forge

-- Model with unsatisfiable constraints for testing unsat display
sig Node {
  edges: set Node
}

-- This is unsatisfiable: can't have exactly 3 nodes where each has no edges but some edges exist
unsatRun: run {
  no edges
  some edges
} for exactly 3 Node
