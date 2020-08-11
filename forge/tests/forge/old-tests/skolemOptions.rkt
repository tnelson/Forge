#lang forge

option verbosity 2
option sb 0
option coregranularity 2
option logtranslation 2
option solver MiniSatProver

sig Node {
    edges: set Node
}

-- run {} for exactly 2 Node -- 16 instances without SB (2^4); 10 with SB 
--run {no iden & edges} for exactly 2 Node -- 3 instances with SB; 4 without SB (the extra single-edge instance)
--run {some n: Node | some n.edges} for 2 Node

test expect {
  {some n: Node | some n.edges} for 2 Node is sat -- no errors for Skolemization
  {some Node} for 2 Node is sat -- no errors for non-Skolemization
}