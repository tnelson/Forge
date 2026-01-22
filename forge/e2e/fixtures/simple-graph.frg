#lang forge

-- Simple test model for e2e testing
sig Node {
  edges: set Node
}

pred connected {
  all n1, n2: Node | n1 in n2.^edges or n2 in n1.^edges or n1 = n2
}

pred someEdges {
  some edges
}

simpleRun: run {
  someEdges
} for exactly 3 Node

connectedRun: run {
  connected
  someEdges
} for exactly 3 Node
