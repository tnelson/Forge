#lang forge

-- Model that produces multiple distinct instances for testing navigation
sig Node {
  edge: lone Node
}

-- With 3 nodes and lone edge, we get many possible configurations
multiRun: run {
  some edge
} for exactly 3 Node
