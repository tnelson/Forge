#lang forge

-- VERSION FOR AUTOMATED TESTING

sig Node {
  edges: set Node
}

pred outNexus {
  some n: Node | all n2: Node | n->n2 in edges
}

pred inNexus {
  all n: Node | some n2: Node | n->n2 in edges
}

--anygraph : run {} for 5 Node
run inNexus for exactly 5 Node
--allSomeExchange : run {outNexus and not inNexus} for 5 Node
