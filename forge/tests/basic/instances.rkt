#lang forge

option solver MiniSatProver
option verbosity 5

sig Color {}

sig Node {
  edges: set Node->Color
}

pred wellformed {
  all n: Node | all m: Node | lone n.edges[m]
}

inst myTestDefaultScope {
  Node = Node0 + Node1 + Node2
  Color = Color0 + Color1
  edges = Node0->Node1->Color0 + Node1->Node0->Color1 + Node1->Node2->Color0  
}

inst myTestCustomScope {
  Node = Node0 + Node1 + Node2
  Color = Color0 + Color1
  edges = Node0->Node1->Color0 + Node1->Node0->Color1 + Node1->Node2->Color0  
} for 3 Node, 3 Color

test expect {
    {} for 3 Node, 3 Color is sat
}