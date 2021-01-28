#lang forge

option solver MiniSatProver
option verbosity 0

sig Color {}

sig Node {
  edges: set Node->Color
}

pred wellformed {
  all n: Node | all m: Node | lone n.edges[m]
}

inst myTest {
  Node = Node0 + Node1 + Node2
  Color = Color0 + Color1
  edges = Node0->Node1->Color0 + Node1->Node0->Color1 + Node1->Node2->Color0  
}

inst myTestBigger {
  Node = Node0 + Node1 + Node2 + Node3 + Node4
  Color = Color0 + Color1 + Color2
  edges = Node0->Node1->Color0 + Node1->Node0->Color1 + Node1->Node2->Color0 + Node0->Node3->Color2 + Node4->Node4->Color1
}
inst testCustomNames {
  Node = N5 + N6 + N7
  Color = Blue + Green + Red
  edges = N5->N6->Blue + N6->N7->Red
} 

test expect {
    {} for 3 Node, 3 Color is sat
    {} for exactly myTest is sat
    {} for exactly myTestBigger is sat
    {#Node = 4} for exactly myTestBigger is unsat
    {#Node = 5} for exactly myTestBigger is sat
    {} for exactly testCustomNames is sat
    {some n: Node | all m: Node | m in n.^(edges.Color)} for exactly testCustomNames is unsat
    {some n: Node | some m: Node | m in n.^(edges.Color)} for exactly testCustomNames is sat
}
