#lang forge/check-ex-spec "undirected-tree"

inst BasicExample {
    Node = Node1 + Node2 + Node3 + Node4

    edges = (Node1 -> (Node2 + Node3 + Node4)) +
           ~(Node1 -> (Node2 + Node3 + Node4))
}

test expect MyTests {
    InstIsValid : undirected-tree[edges] for basic-example is sat
    CompleteGraph : undirected-tree[Node -> Node] for basic-example is unsat
}