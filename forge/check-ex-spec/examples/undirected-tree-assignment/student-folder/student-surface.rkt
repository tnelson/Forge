#lang forge/check-ex-spec "undirected-tree"

example AValidInstance1 is undirectedTree[edges] for {
    Node = Node1 + Node2 + Node3 + Node4

    edges = (Node1 -> (Node2 + Node3 + Node4)) +
           ~(Node1 -> (Node2 + Node3 + Node4))
}


inst BasicExample {
    Node = Node1 + Node2 + Node3 + Node4

    edges = (Node1 -> (Node2 + Node3 + Node4)) +
           ~(Node1 -> (Node2 + Node3 + Node4))
}

example AValidInstance2 is undirectedTree[edges] for BasicExample

example CompleteInvalid is not undirectedTree[Node -> Node] for BasicExample

test expect MyTests {
    InstIsValid : {undirectedTree[edges]} for BasicExample is sat
    CompleteGraph : {undirectedTree[Node -> Node]} for BasicExample is unsat
}