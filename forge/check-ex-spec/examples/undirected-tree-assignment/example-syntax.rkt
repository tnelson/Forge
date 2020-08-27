#lang forge/check-ex-spec "undirected-tree"

example AValidInstance is undirectedTree[edges] for {
    Node = Node1 + Node2 + Node3 + Node4

    edges = (Node1 -> (Node2 + Node3 + Node4)) +
           ~(Node1 -> (Node2 + Node3 + Node4))
}


inst BasicExample {
    Node = Node1 + Node2 + Node3 + Node4

    edges = (Node1 -> (Node2 + Node3 + Node4)) +
           ~(Node1 -> (Node2 + Node3 + Node4))
}

example AValidInstance is undirectedTree[edges] for BasicExample