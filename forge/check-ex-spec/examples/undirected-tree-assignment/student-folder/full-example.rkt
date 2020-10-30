#lang forge/check-ex-spec "undirected-tree"

------------------------------
-- Write a predicate undirectedTree[r: Node -> Node]
--  that evaluates to true if and only if the input
--  relation <r> is an undirected tree. 
------------------------------

-- Should the empty tree produce true?
example EmptyTreeWorks is undirectedTree[edges] for {
  no Node
  no edges
}

-- A more complicated tree
example UndirectedTreePositive1 is undirectedTree[edges] for {
    Node = Node1 + Node2 + Node3 + Node4

    edges = (Node1 -> (Node2 + Node3 + Node4)) +
           ~(Node1 -> (Node2 + Node3 + Node4))
}

inst BasicExample {
    Node = Node1 + Node2 + Node3 + Node4

    edges = (Node1 -> (Node2 + Node3 + Node4)) +
           ~(Node1 -> (Node2 + Node3 + Node4))
}

example UndirectedPositive2 is undirectedTree[edges] for BasicExample

example CompleteInvalid is not undirectedTree[Node -> Node] for BasicExample
