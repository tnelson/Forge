#lang forge
option run_sterling "bst.js"

/*
  Model of binary search trees
  Tim, 2024

  Second version: binary trees with the BST invariant

  Note assumption: this model doesn't take duplicate entries into account.
*/

sig Node {
  key: one Int,     -- every node has some key 
  left: lone Node,  -- every node has at most one left-child
  right: lone Node  -- every node has at most one right-child
}
fun descendantsOf[ancestor: Node]: set Node {
  ancestor.^(left + right) -- nodes reachable via transitive closure
}
pred binary_tree {
  -- no cycles
  all n: Node | n not in descendantsOf[n] 
  -- connected via finite chain of left, right, and inverses
  all disj n1, n2: Node | n1 in n2.^(left + right + ~left + ~right)
  -- left+right differ (unless both are empty)
  all n: Node | some n.left => n.left != n.right 
  -- nodes have a unique parent (if any)
  all n: Node | lone parent: Node | n in parent.(left+right)
}

-- View a tree or two
-- run {binary_tree} for exactly 10 Node

-- Run a test: our predicate enforces a unique root exists (if any node exists)
pred req_unique_root {   
  no Node or {
    one root: Node | 
      all other: Node-root | other in descendantsOf[root]}}
assert binary_tree is sufficient for req_unique_root for 5 Node  

---------------------------------------------------------------------------------

-- We have two potential predicates that might represent the ordering invariant.
-- One is correct, and the other is a common misunderstanding.

pred invariant_v1[n: Node] {
  -- "Every node's left-descendants..." via reflexive transitive closure
  all d: n.left.*(left+right)  | d.key < n.key
  -- "Every node's left-descendants..." via reflexive transitive closure
  all d: n.right.*(left+right) | d.key > n.key
}
pred binary_search_tree_v1 {
  binary_tree  -- a binary tree, with an added invariant
  all n: Node | invariant_v1[n]  
}
pred invariant_v2[n: Node] {
  -- "Every node's immediate children..."
  some n.left implies n.left.key < n.key
  some n.right implies n.right.key > n.key
}
pred binary_search_tree_v2 {
  binary_tree  -- a binary tree, with an added invariant
  all n: Node | invariant_v2[n]
}

-- Get examples of the difference between the two. 
bstdiff: run {not { binary_search_tree_v1 iff binary_search_tree_v2}} for 5 Node 
-- These definitely not the same. Let's explore the impact of the difference.
