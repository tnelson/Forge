#lang forge
option run_sterling "bst.js"

/*
  Model of binary search trees
  Tim, 2024
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
  -- no cycles (modified)
  all n: Node | 
    n not in descendantsOf[n] 
  -- connected via finite chain of left, right, and inverses
  all disj n1, n2: Node | n1 in n2.^(left + right + ~left + ~right)
  -- left+right differ (unless both are empty)
  all n: Node | some n.left => n.left != n.right 
  -- nodes have a unique parent (if any)
  all n: Node | lone parent: Node | n in parent.(left+right)
}
--run {binary_tree} for 7 Node



run {binary_tree} for exactly 10 Node

-- original v allowed self-loops in the root! 

/*
option solver MiniSat -- use native solver (menu item in Alloy)


pred req_unique_root { 
  {binary_tree and some Node} implies 
    {one root: Node | 
      all other: Node-root | other in descendantsOf[root]}}
test expect { 
  {not req_unique_root} for 7 Node is unsat 
}
*/
-------------
/*
pred binary_search_tree {
  binary_tree  -- a binary tree, with an invariant
  all n: Node | {
    -- "Every node's descendents..."
    all d: n.left.*(left+right) | d.key < n.key
    all d: n.right.*(left+right) | d.key > n.key
  }
}
pred binary_search_tree_alternate {
  binary_tree  -- a binary tree, with an invariant
  all n: Node | {
    -- "Every node's immediate children..."
    some n.left implies n.left.key < n.key
    some n.right implies n.right.key > n.key
  }
}
-- examples of the difference between the two
bstdiff: run {not { binary_search_tree iff binary_search_tree_alternate}} for 5 Node 


*/


/*
--   height: one Int,  -- defined below
pred heights {
  all n: Node | { 
    n.height = add[max[n.left.height+n.right.height], 1] }}
pred balanced {
  -- child subtree heights differ by at most one
  all n: Node | let diff = minus[n.left.height, n.right.height] | { 
    diff <= 1 and diff >= -1}}
run example_balanced { 
  binary_search_tree heights balanced} for 7 Node expect 1

-- Validation: expect 3-node chain to not be balanced
assert req_three_node_chain_unbalanced {
  {binary_tree
   heights
   some disj n1, n2, n3: Node | { 
     no n: Node | n not in (n1+n2+n3)
     n2 = n1.left and n3 = n2.left 
   }} implies not balanced
}
check req_three_node_chain_unbalanced for 7 Node

*/ 
