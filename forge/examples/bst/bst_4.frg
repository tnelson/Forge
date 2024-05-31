#lang forge/temporal
option run_sterling "bst_temporal.js"

/*
  Model of binary search trees
  Tim, 2024

  Fourth and final version: temporally modeling a search-descent into the tree,
    adding an assertion to check correctness.

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
--bstdiff: run {not { binary_search_tree_v1 iff binary_search_tree_v2}} for 5 Node 
-- These definitely not the same. Let's explore the impact of the difference.

----------------------------------------------------------------------------------

-- Since a BST descent doesn't need to backtrack, the state can be fairly simple.
one sig SearchState {
    target: one Int,         -- fixed: the target of the search
    var current: lone Node  -- variable: the node currently being visited
}

-- Initial-state predicate for the search
pred init {    
    -- Start at the root of the tree.
    -- This formulation relies on uniqueness of the root, enforced elsewhere
    SearchState.current = {n: Node | all other: Node-n | other in n.^(left+right)}
    -- No constraints on the target value
}

-- Transition predicates: descend from the current node into one of its children.
pred descendLeft {
  -- GUARD 
  SearchState.target < SearchState.current.key
  some SearchState.current.left
  -- ACTION
  SearchState.current' = SearchState.current.left
}
pred descendRight {
  -- GUARD 
  SearchState.target > SearchState.current.key
  some SearchState.current.right
  -- ACTION
  SearchState.current' = SearchState.current.right
}
-- Transition predicate: found target or a leaf; either way the search is over.
pred stop {
  -- GUARD 
  SearchState.target = SearchState.current.key or 
  (SearchState.target > SearchState.current.key and no SearchState.current.right) or 
  (SearchState.target < SearchState.current.key and no SearchState.current.left)
  -- ACTION (frame: do nothing)
  SearchState.current' = SearchState.current
}

-- VALIDATION
test expect {
    -- let's check that these 3 transitions are mutually-exclusive
    r_l_together: {eventually {descendLeft and descendRight}} for 7 Node is unsat
    l_stop_together: {eventually {descendLeft and stop}} for 7 Node is unsat
    r_stop_together: {eventually {descendRight and stop}} for 7 Node is unsat
    -- let's check that these 3 are all possible to execute
    r_sat: {eventually descendRight} for 7 Node is sat
    l_sat: {eventually descendLeft} for 7 Node is sat
    stop_sat: {eventually stop} for 7 Node is sat
}

pred searchTrace {
  init
  always {descendLeft or descendRight or stop}
}

-- Let's look at traces of the search using each version of the invariant. 
-- If you use the custom visualizer, *visited* nodes will have a red border, 
-- and *target* node(s) will have a thick border.

-- We'll make this a bit more interesting, and tell Forge:
--   + not to show us immediate success/failure traces; and
--   + to show us traces where the target is present in the tree
// run {
//   some Node             -- non-empty tree
//   binary_search_tree_v1 -- use first invariant version
//   searchTrace           -- do a search descent
//   not stop              -- don't *immediately* succeed 
//   SearchState.target in Node.key -- the target is present
// } for exactly 8 Node
-- And the same using version 2:
// run {
//   some Node             -- non-empty tree
//   binary_search_tree_v2 -- use second invariant version
//   searchTrace           -- do a search descent
//   not stop              -- don't *immediately* succeed 
//   SearchState.target in Node.key -- the target is present
// } for exactly 8 Node    -- don't *immediately* succeed 

-- Use "Next Config" to move to a different tree example.
-- One of the two should eventually produce an instance witnessing the _failure_ of 
-- binary search: a target in the tree that is never found.

----------------------------------------------------------------------------------
-- Finally, let's write an assertion that the first invariant does in fact work.
pred bs_correct {
    all n: Node | {
        n.key = SearchState.target => 
          eventually SearchState.current = n
    }
}
pred lasso_v1 {
  binary_search_tree_v1 -- use first invariant version
  searchTrace           -- generate any arbitrary search trace
}
assert lasso_v1 is sufficient for bs_correct for 5 Node

-- Great! The check passed (at least, up to 5 nodes)