#lang forge

/* Testing the TOMF functionality of Forge. 
   Since `is sat` is too inexpressive, we define the run and then 
   examine the instance(s) it produces in forge/core. 
*/

option problem_type target
option solver PMaxSAT4J

// Not intended to run, should be called by tomf.rkt. 
// If debugging, comment this line out.
option run_sterling off

// For debugging, if needed
//option engine_verbosity 5
//option verbose 5

sig Node { edges: pfunc Node -> Int }

-- By default: stay as close as possible to first instance
-- To avoid randomly getting the empty instance (which would 
-- then see a different cardinality next time), require some Node
option target_mode close_noretarget
tomf_test_defaults: run {some Node}  

tomf_test_close_noretarget_noNode: run {}
  target_pi {no Node}

// Not supported yet
//tomf_test_far_noretarget_noNode: run {}
//  target_pi {no Node} far_noretarget 

// This confirms that the _partiality_ works: we don't say what 
// the potential other node is connected to, if it exists.
tomf_test_close_noretarget_close_k3: run {}
  target_pi {
    Node = `Node0 + `Node1 + `Node2
    edges = Node -> Node -> 0
  }

inst k3 {
    Node = `Node0 + `Node1 + `Node2
    edges = Node -> Node -> 0
}
tomf_test_use_named_inst: run {}
  target_pi {k3} 

// test in presence of a constraint
tomf_test_close_k3_someIsolated: run {
    some n: Node | no (n.edges) and no ~(edges.Int).n
} target_pi {
    Node = `Node0 + `Node1 + `Node2
    edges = Node -> Node -> 0
} 

// option verbose 5

/////////////////////////////////////////////////////////////////
// Confirm that the PI bounds are respected, though. 
//   N.B.: at the moment, the target must use a SUBSET of the PI bound atoms (this is a Forge-side thing)
//   N.B.: also, the target needs to contain the lower-bound for the run (this is a Pardinus-side thing)
// Together, these actually restrict how we can combine PI-bounds and targets pretty significantly.
tomf_test_close_noretarget_close_k3_with_pi_bounds: run {}
  // Lower/upper bounds in PI
  for {
    // Will be rejected by Pardinus (need verbose 5 to see the error)
    // Node = `Node0 + `Node1 + `Node2
    Node in `Node0 + `Node1 + `Node2
  }
  // Target lower/upper bounds
  target_pi {
    Node = `Node0 + `Node2
    edges = Node -> Node -> 0
  }
/////////////////////////////////////////////////////////////////


// Not supported yet (this example shows bad performance)
// tomf_test_far_noretarget_contains_k3: run {}
//   target_pi {
//     Node = `Node0 + `Node1 + `Node2
//     edges = Node -> Node -> 0
//   } far_noretarget 


// Not supported yet (not necessarily reliable)
// tomf_test_close_retarget_noNode: run {}
//   target_pi {no Node} close_retarget 
// tomf_test_far_retarget_noNode: run {}
//   target_pi {no Node} far_retarget 
// tomf_test_hamming_noNode: run {}
//   target_pi {no Node} hamming_cover

// TODO: at present, the extra relations taint the state for all runs

//////////////////////
// Int Minimization //
//////////////////////

// Minimize number of nodes
tomf_test_close_noretarget_int_numNode: run {}
  minimize_int {#Node} 
// Make sure that the constraints are respected
tomf_test_close_noretarget_int_numNode_gte3: run {#Node >= 3}
  minimize_int {#Node} 


// This setting won't avoid, e.g., (-8 + -8 + -8) even though it "wraps"
// I believe this is because there is no _arithmetic_ involved. 
option no_overflow true

// Should give sum = -8 (mod)
tomf_test_close_noretarget_int_totalWeight4: run {} for exactly 2 Node
  minimize_int {sum m: Node | sum n: Node | m.edges[n]} 

// (Not in .rkt, but here to enable performance testing.)
// Should give sum = -8 (mod)
tomf_test_close_noretarget_int_totalWeight4_6Node: run {} for 6 Node
  minimize_int {sum m: Node | sum n: Node | m.edges[n]} 

// TODO: into .rkt
// What happens if we ask for all edges to have -8 weight? 
//   With no_overflow on, this instance is _rejected_.
tomf_test_close_noretarget_int_totalWeight4_force_many_min: run {
    all n, m: Node | n.edges[m] = -8
} for 3 Node
  minimize_int {sum m: Node | sum n: Node | m.edges[n]} 


// Should give sum = -2 (mod)
tomf_test_close_noretarget_int_totalWeight2: run {} for exactly 2 Node, 2 Int
  minimize_int {sum m: Node | sum n: Node | m.edges[n]}

// Should give sum = -16 (mod)
tomf_test_close_noretarget_int_totalWeight5: run {} for exactly 2 Node, 5 Int
  minimize_int {sum m: Node | sum n: Node | m.edges[n]}

//////////////////////
// Int Maximization //
//////////////////////

// Maximize number of nodes
tomf_test_far_noretarget_int_numNode: run {}
  maximize_int {#Node} 

// Should give sum = 8 (mod)
tomf_test_far_noretarget_int_totalWeight4: run {} for exactly 2 Node
  maximize_int {sum m: Node | sum n: Node | m.edges[n]}

///////////////////////////////////////////
