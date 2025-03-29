#lang forge

/* Testing the TOMF functionality of Forge. 
   Since `is sat` is too inexpressive, we define the run and then 
   examine the instance(s) it produces in forge/core. 
*/

option problem_type target
option solver PMaxSAT4J
// option run_sterling off

sig Node { edges: pfunc Node -> Int }

-- By default: stay as close as possible to first instance
-- To avoid randomly getting the empty instance (which would 
-- then see a different cardinality next time), require some Node
option target_mode close_noretarget
tomf_test_defaults: run {some Node}  

tomf_test_close_noretarget_noNode: run {}
  target_pi {no Node} close_noretarget 
tomf_test_far_noretarget_noNode: run {}
  target_pi {no Node} far_noretarget 

// This confirms that the _partiality_ works: we don't say what 
// the potential other node is connected to, if it exists.
tomf_test_close_noretarget_contains_k3: run {}
  target_pi {
    Node = `Node0 + `Node1 + `Node2
    edges = Node -> Node -> 0
  } close_noretarget 

// TODO: why is this experiencing bad performance?
tomf_test_far_noretarget_contains_k3: run {}
  target_pi {
    Node = `Node0 + `Node1 + `Node2
    edges = Node -> Node -> 0
  } far_noretarget 






// Not reliable yet
tomf_test_close_retarget_noNode: run {}
  target_pi {no Node} close_retarget 
tomf_test_far_retarget_noNode: run {}
  target_pi {no Node} far_retarget 

// Not reliable yet
tomf_test_hamming_noNode: run {}
  target_pi {no Node} hamming_cover



// TODO: why is 2nd instance of hamming cover taking so long??

// TODO: forge-core tests were only looking at the noretarget variants.
//    (Maybe the retargeting versions never worked?)

// TODO: at present, the extra relations taint the state for all runs


//////////////////////
// Int Minimization //
//////////////////////

// TODO: the syntax is rather strange: what is the _target_? Rather this is 
// the minimization objective. 

// Minimize number of nodes
tomf_test_close_noretarget_int_numNode: run {}
  target_int {#Node} close_noretarget 

// This setting won't avoid, e.g., (-8 + -8 + -8) even though it "wraps"
// I believe this is because there is no _arithmetic_ involved. 
option no_overflow true

// Should give sum = -8 (mod)
tomf_test_close_noretarget_int_totalWeight4: run {} for exactly 2 Node
  target_int {sum m: Node | sum n: Node | m.edges[n]} close_noretarget 

// Should give sum = -2 (mod)
tomf_test_close_noretarget_int_totalWeight2: run {} for exactly 2 Node, 2 Int
  target_int {sum m: Node | sum n: Node | m.edges[n]} close_noretarget 

// Should give sum = -16 (mod)
tomf_test_close_noretarget_int_totalWeight5: run {} for exactly 2 Node, 5 Int
  target_int {sum m: Node | sum n: Node | m.edges[n]} close_noretarget 

//////////////////////
// Int Maximization //
//////////////////////

// Maximize number of nodes
tomf_test_far_noretarget_int_numNode: run {}
  target_int {#Node} far_noretarget 

// Should give sum = 8 (mod)
tomf_test_far_noretarget_int_totalWeight4: run {} for exactly 2 Node
  target_int {sum m: Node | sum n: Node | m.edges[n]} far_noretarget 

