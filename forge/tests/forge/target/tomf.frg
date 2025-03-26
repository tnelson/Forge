#lang forge

/* Testing the TOMF functionality of Forge. 
   Since `is sat` is too inexpressive, we define the run and then 
   examine the instance(s) it produces in forge/core. 
*/

option problem_type target
option solver PMaxSAT4J
option run_sterling off

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
tomf_test_close_retarget_noNode: run {}
  target_pi {no Node} close_retarget 
tomf_test_far_retarget_noNode: run {}
  target_pi {no Node} far_retarget 
tomf_test_hamming_noNode: run {}
  target_pi {no Node} hamming_cover

// TODO: int-minimization encoding 

// TODO: why is 2nd instance of hamming cover taking so long??

// TODO: forge-core tests were only looking at the noretarget variants.
//    (Maybe the retargeting versions never worked?)



tomf_test_close_noretarget_numNode: run {}
  target_int {#Node} close_noretarget 




