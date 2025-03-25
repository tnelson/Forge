#lang forge

/* Testing the TOMF functionality of Forge. 
   Since `is sat` is too inexpressive, we define the run and then 
   examine the instance(s) it produces in forge/core. 
*/

option problem_type target
option solver PMaxSAT4J
option verbose 0

-- stay as close as possible to first instance
option target_mode close_noretarget

sig Node { edges: set Node }

// Defined for forge/core to use. 
option run_sterling off
tomf_close_fixed: run {}
target_pi {no Node} close_noretarget 
//target_pi {Node = `Node0 + `Node1} close_noretarget 
  

