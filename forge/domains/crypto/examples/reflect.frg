#lang forge 

/*
  Forge definitions for the reflection protocol example. 

  We will import the `reflect.rkt` file, which contains the CPSA definitions. 
  (Siegel, et al. did all of this in `reflect.rkt`, which was quite verbose.)
*/

open "reflect.rkt"

/** Settings */ 
// If running on MacOS, Glucose is supported. If running on Windows, only 
// the default (sat4j) and MiniSatProver are supported.
// option solver glucose

// Note the visualizer file. As of summer 2024 this is not automatically opened in Sterling.
option run_sterling "../vis/crypto_viz.js"

reflect_responder_pov: run {
  // Basic shape: execution from different perspectives, etc. 
  exec_reflect_init            // one agent is an initiator
  exec_reflect_resp            // one agent is a responder
  constrain_skeleton_reflect_2 // responder POV strand
  temporary                    // constraints defined in base
  wellformed                   // further constraints defined in base

  // Enforce different principals (recall need to use "!", not "not")
  reflect_resp.agent != reflect_init.agent
  // Enforce b and a are not from the same pair
  reflect_resp.reflect_resp_a != getInv[reflect_resp.reflect_resp_b]    
  reflect_resp.reflect_resp_b != getInv[reflect_resp.reflect_resp_a]  // FIXED **TN**


} for exactly 1 KeyPairs, exactly 4 Timeslot, 13 mesg, 
      exactly 6 Key, exactly 6 akey, exactly 3 PrivateKey, exactly 3 PublicKey,
      0 skey, exactly 3 name, exactly 1 Attacker, 0 text, exactly 4 Ciphertext,
      exactly 1 AttackerStrand, exactly 1 reflect_init, exactly 1 reflect_resp,
      exactly 1 skeleton_reflect_0, exactly 1 skeleton_reflect_1, exactly 1 skeleton_reflect_2,
      5 Int
  for {next is linear}

