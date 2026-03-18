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

/** Run the protocol from the responder's point of view. */ 

reflect_responder_pov: run {
  // domain constraints defined in base.frg (must always be included)
  wellformed                   

  // Query shape: execution from different perspectives, etc. 
  exec_reflect_init            // some agent is an initiator
  exec_reflect_resp            // some agent is a responder
  constrain_skeleton_reflect_2 // scenario from POV of responder strand
  
  // Enforce different principals (avoid scenario where one agent talks to itself) 
  reflect_resp.agent != reflect_init.agent

  // Enforce reponder strand's b and a are not from the same key pair
  reflect_resp.reflect_resp_a != getInv[reflect_resp.reflect_resp_b]    
  reflect_resp.reflect_resp_b != getInv[reflect_resp.reflect_resp_a]

} for 
      // How many time indexes and how many terms can exist in an execution of the protocol?
      // Keep in mind 2 things:
      //   - each send+receive pair requires _2_ actual messages, because the model uses 
      //     uses the Attacker as the medium of communication. 
      //   - we follow CPSA terminology and use `mesg` to refer to any term, including plaintexts.
      //     Hence the high bound on `mesg`. 
      exactly 4 Timeslot, exactly 2 Microtick, 13 mesg,  
      // How many keys, and of what type, can exist in an execution of the protocol?
      exactly 1 KeyPairs, exactly 6 Key, exactly 6 akey, 0 skey, 
      exactly 3 PrivateKey, exactly 3 PublicKey,
      // How many data terms can exist in an execution of the protocol?
      exactly 3 name, 0 text, exactly 4 Ciphertext,
      // How many of each principal type can exist in an execution of the protocol?
      exactly 1 reflect_init, exactly 1 reflect_resp,
      // How many bits in an integer? (We shouldn't need integers here; 1 is Forge minimum)
      1 Int
  // Manufacture bounds so that Timeslots are in linear ordering 
  // (similar to Alloy's ordering module: `open util/ordering[Timeslot]`)
    for {next is linear 
        mt_next is linear} 


/*
  Note on bounds:
    - bounds are *not* necessary to give for the skeleton sigs (in this case, 
      skeleton_reflect_0, exactly 1 skeleton_reflect_1, exactly 1 skeleton_reflect_2)
      because they are declared as "one" sigs by the expander, and thus act as constants.

    - Attacker and AttackerStrand are likewise "one" sigs (declared in base.frg).
*/

/*
  Note on output:

    - You may see multiple shapes produced by the visualizer, depending on which the 
      solver generates first. This query says that there must be an initiator and 
      a responder strand, but those terms (initiator, responder) don't have semantic
      weight. So all we have is their isolated perspectives, and (e.g.) these two shapes 
      both work:

      initiator          attacker         responder
          ----------------->
                             ----------------->
                             <-----------------
          <-----------------

      attacker           responder        initiator
          ------------------>
          <------------------------------------
          <------------------
          ------------------------------------>

      After all, from the responder's POV it receives a valid message 1 and replies
      with a valid message 2; similarly for the initiator. The important things to 
      notice are that the contents of the message must be something the attacker has 
      to start with, such as one of the public keys, and that it must either be able
      to echo back the same message or open one of the messages it receives.
*/

