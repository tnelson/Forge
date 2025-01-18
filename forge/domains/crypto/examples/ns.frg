#lang forge 

/*
  Forge definitions for the (pre-fix) Needham-Schroeder protocol example
  from Siegel, et al. 
*/

// Protocol definitions in CPSA DSL
open "ns.rkt" 
// Domain-specific visualizer script
option run_sterling "../vis/crypto_viz.js"

// The attack exists in this execution if both the secrets (from the initiator's 
// perspective) are eventually learned by the attacker.
pred attack_exists {
    ns_init.ns_init_n1 + ns_init.ns_init_n2 in Attacker.learned_times.Timeslot      
}

// Proxy for the concrete attack, used to debug and check the model:
//   some message is sent encrypted with the attacker's public key
pred proxy_shape {
    some m: Message | {
        // getInv[m.data.encryptionKey] in (KeyPairs.owners.Attacker)
        getInv[m.data.encryptionKey] in (KeyPairs.owners.(ns_init.agent))
    }
}

// Protocol success: if the legitimate agents eventually learn both secrets
pred success {
  (ns_init.ns_init_n1 + ns_init.ns_init_n2) in 
  (ns_init.agent.learned_times.Timeslot & ns_resp.agent.learned_times.Timeslot)
}

// The conditions under which we search for an attack
pred attack_conditions {
  // Attacker doesn't generate the secrets
  ns_init.ns_init_n1 not in Attacker.generated_times.Timeslot
  ns_init.ns_init_n2 not in Attacker.generated_times.Timeslot

  // Initiator believes they are a, and responder believes they are b
  // (Note: cannot add restriction on identity of counterpart, or attack won't be produced!)
  ns_init.ns_init_a = ns_init.agent
  ns_resp.ns_resp_b = ns_resp.agent
      
  // For readability, require the secrets to be different
  ns_init.ns_init_n1 != ns_init.ns_init_n2

  // The attacker is not playing a game by themselves
  ns_init.agent != Attacker
  ns_resp.agent != Attacker
}

ns_attack_witness: run {
    wellformed // wellformedness assumptions from domain model
    attack_conditions // various conditions for the attack
    success // the execution "succeeds" (initiator and responder learn secrets)
    exec_ns_init // this is an execution for the initiator
    exec_ns_resp // this is an execution for the responder
    constrain_skeleton_ns_1 // take responder POV, where CPSA shows the attack behavior                                
    
    attack_exists // find a trace witnessing the attack
    
  } for exactly 6 Timeslot, exactly 1 KeyPairs, exactly 6 Key, exactly 3 name, 16 mesg,
    exactly 2 text, exactly 5 Ciphertext, exactly 1 ns_init, exactly 1 ns_resp,
    exactly 3 PublicKey, exactly 3 PrivateKey, 0 skey, exactly 6 akey, exactly 3 strand,
    6 Int 
    // TODO change over to some-based bounds
    for {next is linear} 
