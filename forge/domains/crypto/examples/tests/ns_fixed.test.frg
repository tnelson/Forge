#lang forge 

open "../../base.frg"
open "../ns_fixed.rkt"
open "../ns_fixed.frg"
option run_sterling "../../vis/crypto_viz.js"

TEST_ns_fixed_attack_unsat: assert {
    wellformed // wellformedness assumptions from domain model
    attack_conditions // various conditions for the attack
    success // the execution "succeeds" (initiator and responder learn secrets)
    exec_ns_init // this is an execution for the initiator
    exec_ns_resp // this is an execution for the responder
    constrain_skeleton_ns_1 // take responder POV, where CPSA shows the attack behavior                                
    
    attack_exists // find a trace witnessing the attack
    
  } is unsat for 
    exactly 6 Timeslot, exactly 2 Microtick,
    exactly 1 KeyPairs, exactly 6 Key, exactly 3 name, 16 mesg,
    exactly 2 text, exactly 5 Ciphertext, exactly 1 ns_init, exactly 1 ns_resp,
    exactly 3 PublicKey, exactly 3 PrivateKey, 0 skey, exactly 6 akey, exactly 3 strand,
    6 Int 
    for {next is linear  mt_next is linear} 