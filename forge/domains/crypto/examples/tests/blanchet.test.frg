#lang forge 

open "../../base.frg"
open "../blanchet.rkt"
open "../blanchet.frg"
option run_sterling off

TEST_blanchet_attack_witness: assert {
    blanchet_run
    atk_no_ltks
    blanchet_init.agent != Attacker
    blanchet_resp.agent != Attacker
    blanchet_init.blanchet_init_a in PublicKey
    blanchet_init.blanchet_init_b in PublicKey
    no KeyPairs.ltks.(blanchet_init.blanchet_init_s)
} is sat for exactly 1 KeyPairs, exactly 4 Timeslot, 20 mesg, 
              // NOTE WELL: not giving "exactly" here causes unnecessary bounds increase.
              // TODO: investigate and refactor
              exactly 9 Key, exactly 6 akey, exactly 3 PrivateKey, exactly 3 PublicKey, exactly 3 skey, 
              3 name, 1 Attacker, 3 text, exactly 5 Ciphertext, exactly 1 AttackerStrand, 
              exactly 1 blanchet_init, exactly 1 blanchet_resp, exactly 3 strand, 
              // Must have the skeletons present, even if they are unused 
              exactly 1 skeleton_blanchet_0, 
              exactly 1 skeleton_blanchet_1, 
              exactly 1 skeleton_blanchet_2
              
              //, 5 Int
              
              , 3 Microtick
  for {next is linear
       mt_next is linear}