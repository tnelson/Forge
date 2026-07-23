#lang forge

// Protocol definitions in CPSA DSL
open "blanchet-corrected.rkt" 
// Domain-specific visualizer script
option run_sterling "../vis/crypto_viz.js"

pred blanchet_corrected_run {
    exec_blanchet_corrected_init
    exec_blanchet_corrected_resp
    
    -- Do not run more than one skeleton at a time.
    -- constrain_skeleton_blanchet_0  -- init strand perspective 
    -- constrain_skeleton_blanchet_1  -- sat by self

    -- This contains a "listener strand", which the expander turns into 
    -- #`(in (join #,this-strand-or-skeleton #,(id-converter pname strand-role-or-skeleton-idx v))
    --           (join Attacker learned_times Timeslot)))
    -- That is: the attacker learns the value of this strand variable at some point in time.
    --  (It does NOT create a separate listener strand, it just checks the attacker's knowledge base.)
    constrain_skeleton_blanchet_corrected_2 -- responder strand perspective (with 1 commented out skeleton)
    
    wellformed
}

/** The attacker has no long-term keys. */
pred atk_no_ltks {
   // KeyPairs.ltks:  set name -> name -> skey

    no Attacker.(KeyPairs.ltks)
    // Simplified from:
    //(no (+ (join Attacker (join name     (join KeyPairs ltks)))
    //       (join name     (join Attacker (join KeyPairs ltks)))))
}


// Bounds can be quite troublesome. Count carefully.
run {
    blanchet_corrected_run
    atk_no_ltks

    // The attacker is not playing a game by themselves
    // It's unclear if this should just be a base assumption. But we do want the attacker to
    // be able to own multiple strands. 
    blanchet_corrected_init.agent != Attacker
    blanchet_corrected_resp.agent != Attacker

    // The keys used by the initiator are of the proper types. The CPSA definition just says they are 
    // asymmetric keys, not that (e.g.) the outermost encryption key was a public key.
    blanchet_corrected_init.blanchet_corrected_init_a in PublicKey
    blanchet_corrected_init.blanchet_corrected_init_b in PublicKey

    // The symmetric key sent is not pre-assigned to any agent pair. 
    no KeyPairs.ltks.(blanchet_corrected_init.blanchet_corrected_init_s)


} for exactly 1 KeyPairs, exactly 4 Timeslot,  20 mesg, 9 Key, 6 akey, 3 PrivateKey, 3 PublicKey, 
              3 skey, 3 name, 1 Attacker, 3 text, exactly 5 Ciphertext, exactly 1 AttackerStrand, 
              exactly 1 blanchet_corrected_init, exactly 1 blanchet_corrected_resp, exactly 3 strand, 
              //exactly 1 skeleton_blanchet_0, 
              //exactly 1 skeleton_blanchet_1, 
              exactly 1 skeleton_blanchet_corrected_2
              
              , 5 Int
              
              , 3 Microtick

  for {next is linear
       mt_next is linear}

    //   #:scope [(KeyPairs 1 1)
    //            (Timeslot 4 4) 
    //            (Message 4 4)
               
    //            (mesg 20) ; 9 + 3 + 3 + 5
               
    //            (Key 9)
    //            (akey 6)               
    //            (PrivateKey 3)
    //            (PublicKey 3)
    //            (skey 3)
               
    //            (name 3)
    //            (Attacker 1 1)
               
    //            (text 3) ; includes data
               
    //            (Ciphertext 5 5)               
               
    //            (AttackerStrand 1 1)               
    //            (blanchet_init 1 1)
    //            (blanchet_resp 1 1)               
    //            (strand 3 3)
               
    //            (skeleton_blanchet_0 1 1)
    //            (skeleton_blanchet_1 1 1)
    //            (skeleton_blanchet_2 1 1)               
    //            (Int 5)
    //            ]
    //   ;#:expect sat
    //   )



/*

(defprotocol blanchet basic             OS-EPL term count, if all distinct (recall inv denotes counterpart keys)
  (defrole init                         ------------------------
    (vars (a b akey) (s skey) (d data)) + 4 akey, 1 skey, 1 data
    (trace
     (send (enc (enc s (invk a)) b))    + 2 ciphertext
     (recv (enc d s)))                  + 1 ciphertext
    (uniq-orig s))
  (defrole resp
    (vars (a b akey) (s skey) (d data)) + 4 akey, 1 skey, 1 data
    (trace
     (recv (enc (enc s (invk a)) b))    + 2 ciphertext
     (send (enc d s)))                  + 1 ciphertext
    (uniq-orig d))                      ------------------------
  (comment "Blanchet's protocol"))        8 akey, 2 skey, 2 data, 6 ciphertext

*/