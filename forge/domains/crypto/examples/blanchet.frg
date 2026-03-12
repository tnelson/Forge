#lang forge

// Protocol definitions in CPSA DSL
open "blanchet.rkt" 
// Domain-specific visualizer script
option run_sterling "../vis/crypto_viz.js"

/*
  Note: the CPSA definition here says only that the initiator is encrypting their message
        with *an asymmetric key*. If they encrypt with a _private_ key then of course the 
        attacker can immediately read their message.
  The CPSA manual says that `akey` is the sort of asymmetric keys, so (inv k) doesn't 
  only operate on public keys.

  ---------------------------------------------------------------------------------------

  https://bblanche.gitlabpages.inria.fr/publications/BlanchetETAPS12.pdf

  In the paper, it is an encrypted term, wrapping a signed term containing a symmetric key.

  Message 1. A → B : {{k}skA}pkB     k fresh
  Message 2. B → A : {s}k

        In general, in the literature, as in the example above, the protocols are de-
        scribed informally by giving the list of messages that should be exchanged be-
        tween the principals. Nevertheless, one must be careful that these descriptions
        are only informal: they indicate what happens in the absence of an adversary.
        However, an adversary can capture messages and send his own messages, so the
        source and the target of a message may not be the expected one. Moreover,
        these descriptions leave implicit the verifications done by the principals when
        they receive messages. Since the adversary may send messages different from
        the expected ones, and exploit the obtained reply, these verifications are very
        important: they determine which messages will be accepted or rejected, and
        may therefore protect or not against attacks.

        Although the explanation above may seem to justify its security informally,
        this protocol is subject to an attack:

        Message 1. A → C : {{k}skA}pkC
        Message 1’. C(A) → B : {{k}skA}pkB
        Message 2. B → C(A) : {s}k

    This attack is quite similar to the one against Needham-Schroeder in spirit, 
    although the protocol has a very different shape.


  * What logic exists in this model for verifying keys? 
  
*/

pred blanchet_run {
    exec_blanchet_init
    exec_blanchet_resp
    
    -- Do not run more than one skeleton at a time.
    -- constrain_skeleton_blanchet_0  -- init strand perspective 
    -- constrain_skeleton_blanchet_1  -- sat by self

    -- This contains a "listener strand", which the expander turns into 
    -- #`(in (join #,this-strand-or-skeleton #,(id-converter pname strand-role-or-skeleton-idx v))
    --           (join Attacker learned_times Timeslot)))
    -- That is: the attacker learns the value of this strand variable at some point in time.
    --  (It does NOT create a separate listener strand, it just checks the attacker's knowledge base.)
    constrain_skeleton_blanchet_2 -- responder strand perspective (with 1 commented out skeleton)
    
    // constrain_skeleton_blanchet_3 // note this is the fourth skeleton if including commented out
    wellformed
}

-- How does the attacker have the LTK in timeslot 0? The LTK is uniq-orig in the initiator strand. 
--   the LTK isn't in its base knowledge
--   the LTK isn't generated or originated by it
--     Some sort of problem with workspace. Workspace at that timeslot shows opening:
--       Ciphertext4 (outer term) [mt0] --> Ciphertext1 (inner term) [mt1] --> LTK content [mt2]
--  Ah: the initiator is sending encrypted with a _private_ key, so the attacker can open the ciphertext 
--    because they have the public keys

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
    blanchet_run
    atk_no_ltks

    // The attacker is not playing a game by themselves
    // It's unclear if this should just be a base assumption. But we do want the attacker to
    // be able to own multiple strands. 
    blanchet_init.agent != Attacker
    blanchet_resp.agent != Attacker

    // TODO DESIGN NOTE: I feel like some of these should be axiomatic, but aren't guaranteed by CPSA.
    //  It's worth asking: are there CPSA constraints that I'm unaware of?

    // The keys used by the initiator are of the proper types. The CPSA definition just says they are 
    // asymmetric keys, not that (e.g.) the outermost encryption key was a public key.
    blanchet_init.blanchet_init_a in PublicKey
    blanchet_init.blanchet_init_b in PublicKey
    
    // The symmetric key sent is not pre-assigned to any agent pair. 
    no KeyPairs.ltks.(blanchet_init.blanchet_init_s)


} for exactly 1 KeyPairs, exactly 4 Timeslot,  20 mesg, 9 Key, 6 akey, 3 PrivateKey, 3 PublicKey, 
              3 skey, 3 name, 1 Attacker, 3 text, exactly 5 Ciphertext, exactly 1 AttackerStrand, 
              exactly 1 blanchet_init, exactly 1 blanchet_resp, exactly 3 strand, 
              //exactly 1 skeleton_blanchet_0, 
              //exactly 1 skeleton_blanchet_1, 
              exactly 1 skeleton_blanchet_2
              
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