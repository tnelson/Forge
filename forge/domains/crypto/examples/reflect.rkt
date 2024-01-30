#lang forge/domains/crypto

;; Use experimental domain-specific input, adapted from Siegel et al.
;; This language is essentially #lang forge/core, with some additional helpers.

;; A simple protocol vulnerable to a reflection attack, taken from CPSA:
;; https://github.com/mitre/cpsa/blob/master/tst/reflect.tst

(set-option! 'verbose 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CPSA syntax to define the reflection protocol
(defprotocol reflect basic
  (defrole init
    (vars (a b akey))
    (trace
     (send (enc b (invk a)))
     (recv (enc a (invk b)))))
  (defrole resp
    (vars (a b akey))
    (trace
     (recv (enc b (invk a)))
     (send (enc a (invk b))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Multiple point-of-view strands

(defskeleton reflect
  (vars (a b akey))
  (defstrand resp 1 (a a) (b b))
  (non-orig (invk a) (invk b)))

(defskeleton reflect
  (vars (a b akey))
  (defstrand init 2 (a a) (b b))
  (non-orig (invk a) (invk b)))

(defskeleton reflect
  (vars (a b akey))
  (defstrand resp 1 (a a) (b (invk b)))
  (non-orig (invk a) b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Run the protocol so we can visualize an example

(run reflect_resp_pov
      #:preds [
               ; Basic shape: execution from different perspectives, etc. 

               exec_reflect_init ; one agent is an initiator
               exec_reflect_resp ; one agent is a responder
               constrain_skeleton_reflect_2 ; responder POV strand
               temporary ; constraints defined in base
               wellformed ; further constraints defined in base

               ; Enforce different principals (recall need to use "!", not "not")
               (! (= (join reflect_resp agent)
                     (join reflect_init agent)))

               ; Enforce b and a are not from the same pair
               (! (= (join reflect_resp reflect_resp_a)
                       (getInv (join reflect_resp reflect_resp_b))))               
               (! (= (join reflect_resp reflect_resp_a)
                       (join reflect_resp reflect_resp_b)))
               ]
      #:bounds [(is next linear)]
      #:scope [(KeyPairs 1 1)
               (Timeslot 4 4) ; 4 steps for reflection attack               
               
               (mesg 13) ; 9 + 3 + 0 + 4
               
               (Key 6 6)
               (akey 6 6)               
               (PrivateKey 3 3)
               (PublicKey 3 3)
               (skey 0)
               
               (name 3 3)
               (Attacker 1 1)
               
               (text 0) ; includes data
               
               (Ciphertext 4 4)               
               
               (AttackerStrand 1 1)                              
               (reflect_init 1 1)
               (reflect_resp 1 1)               
               
               (skeleton_reflect_0 1 1)
               (skeleton_reflect_1 1 1)
               (skeleton_reflect_2 1 1)              
               (Int 5 5) 
               ]      
      )

(display reflect_resp_pov)
