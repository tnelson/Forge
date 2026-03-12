#lang forge/domains/crypto

; If 
;  (1) the init strand and the resp strand can share a knowledge base
;    due to running on the same agent, and 
;  (2) the attacker can generate d at the start, then
;  Attacker ----> resp
;  Attacker <---- resp
;   [now resp knows the value, and so does init]
;  init ----> Attacker [using same value]
;  init <---- Attacker
;   - d is uniquely originating (with Attacker)
;   - the attacker knows d (they generated it!)
;

; Wait: is that value of d uniquely originated?
; Possibly underconstrained origination predicate??

;               (no (join Attacker generated_times 
;                 (join blanchet-corrected_resp blanchet-corrected_resp_d)))


;  so we require that Attacker did not generate (responder's) d value

(defprotocol blanchet_corrected basic
  (defrole init
    (vars (a b akey) (s skey) (d data))
    (trace
     (send (enc (enc s b (invk a)) b))
     (recv (enc d s)))
    (uniq-orig s))
  (defrole resp
    (vars (a b akey) (s skey) (d data))
    (trace
     (recv (enc (enc s b (invk a)) b))
     (send (enc d s)))
    (uniq-orig d))
  (comment "Corrected Blanchet's protocol"))

(defskeleton blanchet_corrected
  (vars (a b akey) (s skey) (d data))
  (defstrand init 2 (a a) (b b) (s s) (d d))
  (non-orig (invk b))
  (comment "Analyze from the initiator's perspective"))

(defskeleton blanchet_corrected
  (vars (a b akey) (s skey) (d data))
  (defstrand init 2 (a a) (b b) (s s) (d d))
  (deflistener d)
  (non-orig (invk b))
  (comment "From the initiator's perspective, is the secret leaked?"))

; The scenario the manual describes shows the *responder*'s value
; being compromised, but not the initiator's. Right now, our model
; will create a constraint for both listeners (conjuctively), yielding
; a spurious unsat result unless the initiator's deflistener is removed.
;(defskeleton blanchet-corrected
;  (vars (a b akey) (s skey) (d data))
;  (defstrand resp 2 (a a) (b b) (s s) (d d))
;  (non-orig (invk a) (invk b))
;  (comment "Analyze from the responder's perspective"))

(defskeleton blanchet_corrected
  (vars (a b akey) (s skey) (d data))
  (defstrand resp 2 (a a) (b b) (s s) (d d))
  (deflistener d)
  (non-orig (invk a) (invk b))
  (comment "From the responders's perspective, is the secret leaked?"))

; (run blanchet_corrected
;       #:preds [
;                exec_blanchet-corrected_init
;                exec_blanchet-corrected_resp
;               ; constrain_skeleton_blanchet-corrected_0
;               ; constrain_skeleton_blanchet-corrected_1
;                constrain_skeleton_blanchet-corrected_2 ; note this is the fourth skeleton including commented-out               
;                wellformed

;                ; The attacker has no long-term keys
;                (no (+ (join Attacker (join name (join KeyPairs ltks)))
;                       (join name (join Attacker (join KeyPairs ltks)))))
;                ]
;       #:bounds [(is next linear)]
;       #:scope [(KeyPairs 1 1)
;                (Timeslot 4 4) 
;               ; (Message 4 4)
               
;                (mesg 20) ; 9 + 3 + 3 + 5
               
;                (Key 9)
;                (akey 6)               
;                (PrivateKey 3)
;                (PublicKey 3)
;                (skey 3)
               
;                (name 3)
;                (Attacker 1 1)
               
;                (text 3) ; includes data
               
;                (Ciphertext 5 5)               
               
;                (AttackerStrand 1 1)               
;                (blanchet-corrected_init 1 1)
;                (blanchet-corrected_resp 1 1)               
;                (strand 3 3)
               
;                (skeleton_blanchet-corrected_0 1 1)
;                (skeleton_blanchet-corrected_1 1 1)
;                (skeleton_blanchet-corrected_2 1 1)               
;                (Int 5)
;                ]
;       ;#:expect sat
;       )

; (display blanchet_corrected)