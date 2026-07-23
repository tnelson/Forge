#lang forge/domains/crypto

; https://bblanche.gitlabpages.inria.fr/publications/BlanchetETAPS12.pdf

;(herald "Blanchet's Simple Example Protocol"
;  (comment "There is a flaw in this protocol by design"))

(defprotocol blanchet basic
  (defrole init
    (vars (a b akey) (s skey) (d data))
    (trace
     (send (enc (enc s (invk a)) b))
     (recv (enc d s)))
    (uniq-orig s))
  (defrole resp
    (vars (a b akey) (s skey) (d data))
    (trace
     (recv (enc (enc s (invk a)) b))
     (send (enc d s)))
    (uniq-orig d))
  (comment "Blanchet's protocol"))

; Skeleton 0
(defskeleton blanchet
  (vars (a b akey) (s skey) (d data))
  (defstrand init 2 (a a) (b b) (s s) (d d))
  (non-orig (invk b))
  (comment "Analyze from the initiator's perspective"))

; Skeleton 1
(defskeleton blanchet
  (vars (a b akey) (s skey) (d data))
  (defstrand resp 2 (a a) (b b) (s s) (d d))
  (non-orig (invk a) (invk b))
  (comment "Analyze from the responder's perspective"))

; The scenario the manual describes shows the *responder*'s value
; being compromised, but not the initiator's. Right now, our model
; will create a constraint for both listeners (conjuctively), yielding
; a spurious unsat result unless the initiator's deflistener is removed.
;(defskeleton blanchet
;  (vars (a b akey) (s skey) (d data))
;  (defstrand init 2 (a a) (b b) (s s) (d d))
;  (deflistener d)
;  (non-orig (invk b))
;  (comment "From the initiator's perspective, is the secret leaked?"))

; Skeleton 2 (assuming the above skeleton is commented out)
(defskeleton blanchet
  (vars (a b akey) (s skey) (d data))
  (defstrand resp 2 (a a) (b b) (s s) (d d))
  (deflistener d)
  (non-orig (invk a) (invk b))
  (comment "From the responders's perspective, is the secret leaked?"))