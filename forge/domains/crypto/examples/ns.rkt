#lang forge/domains/crypto

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Needham-Schroeder example from CSPA

(defprotocol ns basic
  (defrole init
    (vars (a b name) (n1 n2 text))
    (trace (send (enc n1 a (pubk b)))
           (recv (enc n1 n2 (pubk a)))
           (send (enc n2 (pubk b)))))
  (defrole resp 
    (vars (a b name) (n1 n2 text))
    (trace (recv (enc n1 a (pubk b)))
           (send (enc n1 n2 (pubk a)))
           (recv (enc n2 (pubk b))))))

(defskeleton ns
  (vars (a b name) (n1 text))
  (defstrand init 3 (a a) (b b) (n1 n1)) 
  (non-orig (privk b) (privk a))
  (uniq-orig n1)
  (comment "Initiator point-of-view"))

(defskeleton ns
  (vars (a b name) (n2 text))
  (defstrand resp 3 (a a) (b b) (n2 n2))
  (non-orig (privk a) (privk b))
  (uniq-orig n2)
  (comment "Responder point-of-view"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
