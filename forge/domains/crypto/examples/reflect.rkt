#lang forge/domains/crypto

;; Use experimental domain-specific input, adapted from Siegel et al.
;; This language is essentially #lang forge/core, with some additional helpers.
;; These DSL files can be imported into #lang forge for ease of use.

;; A simple protocol vulnerable to a reflection attack, taken from CPSA:
;; https://github.com/mitre/cpsa/blob/master/tst/reflect.tst

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
