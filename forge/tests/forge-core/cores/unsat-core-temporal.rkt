#lang forge/core

(require (only-in rackunit check-true check-false check-pred check-eq?)
         (only-in forge/lang/ast node?))

(set-option! 'verbose 0)
(set-option! 'problem_type 'temporal)
(set-option! 'solver 'MiniSatProver)
(set-option! 'core_minimization 'rce)
(set-option! 'logtranslation 1)
(set-option! 'coregranularity 1)
(set-option! 'min_tracelength 3)
(set-option! 'max_tracelength 16)

(define (core-of a-run)
  (forge:Unsat-core ((forge:make-model-generator (forge:get-result a-run) 'next))))

(sig World #:one)
(relation counter (World Int) #:is-var "var")
(sig Other)
(relation flag (Other Other) #:is-var "var")


; World.counter = 0
; always { World.counter' = World.counter}
; eventually { World.counter = 1}
; no flag
(define p-init (= (join World counter) (int 0)))
(define p-stay (always (= (prime (join World counter)) (join World counter))))
(define p-reach-one (eventually (= (join World counter) (int 1))))
(define p-irrelevant (no flag))

(run mapped-core #:preds [p-init p-stay p-reach-one p-irrelevant])
(define mapped (core-of mapped-core))

; Shape of resulting core is as expected. 
(check-pred pair? mapped)
(check-true (andmap node? mapped))
(check-eq? (length mapped) 3)
(check-true (and (memq p-init mapped) #t))
(check-true (and (memq p-stay mapped) #t))
(check-true (and (memq p-reach-one mapped) #t))
(check-false (memq p-irrelevant mapped))

(sig Post)
(sig Server)
(relation posts (Server Post) #:is-var "var")
(define p-primed-disequal (! (= (prime posts) (prime posts))))

(run unmapped-core #:preds [p-primed-disequal])
(define unmapped (core-of unmapped-core))

; Pardinus encodes the shape of temporal traces with its own constraints, so when those 
; are implicated in a core, they can sometimes creep back in as raw strings. 
(check-pred pair? unmapped)
(check-true (andmap string? unmapped))
(check-false (memq p-primed-disequal unmapped))
