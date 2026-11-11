#lang forge/core

(require (only-in rackunit check-true check-false check-pred check-eq? check-equal?)
         (only-in forge/lang/ast node? node-info nodeinfo-loc))

(set-option! 'verbose 0)
(set-option! 'solver 'MiniSatProver)
(set-option! 'core_minimization 'rce)
(set-option! 'logtranslation 1)
(set-option! 'coregranularity 1)

(sig Node)
(relation edges (Node Node))

; no Node
; some {n1: Node | some n1|}
; no edges
(define p-no (no Node))
(define p-some-comprehension (some (set ([n1 Node]) (some n1))))
(define p-irrelevant (no edges))

(run comprehension-core
     #:preds [p-no p-some-comprehension p-irrelevant])

(define result ((forge:make-model-generator (forge:get-result comprehension-core) 'next)))

(check-true (forge:Unsat? result))
(define core (forge:Unsat-core result))
(check-pred pair? core)

; Every part of the core maps back to a Forge AST node
(check-true (andmap node? core))

; Using memq (referential equality) because the map should use the pre-solver node objects.
(check-eq? (length core) 2)
(check-true (and (memq p-no core) #t))
(check-true (and (memq p-some-comprehension core) #t))
(check-false (memq p-irrelevant core))