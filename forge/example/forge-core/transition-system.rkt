#lang forge/core

(sig State)
(sig Contents)
(sig InitContents #:one #:extends Contents)
(sig TermContents #:one #:extends Contents)

(relation contains (State Contents))
(relation next-state (State State))


; USER INPUT HERE
; An init state contains InitContents
(pred (init state)
  (= (join state contains) InitContents))

; USER INPUT HERE
; A term state contains TermContents
(pred (term state)
  (= (join state contains) TermContents))

; USER INPUT HERE
; Transitions must change contents 
(pred (legal-transition pre post)
  (not (= (join pre contains) (join post contains))))


; Enforce above predicates
(pred valid-configuration
  (let ([init-state (- State (join State next-state))]
        [term-state (- State (join next-state State))])
    (and (init init-state)
         (term term-state)))
  (all ([pre State] [post (join pre next-state)])
    (legal-transition pre post)))

; Enforce structure on transition and contains
(inst transition
  (is next-state linear)
  (is contains func))

(run transition-run
     #:preds [valid-configuration]
     #:bounds [transition]
     #:scope ([State 6]
              [Contents 6 6]))
(display transition-run)