#lang forge/core

(set-verbosity 10)

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
  (is contains func)
  (<= (card State) 5))

(run transition-run
     #:preds [valid-configuration]
     #:bounds [transition]
     #:scope (;[State 6]
              [Contents 6 6]))
; (display transition-run)

; get-fields :: node/expr/relation Run -> List<Relation>
(define (get-fields sig-rel run)
  (define state (forge:get-state run)) ; State
  (define sig (forge:get-sig state sig-rel)) ; Sig
  (define relations (forge:get-relations state)) ; List<Relation>
  (for/list ([relation relations]
             #:when (equal? (first (forge:get-sigs state relation))
                            sig))
    relation)) ; You can grab what you want from each field here

(get-fields State transition-run)
(forge:get-fields transition-run (forge:get-sig transition-run State))