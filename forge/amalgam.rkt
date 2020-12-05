;#lang racket
;(require "sigs.rkt")
; Why does the above cause an error?
#lang forge/core

;(require forge/amalgam/desugar/desugar)

(require "amalgam/tests/forge_ex.rkt")  
(run udt
     #:preds [isUndirectedTree]
     #:scope [(Node 7)]) 

; Entry point for Amalgam from forge/core
;  This is a sketch for the moment

(provide build-provenances)

(define (flip-tuple a-hash t raw-r)
  (define r (cond [(string? raw-r) (string->symbol raw-r)]
                  [(symbol? raw-r) raw-r]
                  [else (error (format "amalgam: unexpected type for relation name: ~a" raw-r))]))
  (unless (hash-has-key? a-hash r)
    (error (format "amalgam: failed to find ~a in ~a" r (hash-keys a-hash))))
  (if (member t (hash-ref a-hash r))
      ; update functionally, return immutable map
      (hash-set a-hash r (remove t (hash-ref a-hash r)))
      (hash-set a-hash r (cons t (hash-ref a-hash r)))))

; pair<list<atom>, string>, boolean, Run -> provenance-set
; Due to the way the evaluator works at the moment, this is always
; with respect to the current solver state for <a-run>.
(define (build-provenances tup negate? orig-run)
  (printf "build-provenances ~a~n" tup)
  ; get conjunction of predicates F from the run command
  (define spec (forge:Run-run-spec orig-run))
  (define Fs (forge:Run-spec-preds spec))
  ;(define F (and Fs)) ; drake saying no meme
  (define F (foldl (lambda (f acc) (and f acc)) (first Fs) (rest Fs))); drake saying yes meme
  (printf "  F: ~a~n" F) 
  
  (define state (forge:Run-spec-state spec))
  (define scope (forge:Run-spec-scope spec))
  (define sigs (forge:State-sigs state))
  (define relations (forge:State-relations state))  

;  pbindings ; Map<Symbol, sbound>
;  tbindings ; Map<Symbol, List<Symbol>>
  
  ; TODO This may only work if the solver state is the *first* instance in the stream
  ;   Confirm + discuss: what's the method by which the generator moves forward?
  (define orig-inst (stream-first (forge:Run-result orig-run)))
  (unless (symbol=? 'sat (car orig-inst))
    (error "amalgam called on unsat run"))  
  (printf "  orig-inst: ~a~n" orig-inst)
  (define new-totals (flip-tuple (cdr orig-inst) (car tup) (cdr tup)))
  (define bounds (forge:Bound (hash) new-totals)) 
  (printf "  bounds: ~a~n" bounds)
  ; can't use inst syntax here, so construct manually
  (define alt-inst
    (lambda (s b) (values scope bounds)))
  ; Get the solver to produce the L-alternate for us
  (run alt-run
       #:preds []
       #:bounds alt-inst)
  (printf "  first alt instance: ~a~n" (stream-first (forge:Run-result alt-run)))
  ; evaluate to make sure tup is locally necessary  
  (define check-alt (evaluate alt-run 'unused F))
  (printf "  check-alt: ~a~n" check-alt)

  ; desugar F
  ; Pass in the run, not the bounds, since we may need more of the run (like atom-rels)  
  ;(define desugared (desugar-formula F '() a-run))
  
  ; do amalgam descent on desugared F
  ;(amalgam-descent desugared orig-run alt-run)
  '())


; E.g., at REPL (or in separate module that requires the forge module)
; > (require racket/stream racket/base forge/amalgam)
; > (build-provenances (cons '(Node3 Node1) "edges") #f foo1)

(build-provenances (cons '(Node3 Node1) "edges") #f udt)


