;#lang racket
;(require "sigs.rkt")
; Why does the above cause an error?
#lang forge/core

;(set-verbosity 10)

;(require forge/amalgam/desugar/desugar)

(require "amalgam/tests/forge_ex.rkt")
(require racket/hash)
;(require (only-in "breaks.rkt" sbound))
; ^ This will be a *different* struct defn; instead get via sigs

(run udt
     #:preds [isUndirectedTree]
     #:scope [(Node 7)]) 

; Entry point for Amalgam from forge/core
;  This is a sketch for the moment

(provide build-provenances)

; takes a hash-table representation of an instance and a tuple and flips the truth of
;  that tuple in the instance. The tuple is <t> in <raw-r>.
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
  ;(printf "  F: ~a~n" F) 
  
  (define state (forge:Run-spec-state spec))
  (define orig-scope (forge:Run-spec-scope spec))
  (define orig-bounds (forge:Run-spec-bounds spec))
  (define sigs (forge:State-sigs state))
  (define relations (forge:State-relations state))
  
  ; TODO This may only work if the solver state is the *first* instance in the stream
  ;   Confirm + discuss: what's the method by which the generator moves forward?
  (define orig-inst (stream-first (forge:Run-result orig-run)))
  (unless (symbol=? 'sat (car orig-inst))
    (error "amalgam called on unsat run"))
  (printf "~n  first orig instance: ~a~n" orig-inst)
  ;(printf "~n  orig-inst: ~a~n" orig-inst)
  ;(printf "~n  orig-bounds: ~a~n" (forge:Run-spec-bounds spec))
  ;(printf "~n  orig-scope: ~a~n" orig-scope) 
  (define new-totals (flip-tuple (cdr orig-inst) (car tup) (cdr tup)))
  
  ; no. "total bindings" is a misnomer. instead need to provide sbounds in pbindings
  ; e.g. (for fixed edge relation)
  ;Original PBindings: 
  ; #hash(((relation 2 "edges" (Node Node) Node) .
  ;   #(struct:sbound (relation 2 "edges" (Node Node) Node)
  ;   #<set: (Node0 Node1)> #<set: (Node0 Node1)>))
  ;    ...
  ;(define bounds (forge:Bound (hash) new-totals))
  ;(define all-rels (hash-union sigs relations))
  
  (define (make-exact-sbound rel tups)
    (forge:sbound rel (list->set tups) (list->set tups)))
  
  ; For reasons passing understanding, get-relation returns a symbol...
  (define (get-actual-relation relname)
    (cond [(hash-has-key? sigs relname) (forge:Sig-rel (hash-ref sigs relname))]
          [(hash-has-key? relations relname) (forge:Relation-rel (hash-ref relations relname))]
          [else (error "unknown relation or sig" relname)]))
 
  (define new-pbindings
    (for/hash ([k (hash-keys new-totals)]);(hash-keys orig-bounds)])
      (values (get-actual-relation k)
              (make-exact-sbound (get-actual-relation k)
                                 (hash-ref new-totals k)))))
  
  ;(printf "~n~n  new-pbindings: ~a~n" new-pbindings)
 
  (define bounds (forge:Bound new-pbindings (forge:Bound-tbindings orig-bounds)))
  (define scope (forge:Scope #f #f (hash))) ; empty
  ;(printf "~n  bounds: ~a~n" bounds)
  ; can't use inst syntax here, so construct manually
  (define alt-inst
    (lambda (s b) (values scope bounds)))
  ; Get the solver to produce the L-alternate for us
  (run alt-run
       #:preds []
       #:bounds alt-inst)
  (printf "~n  first alt instance: ~a~n" (stream-first (forge:Run-result alt-run)))
  ;(printf "~n  ALT BOUNDS: ~a~n" (forge:Run-spec-bounds (forge:Run-run-spec alt-run)))
  ; evaluate to make sure tup is locally necessary  
  (define check-alt (evaluate alt-run 'unused F))
  (printf "~n  check-alt: ~a~n" check-alt)

  ; desugar F
  ; Pass in the run, not the bounds, since we may need more of the run (like atom-rels)  
  ;(define desugared (desugar-formula F '() a-run))
  
  ; do amalgam descent on desugared F
  ;(amalgam-descent desugared orig-run alt-run)
  '())


; E.g., at REPL (or in separate module that requires the forge module)
; > (require racket/stream racket/base forge/amalgam)
; > (build-provenances (cons '(Node3 Node1) "edges") #f foo1)

; This will produce an error if Node1 isn't in all-atoms
;   TODO
;(build-provenances (cons '(Node1 Node1) "edges") #f udt)

; these are OK assuming 3, 4, 5, 6 are used
;(build-provenances (cons '(Node3 Node3) "edges") #f udt) ; add
(build-provenances (cons '(Node4 Node5) "edges") #f udt) ; remove

(define (amalgam-descent a-run currTuple node)
  (match node    
    [(node/formula/op/&& info args)
     #t
     ]
    [(node/formula/op/|| info args)
     #t
     ]
    [(node/formula/op/in info args)
     #f
     ]
    ; not a base case in more efficient "desugar as needed only" version
    [(node/formula/op/! info args)
     #f
     ]))
