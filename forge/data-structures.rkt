#lang racket

(require (prefix-in @ racket) 
         (prefix-in @ racket/set))

(require "shared.rkt")

(require (rename-in "lang/ast.rkt"
                    [node/expr/relation? ast-relation?]))
; (require racket/lazy-require)
; (lazy-require
;   ["lang/ast.rkt" (Int 
;                    succ 
;                    [node/expr/relation? ast-relation?]
;                    [node/expr/relation-name relation-name])])

; Data structures
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Data Structures ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct Sig (
  name       ; Symbol
  rel        ; node/expr/relation
  one        ; Boolean
  abstract   ; Boolean
  extends    ; Symbol | #f
  extenders  ; List<Symbol>
  ) #:transparent)

(struct Relation (
  name  ; Symbol
  rel   ; node/expr/relation
  sigs  ; List<Symbol>
  breaker ; Symbol
  ) #:transparent)

(struct Range (
  lower ; int
  upper ; int
  ) #:transparent)

(struct Scope (
  default-scope  ; Range | #f
  bitwidth       ; int | #f
  sig-scopes     ; Map<Symbol, Range>
  ) #:transparent)

(struct Bound (
  pbindings ; Map<Symbol, sbound>
  tbindings ; Map<Symbol, List<Symbol>>
  ) #:transparent)

(struct Options (
  solver          ; symbol
  verbosity       ; int
  sb              ; int
  coregranularity ; int
  logtranslation  ; int
  ) #:transparent)

(struct State (
  sigs        ; Map<Symbol, Sig>
  sig-order   ; List<Symbol>
  relations   ; Map<Symbol, Relation>
  relation-order ; List<Symbol>
  predicates  ; Set<Symbol>
  functions   ; Set<Symbol>
  constants   ; Set<Symbol>
  insts       ; Set<Symbol>
  options     ; Options
  ) #:transparent)

(struct Run-spec (
  state   ; State
  preds   ; Set<node/formula>
  scope   ; Scope
  bounds  ; Bound
  ) #:transparent)

(struct Run (
  name     ; Symbol
  command  ; String (syntax)
  run-spec ; Run-spec
  result   ; Stream
  atom-rels ; List<node/expr/relation>
  ) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  Initial State  ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define init-sigs (hash 'Int (Sig 'Int Int  #f #f #f empty)))
(define init-sig-order (list 'Int))
(define init-relations (hash 'succ (Relation 'succ succ '(Int Int) #f)))
(define init-relation-order (list 'succ))
(define init-predicates (@set))
(define init-functions (@set))
(define init-constants (@set))
(define init-insts (@set))
(define init-options (Options 'SAT4J 5 5 0 0))
(define init-state (State init-sigs init-sig-order
                          init-relations init-relation-order
                          init-predicates init-functions init-constants 
                          init-insts
                          init-options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;    Defaults     ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define DEFAULT-BITWIDTH 4)
(define DEFAULT-SIG-SCOPE (Range 0 4))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; State Accessors ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|
type AST-Relation = node/expr/Relation
type Sig* = (|| Sig AST-Relation)
type Relation* = (|| Relation AST-Relation)
type Run-or-State = (|| Run State)

get-state :: Run-or-State -> State
If run-or-state is a State, returns it;
if it is a Run-spec or a Run, then returns its state.

; Sig stuff

get-sig :: Run-or-State (|| Symbol AST-Relation) -> Sig
Returns the Sig of a given name/ast-relation from a run/state.

get-sigs :: Run-or-State, Relation*? -> List<Sig>
If a relation is provided, returns the column sigs;
otherwise, returns the Sigs of the given relation in a run/state.

get-top-level-sigs :: Run-or-State -> List<Sig>
Returns the Sigs in a run/state that do not extend another Sig.

get-children :: Run-or-State, Sig* -> List<Sig>
Returns the children Sigs of a Sig.

get-fields :: Run-or-State Sig* -> List<Relation>
Returns the relations whose first sig is the given sig.

; Relation stuff

get-relation :: Run-or-State, (|| Symbol AST-Relation) -> Relation
Returns the Relation of a given name/ast-relation from a run/state.

get-relations :: Run-or-State -> List<Relation>
Returns the Relations in a run/state.

; Result stuff

get-result :: Run -> Stream
Returns a stream of instances for the given run.

is-sat? :: Run -> boolean
is-unsat? :: Run -> boolean
Returns whether the given run resulted in sat or unsat, respectively.
|#

; get-state :: Run-or-State -> State
; If run-or-state is a State, returns it;
; if it is a Run-spec or a Run, then returns its state.
(define (get-state run-or-state)
  (cond [(Run? run-or-state)
         (Run-spec-state (Run-run-spec run-or-state))]
        [(Run-spec? run-or-state)
         (Run-spec-state run-or-state)]
        [(State? run-or-state)
         run-or-state]))

; get-sig :: Run-or-State (|| Symbol Sig*) -> Sig
; Returns the Sig of a given name/ast-relation from a run/state.
(define (get-sig run-or-state sig-name-or-rel)
  (define sig-name
    (cond [(symbol? sig-name-or-rel) sig-name-or-rel]
          [(ast-relation? sig-name-or-rel)
           (string->symbol (relation-name sig-name-or-rel))]
          [(Sig? sig-name-or-rel)
           (Sig-name sig-name-or-rel)]))
  (hash-ref (State-sigs (get-state run-or-state)) sig-name))

; get-sigs :: Run-or-State, Relation*? -> List<Sig>
; If a relation is provided, returns the column sigs;
; otherwise, returns the Sigs of the given relation in a run/state.
(define (get-sigs run-or-state [relation #f])
  (define state (get-state run-or-state))
  (if relation
      (map (curry get-sig state) 
           (Relation-sigs (get-relation state relation)))
      (map (curry hash-ref (State-sigs state )) 
           (State-sig-order state))))

; get-top-level-sigs :: Run-or-State -> List<Sig>
; Returns the Sigs in a run/state that do not extend another Sig.
(define (get-top-level-sigs run-or-state)
  (filter (compose @not Sig-extends) (get-sigs run-or-state)))

; get-fields :: Run-or-State Sig* -> List<Relation>
; Returns the relations whose first sig is the given sig.
(define (get-fields run-or-state sig-or-rel)
  (define state (get-state run-or-state))
  (define sig (get-sig state sig-or-rel))
  (define relations (get-relations state))

  (for/list ([relation relations]
             #:when (equal? (first (get-sigs state relation))
                            sig))
    relation))

; get-relation :: Run-or-State, (|| Symbol Relation*) -> Relation
; Returns the Relation of a given name/ast-relation from a run/state.
(define (get-relation run-or-state relation-name-or-rel)
  (define name
    (cond [(symbol? relation-name-or-rel) relation-name-or-rel]
          [(ast-relation? relation-name-or-rel)
           (string->symbol (relation-name relation-name-or-rel))]
          [(Relation? relation-name-or-rel)
           (Relation-name relation-name-or-rel)]))
  (hash-ref (State-relations (get-state run-or-state)) name))

; get-relations :: Run-or-State -> List<Relation>
; Returns the Relations in a run/state.
(define (get-relations run-or-state)
  (define state (get-state run-or-state))
  (map (curry hash-ref (State-relations state) )
       (State-relation-order state)))

; get-children :: Run-or-State, Sig* -> List<Sig>
; Returns the children Sigs of a Sig.
(define (get-children run-or-state sig-or-rel)
  (define children-names (Sig-extenders (get-sig run-or-state sig-or-rel)))
  (define sig-map (State-sigs (get-state run-or-state)))
  (map (curry hash-ref sig-map) children-names))

; get-result :: Run -> Stream
; Returns a stream of instances for the given run.
(define (get-result run)
  (Run-result run))

; get-pbinding :: Run-spec, Sig -> (|| List<List<Symbol>> #f)
; Returns the partial binding in a given Run-spec
; for a given Sig, returning #f if none present.
(define (get-sig-pbinding run-spec sig)
  (hash-ref (Bound-pbindings (Run-spec-bounds run-spec)) (Sig-name sig) #f))

; get-pbinding :: Run-spec, Sig -> (|| List<List<Symbol>> #f)
; Returns the total binding in a given Run-spec
; for a given Sig, returning #f if none present.
(define (get-sig-tbinding run-spec sig)
  (hash-ref (Bound-tbindings (Run-spec-bounds run-spec)) (Sig-name sig) #f))

; get-pbinding :: Run-spec, Relation -> (|| List<List<Symbol>> #f)
; Returns the partial binding in a given Run-spec
; for a given Relation, returning #f if none present.
(define (get-relation-pbinding run-spec rel)
  (hash-ref (Bound-pbindings (Run-spec-bounds run-spec)) (Relation-name rel) #f))

; get-tbinding :: Run-spec, Relation -> (|| List<List<Symbol>> #f)
; Returns the total binding in a given Run-spec
; for a given Relation, returning #f if none present.
(define (get-relation-tbinding run-spec rel)
  (hash-ref (Bound-tbindings (Run-spec-bounds run-spec)) (Relation-name rel) #f))

; get-scope :: (|| Run-spec Scope), (|| Sig Symbol) -> Range
; Returns the run bound of a Sig, in order:
; - if it is Int, then returns (Range 2^bitwidth 2^bitwidth);
; - if an explicit bound is given, returns it;
; - if a default bound is given; returns it;
; - return DEFAULT-SIG-BOUND
(define (get-scope run-spec-or-scope sig-or-name)
  (define scope 
    (cond [(Scope? run-spec-or-scope)
           run-spec-or-scope]
          [(Run-spec? run-spec-or-scope)
           (Run-spec-scope run-spec-or-scope)]))

  (define sig-name
    (cond [(Sig? sig-or-name)
           (Sig-name sig-or-name)]
          [(symbol? sig-or-name)
           sig-or-name]))

  (if (equal? sig-name 'Int)
      (let* ([bitwidth (get-bitwidth scope)]
             [num-ints (expt 2 bitwidth)])
        (Range num-ints num-ints))
      (let* ([scope-map (Scope-sig-scopes scope)]
             [default-scope (or (Scope-default-scope scope) 
                                DEFAULT-SIG-SCOPE)])
        (hash-ref scope-map sig-name default-scope))))

; get-bitwidth :: (|| Run-spec Scope) -> int
; Returns the bitwidth for a run/scope, returning the
; DEFAULT-BITWIDTH if none is provided.
(define (get-bitwidth run-spec-or-scope)
  (define scope
    (cond [(Run-spec? run-spec-or-scope)
           (Run-spec-scope run-spec-or-scope)]
          [(Scope? run-spec-or-scope)
           run-spec-or-scope]))
  (or (Scope-bitwidth scope)
      DEFAULT-BITWIDTH))

; get-all-rels :: (|| Run Run-spec) -> List<node/expr/relation>
; Returns a list of all sigs, then all relations, as
; their rels in the order they were defined; if given a Run,
; includes all of the additional relations used for individual
; atom access by the evaluator. 
; Used for translate to kodkod-cli.
(define (get-all-rels run-or-spec)
  (cond [(Run-spec? run-or-spec)
         (let ([run-spec run-or-spec])
           (append
             (map Sig-rel (get-sigs run-spec))
             (map Relation-rel (get-relations run-spec))))]
        [(Run? run-or-spec)
         (let ([run run-or-spec]
               [run-spec (Run-run-spec run-or-spec)])
           (append
             (map Sig-rel (get-sigs run-spec))
             (map Relation-rel (get-relations run-spec))
             (Run-atom-rels run)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; State Updaters  ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; sig-add-extender :: Sig, Symbol -> Sig
; Adds a new extender to the given Sig.
(define (sig-add-extender sig extender)
  (define new-extenders (append (Sig-extenders sig) (list extender)))
  (struct-copy Sig sig
               [extenders new-extenders]))

; state-add-sig :: State, Symbol, bool, bool, (Symbol | #f) -> State
; Adds a new Sig to the given State; if new Sig extends some
; other Sig, then updates that Sig with extension.
(define (state-add-sig state name rel one abstract extends)
  (define new-sig (Sig name rel one abstract extends empty))
  (when (@and extends (@not (member extends (State-sig-order state))))
    (raise "Can't extend non-existant sig."))

  (define sigs-with-new-sig (hash-set (State-sigs state) name new-sig))
  (define new-state-sigs
    (if extends
        (hash-set sigs-with-new-sig extends 
                                    (sig-add-extender (hash-ref (State-sigs state) extends) name))
        sigs-with-new-sig))
  (define new-state-sig-order (append (State-sig-order state) (list name)))

  (struct-copy State state
               [sigs new-state-sigs]
               [sig-order new-state-sig-order]))

; state-add-relation :: State, Symbol, List<Sig>, Symbol?-> State
; Adds a new relation to the given State.
(define (state-add-relation state name rel rel-sigs [breaker #f])
  (define new-relation (Relation name rel rel-sigs breaker))
  (define new-state-relations (hash-set (State-relations state) name new-relation))
  (define new-state-relation-order (append (State-relation-order state) (list name)))
  (struct-copy State state
               [relations new-state-relations]
               [relation-order new-state-relation-order]))

; state-add-predicate :: State, Symbol -> State
; Adds a new predicate to the given State.
(define (state-add-predicate state name)
  (define new-state-predicates (set-add (State-predicates state) name))
  (struct-copy State state
               [predicates new-state-predicates]))

; state-add-function :: State, Symbol -> State
; Adds a new function to the given State.
(define (state-add-function state name)
  (define new-state-functions (set-add (State-functions state) name))
  (struct-copy State state
               [functions new-state-functions]))

; state-add-constant :: State, Symbol -> State
; Adds a new constant to the given State.
(define (state-add-constant state name)
  (define new-state-constants (set-add (State-constants state) name))
  (struct-copy State state
               [constants new-state-constants]))
