#lang racket

(require "lang/ast.rkt"
         "lang/bounds.rkt"
         "breaks.rkt")
(require (prefix-in @ racket) 
         (prefix-in @ racket/set))

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

(struct Target (
  instance ; Map<Symbol, List<List<Symbol>>>
  distance ; 'close | 'far
  ) #:transparent)

; If adding new option fields, remember to update all of:
;  --default value -- state-set-option and -- get-option
(struct Options (
  solver          ; symbol
 ; verbosity       ; int ; handled in shared.rkt
  backend         ; symbol
  sb              ; int
  coregranularity ; int
  logtranslation  ; int
  min_tracelength ; int
  max_tracelength ; int
  problem_type    ; symbol
  target_mode     ; symbol
  core_minimization ; symbol
  skolem_depth ; int
  ) #:transparent)

(struct State (
  sigs        ; Map<Symbol, Sig>
  sig-order   ; List<Symbol>
  relations   ; Map<Symbol, Relation>
  relation-order ; List<Symbol>
  pred-map  ; Map<Symbol, Predicate>
  fun-map   ; Map<Symbol, Function>
  const-map   ; Map<Symbol, Constant>
  inst-map       ; Map<Symbol, Inst>
  options     ; Options
  runmap      ; Map<Symbol, Run> (as hash)
  ) #:transparent)

(struct Run-spec (
  state   ; State
  preds   ; Set<node/formula>
  scope   ; Scope
  bounds  ; Bound
  target  ; Target | #f
  ) #:transparent)

(struct Server-ports (
  stdin
  stdout
  shutdown
  is-running?) #:transparent)

(struct Kodkod-current (
  [formula #:mutable]
  [expression #:mutable]
  [int #:mutable]))

(struct Run (
  name     ; Symbol
  command  ; String (syntax)
  run-spec ; Run-spec
  result   ; Stream
  server-ports ; Server-ports
  atoms    ; List<Symbol>
  kodkod-currents ; Kodkod-current
  kodkod-bounds ; List<bound> (lower and upper bounds for each relation)
  ) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  Initial State  ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define init-sigs (hash 'Int (Sig 'Int Int  #f #f #f empty)))
(define init-sig-order (list 'Int))
(define init-relations (hash 'succ (Relation 'succ succ '(Int Int) #f)))
(define init-relation-order (list 'succ))
(define init-pred-map (@hash))
(define init-fun-map (@hash))
(define init-const-map (@hash))
(define init-inst-map (@hash))
(define init-runmap (@hash))
(define init-options (Options 'SAT4J 'pardinus 20 0 0 1 5 'default 'close-noretarget 'fast 0))
(define init-state (State init-sigs init-sig-order
                          init-relations init-relation-order
                          init-pred-map init-fun-map init-const-map
                          init-inst-map
                          init-options
                          init-runmap))

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
          [(node/expr/relation? sig-name-or-rel)
           (string->symbol (relation-name sig-name-or-rel))]
          [(Sig? sig-name-or-rel)
           (Sig-name sig-name-or-rel)]
          [else (error (format "get-sig failed to locate: ~a" sig-name-or-rel))]))
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
          [(node/expr/relation? relation-name-or-rel)
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

; get-pred :: Run-or-State, Symbol -> Predicate
; Gets a predicate by name from a given state
(define (get-pred run-or-state name)
  (define state (get-state run-or-state))
  (hash-ref (State-pred-map state) name))

; get-fun :: Run-or-State, Symbol -> Function
; Gets a function by name from a given state
(define (get-fun run-or-state name)
  (define state (get-state run-or-state))
  (hash-ref (State-fun-map state) name))

; get-const :: Run-or-State, Symbol -> Constant
; Gets a constant by name from a given state
(define (get-const run-or-state name)
  (define state (get-state run-or-state))
  (hash-ref (State-const-map state) name))

; get-inst :: Run-or-State, Symbol -> Inst
; Gets a inst by name from a given state
(define (get-inst run-or-state name)
  (define state (get-state run-or-state))
  (hash-ref (State-inst-map state) name))

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

; get-all-rels :: (|| Run Run-spec) -> List<AST-Relation>
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
             (map Relation-rel (get-relations run-spec))))]))

; get-relation-map :: (|| Run Run-spec) -> Map<Symbol, AST-Relation>
; Returns a map from names to AST-Relations.
(define (get-relation-map run-or-spec)
  (for/hash ([rel (get-all-rels run-or-spec)])
    (values (relation-name rel) rel)))
