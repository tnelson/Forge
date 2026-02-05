#lang typed/racket/base/optional

; Structures and helper functions for running Forge, along with some constants and
; configuration code (e.g., most options).

(require forge/types/ast-adapter
         forge/lang/bounds
         forge/breaks
         forge/shared
         ; Import AST constructors needed for macros and helpers
         (only-in forge/lang/ast
                  &&/info ||/info =>/info !/info =/info in/info &/info ->/info
                  int>/info |int</info| int=/info ite/info))
(require/typed forge/lang/ast
  [pretty-type-of (-> Any String)]
  [deparse (-> Any String)])
(require (prefix-in @ (only-in racket hash not +))
         (only-in racket thunk curry)
         (prefix-in @ racket/set))
(require racket/match
         racket/set
         racket/list)
(require (for-syntax racket/base racket/syntax syntax/srcloc syntax/parse))
(require forge/types/lazy-tree-adapter)
(require syntax/srcloc)
; Typed imports for solver-specific functions
(require/typed forge/pardinus-cli/server/kks
  [pardinus-port (Parameterof Output-Port)]
  [(clear pardinus-clear) (-> Symbol Void)])
(require/typed forge/solver-specific/smtlib-shared
  [(smtlib-display cvc5-smtlib-display) (-> Output-Port String Void)])

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Data Structures ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Results from solver

; For a non-temporal result, just take the first element of instances
(struct Sat (
  [instances : Any] ; list of hashes
  [stats : Any]     ; association list
  [metadata : Any]  ; association list
  ) #:transparent)

(struct Unsat (
  ; If there's a core, there are two cases per component:
  ;  (1) a node: a known formula
  ;  (2) a string: an unknown formula (Kodkod couldn't map back this part of the core)
  [core : (U False (Listof (U node String)))]
  [stats : Any] ; association list
  [kind : Symbol] ; symbol
  ) #:transparent)

; For SMT backends only, may yield "unknown"
(struct Unknown (
  [stats : Any]    ; data on performance, translation, etc.
  [metadata : Any] ; any solver-specific data provided about the unknown result
  ) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sigs and Relations enrich the "relation" AST node with
; Forge-specific information, which often leads to added
; constraints.

; DO NOT EXTEND THIS STRUCT
(struct Sig node/expr/relation (
  [name : Symbol]
  [one : Boolean]
  [lone : Boolean]
  [abstract : Boolean]
  [extends : (U Sig False)]
  ) #:transparent)

; DO NOT EXTEND THIS STRUCT
; TODO: really this should be called "Field", since it represents that at the surface/core level.
(struct Relation node/expr/relation (
  [name : Symbol]
  [sigs-thunks : (Listof (-> Sig))]
  [breaker : (U node/breaking/break False)]
  ) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; There are many technical terms involved in Forge bounds; we define the key ones here.

;   A *SCOPE* defines the range of allowed cardinalities for a sig.
;     A scope may be declared *EXACT*, in which case the range is a single value.

;   A *BOUND* (which would more correctly be called a "relational bound") is a
;     pair of sets of tuples, one lower (which the relation must contain) and one
;     upper (which the relation may contain).
;     A bound may be declared *EXACT*, in which case the lower and upper bounds are equal.

;   A bound is *COMPLETE* if it fully defines the upper and lower bounds for a given relation.
;   A bound is *INCOMPLETE* if other tuples may be added to either upper or lower in the future.
;     Incomplete bounds are seen in piecewise definitions, where a user may use one bind declaration
;     to bound the value of a field for a specific atom; values for other atoms may be provided later.

; NOTE WELL:
; The structs below define an intermediate representation; the Kodkod bounds (produced in
; forge/send-to-solver) are what is actually sent to the solver.

; ALSO: be aware that the "bounds", "sbounds" etc. structs defined elsewhere are distinct from
; the Bounds struct defined here. At some point, we can perhaps condense these into a single IR.

; A Range contains the minimum and maximum scope for a relation.
(struct Range (
  [lower : (U Nonnegative-Integer False)]
  [upper : (U Nonnegative-Integer False)]
  ) #:transparent)

; A Scope represents the numeric size limitations on sigs in a run.
; This includes the range of possible bitwidths, and a default range
; to use for sigs whose scope is undefined.
(struct Scope (
  [default-scope : (U Range False)]
  [bitwidth : (U Nonnegative-Integer False)]
  [sig-scopes : (HashTable Symbol Range)]
  ) #:transparent)

; A PiecewiseBound represents an atom-indexed, incomplete partial bound. E.g., one might write:
;   `Alice.father in `Bob + `Charlie
;   `Bob.father in `Charlie + `David
; Note that a piecewise bound is not the same as a "partial" bound; a partial bound is complete,
; in the sense that only one bind declaration is possible for that relation.
(struct PiecewiseBound (
  [tuples : (Listof Tuple)]                ; first element is the indexed atom in the original piecewise bounds
  [atoms : (Listof FAtom)]                 ; which atoms have been bound? (distinguish "given none" from "none given")
  [operator : (U '= 'in 'ni)]              ; which operator mode?
  ) #:transparent)
(define-type PiecewiseBounds (HashTable node/expr/relation PiecewiseBound))

; Type for arguments to helper predicates and functions: expressions, int-expressions, or Racket integers
(define-type HelperArg (U Integer node/expr node/int))
; Type for a helper predicate: either a procedure (n-arg, or 0-arg before use) or a node/formula (0-arg after use)
(define-type HelperPred (U (->* () (#:info (U nodeinfo False)) #:rest HelperArg node/formula) node/formula))
; Type for a helper function: always a procedure returning node/expr
(define-type HelperFun (->* () (#:info (U nodeinfo False)) #:rest HelperArg node/expr))

; A Bound represents the set-based size limitations on sigs and relations in a run.
; Information from Scope(s) and Bounds(s) will be combined only once a run executes.
(struct Bound (
  ; pbindings: partial (but complete) bindings for a given relation
  [pbindings : (HashTable node/expr/relation sbound)]
  ; tbindings: total (and complete) bindings for a given relation; also known as an exact bound.
  [tbindings : (HashTable node/expr/relation Any)]
  ; incomplete bindings for a given relation, indexed by first column
  [piecewise : PiecewiseBounds]
  ; original AST nodes, for improving errors, indexed by relation
  [orig-nodes : (HashTable node/expr/relation (Listof node))]
  ) #:transparent)

; An Inst function is an accumulator of bounds information. It doesn't (necessarily)
; contain the full information about a run's scope, bounds, etc. Rather, it allows for
; the aggregation of this info across multiple `inst` declarations.
(struct Inst (
  [func : (-> Scope Bound (Values Scope Bound))]
  ) #:transparent)

; A Target describes the goal of a target-oriented model-finding run.
(define-type TargetMode (U 'close_noretarget 'far_noretarget 'close_retarget 'far_retarget 'hamming_cover))
(struct Target (
  [target : (U (HashTable Symbol (Listof (Listof (U Number Symbol)))) Inst)]
  ; This is not the same as option target_mode, which provides a global default.
  ; Rather, this is per target.
  [distance : TargetMode]
  ) #:transparent)

(define-type Multiplicity (U 'set 'lone 'one 'no 'func 'pfunc))
(struct expression-type (
  [type : (Listof (Listof Symbol))]
  [multiplicity : Multiplicity]
  [temporal-variance : (U Boolean String)]
  [top-level-types : (Listof (U 'Int 'univ))]
  ) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; If adding new option fields, remember to update all of:
;  -- DEFAULT_OPTIONS
;  -- option-types
;  -- option-types-names
;  -- state-set-option (in sigs.rkt or sigs-functional.rkt)
; Options are stored as a hash for flexibility; type safety is provided
; via case-> typing on get-option for known option keys.

(struct State (
  [sigs : (HashTable Symbol Sig)]
  [sig-order : (Listof Symbol)]
  [relations : (HashTable Symbol Relation)]
  [relation-order : (Listof Symbol)]
  [pred-map : (HashTable Symbol HelperPred)]
  [fun-map : (HashTable Symbol HelperFun)]
  [const-map : (HashTable Symbol node)]
  [inst-map : (HashTable Symbol Inst)]
  [options : (HashTable Symbol Any)]  ; hash-based options with case-> typed accessor
  [runmap : (HashTable Symbol Any)] ; TODO: Any -> Run
  ) #:transparent)

(struct Run-spec (
  [state : State]  ; Model state at the point of this run
  [preds : (Listof node/formula)] ; predicates to run, conjoined
  [scope : Scope]  ; Numeric scope(s)
  [bounds : Bound] ; set-based upper and lower bounds
  [target : (U Target False)] ; target-oriented model finding
  ) #:transparent)

(struct Server-ports (
  [stdin : Output-Port]
  [stdout : Input-Port]
  [stderr : Input-Port]
  [shutdown : (-> Void)]
  [is-running? : (-> Boolean)]
  ) #:transparent)

(struct Kodkod-current (
  [formula : Nonnegative-Integer]
  [expression : Nonnegative-Integer]
  [int : Nonnegative-Integer]) #:mutable)

(struct Run (
  [name : Symbol]
  [command : Syntax]
  [run-spec : Run-spec]
  ; This is the *start* of the exploration tree.
  [result : tree:node]
  [server-ports : Server-ports]
  [atoms : (Listof FAtom)]
  [kodkod-currents : Kodkod-current]
  [kodkod-bounds : (Listof Any)]
  ; This is Sterling's current cursor into the exploration tree.
  ; It is mutated whenever Sterling asks for a new instance. We keep this
  ; separately, since there may be multiple cursors into the lazy tree if
  ; the run is also being processed in a script, but the programmatic cursor
  ; and the Sterling cursor should not interfere.
  [last-sterling-instance : (Boxof (U Sat Unsat Unknown False))]
  ) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;    Defaults     ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define DEFAULT-BITWIDTH 4)
(define DEFAULT-SIG-SCOPE (Range 0 4))

; an engine_verbosity of 1 logs SEVERE level in the Java engine;
;   this will send back info about crashes, but shouldn't spam (and possibly overfill) stderr.
(define DEFAULT-OPTIONS : (HashTable Symbol Any)
  (hash 'eval-language     'surface
        'solver            'SAT4J
        'backend           'pardinus
        'sb                20
        'coregranularity   0
        'logtranslation    0
        'min_tracelength   1
        'max_tracelength   5
        'problem_type      'default
        'target_mode       'close_noretarget
        'core_minimization 'fast
        'skolem_depth      0
        'local_necessity   'off
        'run_sterling      'on
        'sterling_port     0
        'sterling_static_port 0
        'engine_verbosity  1
        'test_keep         'first
        'no_overflow       'false
        'java_exe_location #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;    Constants    ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Built-in Int sig and succ relation - defined as values with empty nodeinfo
; (source location tracking not needed for built-in constants)
(define Int : Sig (Sig empty-nodeinfo 1 "Int" (thunk '("Int")) "univ" #f 'Int #f #f #f #f))
(define succ : Relation (Relation empty-nodeinfo 2 "succ" (thunk '("Int" "Int")) "Int" #f 'succ (list (thunk Int) (thunk Int)) #f))

; These use the typed AST functional interface
(: max (-> node/expr node/int))
(define (max s-int)
  (sum/func (-/func s-int (join/func (^/func succ) s-int))))
(: min (-> node/expr node/int))
(define (min s-int)
  (sum/func (-/func s-int (join/func s-int (^/func succ)))))

; Helper for option type checking - returns a predicate that checks membership
(: oneof-pred (-> (Listof Symbol) (-> Any Boolean)))
(define (oneof-pred lst)
  (lambda ([x : Any]) (if (member x lst) #t #f)))

(define VALID_BUILTIN_SOLVERS '(SAT4J Glucose MiniSat MiniSatProver PMaxSAT4J))

(define option-types
  (hash 'eval-language symbol?
        ; allow for custom solver path given as a string
        'solver (lambda ([x : Any]) (or (member x VALID_BUILTIN_SOLVERS) (string? x)))
        'backend symbol?
        ; 'verbosity exact-nonnegative-integer?
        'sb exact-nonnegative-integer?
        'coregranularity exact-nonnegative-integer?
        'logtranslation exact-nonnegative-integer?
        'min_tracelength exact-positive-integer?
        'max_tracelength exact-positive-integer?
        'problem_type symbol?
        'target_mode (oneof-pred '(close_noretarget far_noretarget close_retarget far_retarget hamming_cover))
        'core_minimization symbol?
        'skolem_depth exact-integer?
        'local_necessity symbol?
        'run_sterling (lambda (x) (or (symbol? x)
                                      (string? x)
                                      (and (list? x)
                                           (andmap (lambda (ele) (string? ele)) x))))
        'sterling_port exact-nonnegative-integer?
        'sterling_static_port exact-nonnegative-integer?
        'engine_verbosity exact-nonnegative-integer?
        'test_keep (oneof-pred '(first last))
        'no_overflow (oneof-pred '(false true))
        'java_exe_location (lambda (x) (or (equal? x #f) (string? x)))))

(define option-types-names
  (hash 'eval-language "symbol"
        'solver (format "one of ~a or a path string" VALID_BUILTIN_SOLVERS)
        'backend "symbol"
        'sb "non-negative integer"
        'coregranularity "non-negative integer"
        'logtranslation "non-negative integer"
        'min_tracelength "positive integer"
        'max_tracelength "positive integer"
        'problem_type "symbol"
        'target_mode "one of: close_noretarget far_noretarget close_retarget far_retarget hamming_cover"
        'core_minimization "symbol"
        'skolem_depth "integer"
        'local_necessity "symbol"
        'run_sterling "symbol, string, or sequence of strings"
        'sterling_port "non-negative integer"
        'sterling_static_port "non-negative integer"
        'engine_verbosity "non-negative integer"
        'test_keep "one of: first or last"
        'no_overflow "one of: false or true"
        'java_exe_location "string"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  Initial State  ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define init-sigs (hash 'Int Int))
(define init-sig-order (list 'Int))
(define init-relations (hash 'succ succ))
(define init-relation-order (list 'succ))
(define init-pred-map : (HashTable Symbol HelperPred) (@hash))
(define init-fun-map : (HashTable Symbol HelperFun) (@hash))
(define init-const-map : (HashTable Symbol node) (@hash))
(define init-inst-map : (HashTable Symbol Inst) (@hash))
(define init-runmap : (HashTable Symbol Any) (@hash))
(define init-options DEFAULT-OPTIONS)
(define init-state (State init-sigs init-sig-order
                          init-relations init-relation-order
                          init-pred-map init-fun-map init-const-map
                          init-inst-map
                          init-options
                          init-runmap))


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

; Type alias for common parameter type
(define-type Run-or-State (U Run Run-spec State))

; get-state :: Run-or-State -> State
; If run-or-state is a State, returns it;
; if it is a Run-spec or a Run, then returns its state.
(: get-state (-> Run-or-State State))
(define (get-state run-or-state)
  (cond [(Run? run-or-state)
         (Run-spec-state (Run-run-spec run-or-state))]
        [(Run-spec? run-or-state)
         (Run-spec-state run-or-state)]
        [(State? run-or-state)
         run-or-state]))

; get-run-spec :: Run-or-State -> Run-spec
(: get-run-spec (-> (U Run Run-spec) Run-spec))
(define (get-run-spec run-or-state)
    (cond [(Run? run-or-state)
         (Run-run-spec run-or-state)]
        [(Run-spec? run-or-state)
         run-or-state]))

; get-sig :: Run-or-State (|| Symbol Sig*) -> Sig
; Returns the Sig of a given name/ast-relation from a run/state.
(: get-sig (-> Run-or-State (U Symbol node/expr/relation) (U Sig False)))
(define (get-sig run-or-state sig-name-or-rel)
  (define sig-name : Symbol
    (cond [(symbol? sig-name-or-rel) sig-name-or-rel]
          [(Sig? sig-name-or-rel)
           (Sig-name sig-name-or-rel)]
          [(node/expr/relation? sig-name-or-rel)
           (string->symbol (relation-name sig-name-or-rel))]
          [else (raise-forge-error #:msg (format "get-sig failed to locate: ~a" sig-name-or-rel)
                                   #:context #f)]))
  (cond [(hash-has-key? (State-sigs (get-state run-or-state)) sig-name)
         (hash-ref (State-sigs (get-state run-or-state)) sig-name)]
         [else #f]))

; get-sigs :: Run-or-State, Relation*? -> List<Sig>
; If a relation is provided, returns the column sigs;
; otherwise, returns the Sigs of the given relation in a run/state.
(: get-sigs (->* (Run-or-State) ((U False node/expr/relation)) (Listof Sig)))
(define (get-sigs run-or-state [relation #f])
  (define state (get-state run-or-state))
  (if relation
      (map (lambda ([sig-thunk : (-> Sig)]) (assert (get-sig state (sig-thunk)) Sig?))
           (Relation-sigs-thunks (assert (get-relation state relation) Relation?)))
      (map (lambda ([s : Symbol]) (hash-ref (State-sigs state) s))
           (State-sig-order state))))

; get-top-level-sigs :: Run-or-State -> List<Sig>
; Returns the Sigs in a run/state that do not extend another Sig.
(: get-top-level-sigs (-> Run-or-State (Listof Sig)))
(define (get-top-level-sigs run-or-state)
  (filter (lambda ([s : Sig]) (@not (Sig-extends s))) (get-sigs run-or-state)))

; get-fields :: Run-or-State Sig* -> List<Relation>
; Returns the relations whose first sig is the given sig.
(: get-fields (-> Run-or-State (U Sig node/expr/relation) (Listof Relation)))
(define (get-fields run-or-state sig-or-rel)
  (define state (get-state run-or-state))
  (define sig (get-sig state sig-or-rel))
  (define relations (get-relations state))

  (for/list ([relation : Relation relations]
             #:when (equal? (first (get-sigs state relation))
                            sig))
    relation))

; get-relation :: Run-or-State, (|| Symbol Relation*) -> Relation
; Returns the Relation of a given name/ast-relation from a run/state.
(: get-relation (-> Run-or-State (U Symbol node/expr/relation) (U Relation False)))
(define (get-relation run-or-state relation-name-or-rel)
  (define name : Symbol
    (cond [(symbol? relation-name-or-rel) relation-name-or-rel]
          [(node/expr/relation? relation-name-or-rel)
           (string->symbol (relation-name relation-name-or-rel))]
          [(Relation? relation-name-or-rel)
           (Relation-name relation-name-or-rel)]
          [else (error "get-relation: unexpected input")]))
  (cond [(hash-has-key? (State-relations (get-state run-or-state)) name)
         (hash-ref (State-relations (get-state run-or-state)) name)]
        [else #f]))

; get-relations :: Run-or-State -> List<Relation>
; Returns the Relations in a run/state.
(: get-relations (-> Run-or-State (Listof Relation)))
(define (get-relations run-or-state)
  (define state (get-state run-or-state))
  (map (lambda ([s : Symbol]) (hash-ref (State-relations state) s))
       (State-relation-order state)))

; get-pred :: Run-or-State, Symbol -> HelperPred or #f
; Gets a predicate by name from a given state, or #f if not found.
; Note that this will return the procedure, not the macro (no stx loc capture)
(: get-pred (-> Run-or-State Symbol (U HelperPred False)))
(define (get-pred run-or-state name)
  (define state (get-state run-or-state))
  (hash-ref (State-pred-map state) name #f))

; get-fun :: Run-or-State, Symbol -> HelperFun or #f
; Gets a function by name from a given state, or #f if not found.
; Note that this will return the procedure, not the macro (no stx loc capture)
(: get-fun (-> Run-or-State Symbol (U HelperFun False)))
(define (get-fun run-or-state name)
  (define state (get-state run-or-state))
  (hash-ref (State-fun-map state) name #f))

; get-const :: Run-or-State, Symbol -> Constant or #f
; Gets a constant by name from a given state, or #f if not found.
(: get-const (-> Run-or-State Symbol (U node False)))
(define (get-const run-or-state name)
  (define state (get-state run-or-state))
  (hash-ref (State-const-map state) name #f))

; get-inst :: Run-or-State, Symbol -> Inst or #f
; Gets an inst by name from a given state, or #f if not found.
(: get-inst (-> Run-or-State Symbol (U Inst False)))
(define (get-inst run-or-state name)
  (define state (get-state run-or-state))
  (hash-ref (State-inst-map state) name #f))

; get-children :: Run-or-State, Sig* -> List<Sig>
; Returns the children Sigs of a Sig.
(: get-children (-> Run-or-State (U Symbol Sig node/expr/relation) (Listof Sig)))
(define (get-children run-or-state sig-or-rel)
  (define sigs (get-sigs run-or-state))
  (define parent (get-sig run-or-state sig-or-rel))
  (filter (lambda ([sig : Sig]) (equal? (Sig-extends sig) parent)) sigs))

; get-result :: Run -> Stream
; Returns a stream of instances for the given run.
(: get-result (-> Run tree:node))
(define (get-result run)
  (Run-result run))

; get-pbinding :: Run-spec, Sig -> (|| List<List<Symbol>> #f)
; Returns the partial binding in a given Run-spec
; for a given Sig, returning #f if none present.
(: get-sig-pbinding (-> Run-spec Sig Any))
(define (get-sig-pbinding run-spec sig)
  (hash-ref (Bound-pbindings (Run-spec-bounds run-spec)) sig #f))

; get-pbinding :: Run-spec, Sig -> (|| List<List<Symbol>> #f)
; Returns the total binding in a given Run-spec
; for a given Sig, returning #f if none present.
(: get-sig-tbinding (-> Run-spec Sig Any))
(define (get-sig-tbinding run-spec sig)
  (hash-ref (Bound-tbindings (Run-spec-bounds run-spec)) sig #f))

; get-pbinding :: Run-spec, Relation -> (|| List<List<Symbol>> #f)
; Returns the partial binding in a given Run-spec
; for a given Relation, returning #f if none present.
(: get-relation-pbinding (-> Run-spec Relation Any))
(define (get-relation-pbinding run-spec rel)
  (hash-ref (Bound-pbindings (Run-spec-bounds run-spec)) rel #f))

; get-tbinding :: Run-spec, Relation -> (|| List<List<Symbol>> #f)
; Returns the total binding in a given Run-spec
; for a given Relation, returning #f if none present.
(: get-relation-tbinding (-> Run-spec Relation Any))
(define (get-relation-tbinding run-spec rel)
  (hash-ref (Bound-tbindings (Run-spec-bounds run-spec)) rel #f))

; get-scope :: (|| Run-spec Scope), (|| Sig Symbol) -> Range
; Returns the run bound of a Sig, in order:
; - if it is Int, then returns (Range 2^bitwidth 2^bitwidth);
; - if an explicit bound is given, returns it;
; - if a default bound is given; returns it;
; - return DEFAULT-SIG-BOUND
(: get-scope (-> (U Run-spec Scope) (U Sig Symbol) Range))
(define (get-scope run-spec-or-scope sig-or-name)
  (define scope : Scope
    (cond [(Scope? run-spec-or-scope)
           run-spec-or-scope]
          [(Run-spec? run-spec-or-scope)
           (Run-spec-scope run-spec-or-scope)]))

  (define sig-name : Symbol
    (cond [(Sig? sig-or-name)
           (Sig-name sig-or-name)]
          [(symbol? sig-or-name)
           sig-or-name]))

  (if (equal? sig-name 'Int)
      (let* ([bitwidth (get-bitwidth scope)]
             [num-ints (assert (expt 2 bitwidth) exact-nonnegative-integer?)])
        (Range num-ints num-ints))
      (let* ([scope-map (Scope-sig-scopes scope)]
             [default-scope (or (Scope-default-scope scope)
                                DEFAULT-SIG-SCOPE)])
        (hash-ref scope-map sig-name (lambda () default-scope)))))

; get-bitwidth :: (|| Run-spec Scope) -> int
; Returns the bitwidth for a run/scope, returning the
; DEFAULT-BITWIDTH if none is provided.
(: get-bitwidth (-> (U Run-spec Scope) Integer))
(define (get-bitwidth run-spec-or-scope)
  (define scope : Scope
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
(: get-all-rels (-> (U Run Run-spec) (Listof node/expr/relation)))
(define (get-all-rels run-or-spec)
  (cond [(Run-spec? run-or-spec)

         (let ([run-spec run-or-spec])
           (append
             (get-sigs run-spec)
             (get-relations run-spec)))]
        [(Run? run-or-spec)
         (let ([run run-or-spec]
               [run-spec (Run-run-spec run-or-spec)])
           (append
             (get-sigs run-spec)
             (get-relations run-spec)))]))

; get-relation-map :: (|| Run Run-spec) -> Map<Symbol, AST-Relation>
; Returns a map from names to AST-Relations.
(: get-relation-map (-> (U Run Run-spec) (HashTable String node/expr/relation)))
(define (get-relation-map run-or-spec)
  (for/hash : (HashTable String node/expr/relation) ([rel (get-all-rels run-or-spec)])
    (values (relation-name rel) rel)))

; get-option :: Run-or-state Symbol -> Any
; Returns the value of an option from the state's options hash.
; Note: callers needing specific types should cast the result.
(: get-option (-> Run-or-State Symbol Any))
(define (get-option run-or-state option)
  (define state (get-state run-or-state))
  (hash-ref (State-options state) option #f))

; is-sat? :: Run -> boolean
; Checks if a given run result is 'sat
(: is-sat? (-> Run Boolean))
(define (is-sat? run)
  (define first-instance (tree:get-value (Run-result run)))
  (Sat? first-instance))

; is-unsat? :: Run -> boolean
; Checks if a given run result is 'unsat
(: is-unsat? (-> Run Boolean))
(define (is-unsat? run)
  (define first-instance (tree:get-value (Run-result run)))
  (Unsat? first-instance))

; is-unknown? :: Run -> boolean
; Checks if a given run result is 'unknown. This kind of result won't be given
; by all kinds of solver backends, but some do produce it.
(: is-unknown? (-> Run Boolean))
(define (is-unknown? run)
  (define first-instance (tree:get-value (Run-result run)))
  (Unknown? first-instance))


; get-stdin :: Run -> input-port?
(: get-stdin (-> Run Output-Port))
(define (get-stdin run)
  (assert-solver-process-alive run)
  (Server-ports-stdin (Run-server-ports run)))

; get-stdout :: Run -> output-port?
(: get-stdout (-> Run Input-Port))
(define (get-stdout run)
  (assert-solver-process-alive run)
  (Server-ports-stdout (Run-server-ports run)))

; get-stderr :: Run -> output-port?
(: get-stderr (-> Run Input-Port))
(define (get-stderr run)
  (assert-solver-process-alive run)
  (Server-ports-stderr (Run-server-ports run)))

;;;;;;;;;;;;;;;;;;;;;;;
; Per-run closed status

; Keep track of which runs have been closed via close-run
(: closed-run-names (Boxof (Listof Symbol)))
(define closed-run-names (box (list)))
; Allows other modules to let this layer know a run is closed; this box
; is referenced by the instance generator for each run.
(: add-closed-run-name! (-> Symbol Void))
(define (add-closed-run-name! name)
  (set-box! closed-run-names (cons name (unbox closed-run-names))))
(: is-run-closed? (-> (U Symbol Run) Boolean))
(define (is-run-closed? name-or-run)
  (define (truthify [x : Any]) : Boolean (if x #t #f))
  (truthify
   (cond [(Run? name-or-run)
          (member (Run-name name-or-run) (unbox closed-run-names))]
         [else
          (member name-or-run (unbox closed-run-names))])))

; close-run :: Run -> void
; Idempotent: safe to call multiple times on the same run.
(: close-run (-> Run Void))
(define (close-run run)
  (unless (is-run-closed? run)
    (assert-solver-process-alive run)
    (when (>= (get-verbosity) VERBOSITY_HIGH)
          (printf "Clearing run ~a. Keeping engine process active...~n" (Run-name run)))
    ; Cut off this Run's ability to query the solver, since it's about to be closed
    ; This state is referenced in the instance-generator thunk
    (add-closed-run-name! (Run-name run))

    ; Since we're using a single process now, send it instructions to clear this run
    ; Different backends will be cleared in different ways.
    (define backend (get-option (Run-run-spec run) 'backend))
    (match backend
      ['pardinus
       (parameterize ([pardinus-port (get-stdin run)])
         (pardinus-clear (Run-name run))
         (flush-output (get-stdin run)))]
      ['smtlibtor
       (cvc5-smtlib-display (get-stdin run) "(reset)")]
      [else
       (raise-forge-error #:msg (format "Unsupported backend when closing solver run: ~a" backend)
                          #:context #f)])))

; is-solver-process-alive? :: Run -> Boolean
; This reports whether the _solver server process_ is running;
; *NOT* whether an individual run is still open (use is-run-closed? for that).
(: is-solver-process-alive? (-> Run Boolean))
(define (is-solver-process-alive? run)
  ((Server-ports-is-running? (Run-server-ports run))))

(: assert-solver-process-alive (-> Run Void))
(define (assert-solver-process-alive run)
  (unless (is-solver-process-alive? run)
    (raise-user-error "Solver process is not running.")))

(require (for-syntax syntax/srcloc)) ; for these macros

;; Added sugar over the AST
;; It is vital to PRESERVE SOURCE LOCATION in these, or else errors and highlighting may focus 
;; on the macro definition point
(provide implies iff <=> ifte int>= int<= ni != !in !ni <: :> xor)

; (xor a b) ----> (or (and a (! b)) (and (! a) b))
(define-syntax (xor stx)
  (syntax-parse stx
    [((~datum xor) (~optional (~and #:lang check-lang)) a b)
     (with-syntax ([newloc (quasisyntax/loc stx (nodeinfo #,(build-source-location stx)
                                                          (~? check-lang 'checklangNoCheck)
                                                          #f))])
       (quasisyntax/loc stx (||/info newloc
                                     (&&/info newloc a (!/info newloc b))
                                     (&&/info newloc (!/info newloc a) b))))]))

   
(define-syntax (implies stx) 
  (syntax-case stx () 
    [(_ (#:lang check-lang) a b) 
      (quasisyntax/loc stx  (=>/info (nodeinfo #,(build-source-location stx) check-lang #f) a b))]
    [(_ a b) 
      (quasisyntax/loc stx  (=>/info (nodeinfo #,(build-source-location stx) 'checklangNoCheck #f) a b))]))


(define-syntax (iff stx) 
  (syntax-case stx () 
    [(_ (#:lang check-lang) a b) 
      (quasisyntax/loc stx 
        (&&/info (nodeinfo #,(build-source-location stx) check-lang #f)
                 (=>/info (nodeinfo #,(build-source-location stx) check-lang #f) a b)
                 (=>/info (nodeinfo #,(build-source-location stx) check-lang #f) b a)))]
    [(_ a b) 
      (quasisyntax/loc stx 
        (&&/info (nodeinfo #,(build-source-location stx) 'checklangNoCheck #f)
                 (=>/info (nodeinfo #,(build-source-location stx) 'checklangNoCheck #f) a b)
                 (=>/info (nodeinfo #,(build-source-location stx) 'checklangNoCheck #f) b a)))]))
(define-syntax (<=> stx) 
  (syntax-case stx () 
    [(_ (#:lang check-lang) a b) (quasisyntax/loc stx (&&/info (nodeinfo #,(build-source-location stx) check-lang #f)
                                  (=>/info (nodeinfo #,(build-source-location stx) check-lang #f) a b)
                                  (=>/info (nodeinfo #,(build-source-location stx) check-lang #f) b a)))]
    [(_ a b) (quasisyntax/loc stx (&&/info (nodeinfo #,(build-source-location stx) 'checklangNoCheck #f)
                                  (=>/info (nodeinfo #,(build-source-location stx) 'checklangNoCheck #f) a b)
                                  (=>/info (nodeinfo #,(build-source-location stx) 'checklangNoCheck #f) b a)))]))

; Helper to convert int expressions to node/expr for ite
(: ensure-expr (-> Any nodeinfo node/expr))
(define (ensure-expr x info)
  (cond [(node/expr? x) (assert x node/expr?)]
        [(node/int? x) (sing/func (assert x node/int?) #:info info)]
        [(exact-integer? x) (sing/func (int/func x #:info info) #:info info)]
        [else (raise-forge-error #:msg (format "Expected expression, got ~a" (pretty-type-of x))
                                 #:context info)]))

; for ifte, use struct type to decide whether this is a formula (sugar)
; or expression form (which has its own AST node). Avoid exponential
; blowup from chained IFTEs by expanding to a chain of function calls.
; Uses typed AST functions for proper type safety.
(: ifte-disambiguator (-> nodeinfo Any Any Any (U node/formula node/expr)))
(define (ifte-disambiguator info a b c)
  (unless (node/formula? a)
    (raise-forge-error
     #:msg (format "If-then-else needed a boolean-valued formula for its first argument; got ~a." (pretty-type-of a))
     #:context a))
  ; Type narrowing: after the check above, a is known to be node/formula
  (define a-fmla : node/formula (assert a node/formula?))
  (cond
    ; It's a formula if-then-else: (a => b) && (!a => c) = (!a || b) && (a || c)
    [(and (node/formula? b) (node/formula? c))
     (define b-fmla : node/formula (assert b node/formula?))
     (define c-fmla : node/formula (assert c node/formula?))
     (&&/func (||/func (!/func a-fmla #:info info) b-fmla #:info info)
              (||/func a-fmla c-fmla #:info info)
              #:info info)]
    ; It's an expression if-then-else (note: mixing int-expr and rel-expr is OK)
    [(and (or (node/expr? b) (node/int? b) (integer? b))
          (or (node/expr? c) (node/int? c) (integer? c)))
     (ite/func info a-fmla (ensure-expr b info) (ensure-expr c info))]
    ; It's an error
    [else
     (raise-forge-error #:msg (format "If-then-else needed consistent types (either both formulas or both expressions) for its true and false branches, but got (~a) and (~a)."
                                      (pretty-type-of b) (pretty-type-of c))
                        #:context info)]))

(define-syntax (ifte stx)
  (syntax-parse stx 
    [(_ (~optional (#:lang check-lang) #:defaults ([check-lang #''checklangNoCheck])) a b c) (quasisyntax/loc stx
                 (ifte-disambiguator (nodeinfo #,(build-source-location stx) check-lang #f) a b c))]))

(define-syntax (ni stx) (syntax-case stx () 
      [(_ a b) (quasisyntax/loc stx (in/info (nodeinfo #,(build-source-location stx) 'checklangNoCheck #f) b a))]
      [(_ (#:lang check-lang) a b) (quasisyntax/loc stx (in/info (nodeinfo #,(build-source-location stx) check-lang #f) b a))]))
(define-syntax (!= stx) (syntax-case stx () [(_ a b) (quasisyntax/loc stx (!/info (nodeinfo #,(build-source-location stx) 'checklangNoCheck #f)
                                                             (=/info (nodeinfo #,(build-source-location stx) 'checklangNoCheck #f) a b)))]
                                            [(_ (#:lang check-lang) a b) (quasisyntax/loc stx 
                                                             (!/info (nodeinfo #,(build-source-location stx) check-lang #f)
                                                                     (=/info (nodeinfo #,(build-source-location stx) check-lang #f) a b)))]))
(define-syntax (!in stx) (syntax-parse stx [(_ (~optional (#:lang check-lang) #:defaults ([check-lang #''checklangNoCheck])) a b) 
                                                    (quasisyntax/loc stx  (!/info (nodeinfo #,(build-source-location stx) check-lang #f)
                                                              (in/info (nodeinfo #,(build-source-location stx) check-lang #f) a b)))]))
(define-syntax (!ni stx) (syntax-parse stx [(_ (~optional (#:lang check-lang) #:defaults ([check-lang #''checklangNoCheck])) a b) 
                                                    (quasisyntax/loc stx (!/info (nodeinfo #,(build-source-location stx) check-lang #f)
                                                              (in/info (nodeinfo #,(build-source-location stx) check-lang #f) b a)))]))
(define-syntax (int>= stx) (syntax-case stx () [(_ a b) (quasisyntax/loc stx (||/info (nodeinfo #,(build-source-location stx) 'checklangNoCheck #f)
                                                              (int>/info (nodeinfo #,(build-source-location stx) 'checklangNoCheck #f) a b)
                                                              (int=/info (nodeinfo #,(build-source-location stx) 'checklangNoCheck #f) a b)))]
                                            [(_ (#:lang check-lang) a b) (quasisyntax/loc stx (||/info (nodeinfo #,(build-source-location stx) check-lang #f)
                                                              (int>/info (nodeinfo #,(build-source-location stx) check-lang #f) a b)
                                                              (int=/info (nodeinfo #,(build-source-location stx) check-lang #f) a b)))]))
(define-syntax (int<= stx) (syntax-case stx () [(_ a b) (quasisyntax/loc stx (||/info (nodeinfo #,(build-source-location stx) 'checklangNoCheck #f)
                                                              (int</info (nodeinfo #,(build-source-location stx) 'checklangNoCheck #f) a b)
                                                              (int=/info (nodeinfo #,(build-source-location stx) 'checklangNoCheck #f) a b)))]
                                            [(_ (#:lang check-lang) a b) (quasisyntax/loc stx (||/info (nodeinfo #,(build-source-location stx) check-lang #f)
                                                              (int</info (nodeinfo #,(build-source-location stx) check-lang #f) a b)
                                                              (int=/info (nodeinfo #,(build-source-location stx) check-lang #f) a b)))]))

(define-syntax (<: stx) 
  (syntax-case stx () 
    [(_ a b) 
      (quasisyntax/loc stx 
        (<:helper a b (nodeinfo #,(build-source-location stx) 'checklangNoCheck #f)))]
    [(_ (#:lang check-lang) a b) 
      (quasisyntax/loc stx 
        (<:helper a b (nodeinfo #,(build-source-location stx) check-lang #f)))]))

; TODO: this only functions for binary relations
(: <:helper (-> node/expr node/expr nodeinfo node/expr))
(define (<:helper a b info)
  (domain-check<: a b (nodeinfo-loc info))
  (&/func b (->/func a univ #:info info) #:info info))

(define-syntax (:> stx)
  (syntax-case stx ()
    [(_ a b)
      (quasisyntax/loc stx
        (:>helper a b (nodeinfo #,(build-source-location stx) 'checklangNoCheck #f)))]
    [(_ (#:lang check-lang) a b)
      (quasisyntax/loc stx
        (:>helper a b (nodeinfo #,(build-source-location stx) check-lang #f)))]))

; TODO: this only functions for binary relations
(: :>helper (-> node/expr node/expr nodeinfo node/expr))
(define (:>helper a b info)
  (domain-check:> a b (nodeinfo-loc info))
  (&/func a (->/func univ b #:info info) #:info info))

(: domain-check<: (-> node/expr node/expr Any Void))
(define (domain-check<: a b loc)
  (unless (equal? (node/expr-arity b)
                  (@+ 1 (node/expr-arity a)))
    (raise-forge-error
     #:msg (format "<: argument has incorrect arity (~a vs. ~a) in ~a <: ~a"
                   (node/expr-arity a) (node/expr-arity b) (deparse a) (deparse b))
     #:context loc)))

(: domain-check:> (-> node/expr node/expr Any Void))
(define (domain-check:> a b loc)
  (unless (equal? (node/expr-arity a)
                  (@+ 1 (node/expr-arity b)))
    (raise-forge-error
     #:msg (format ":> argument has incorrect arity (~a vs. ~a) in ~a :> ~a"
                   (node/expr-arity a) (node/expr-arity b) (deparse a) (deparse b))
     #:context loc)))

; A Field relation is functional if it has a functional breaker assigned.
(: Relation-is-functional? (-> Any Boolean))
(define (Relation-is-functional? r)
  (if (Relation-is? r '(pfunc func)) #t #f))

(: Relation-is? (-> Any (Listof Symbol) Boolean))
(define (Relation-is? r sym-list)
  (and (Relation? r)
       (node/breaking/break? (Relation-breaker r))
       (if (member (node/breaking/break-break (Relation-breaker r)) sym-list) #t #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; "Primification"-related utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Do not check integer literals with respect to bitwidth for these backends
(define UNBOUNDED_INT_BACKENDS : (Listof Symbol) '(smtlibtor))

; Turn signame into list of all primsigs it contains
; Note we use Alloy-style "_remainder" names here; these aren't necessarily embodied in Forge
(: primify (-> (U Run Run-spec State) (U Symbol String) (Listof Symbol)))
(define (primify run-or-state raw-signame)
  (let ([signame : Symbol (cond [(string? raw-signame) (string->symbol raw-signame)]
                                [(Sig? raw-signame) (Sig-name raw-signame)]
                                [else raw-signame])])
    (cond [(equal? 'Int signame)
           '(Int)]
          [(equal? 'univ signame)
           (if (member (get-option (get-run-spec (assert run-or-state (lambda (x) (or (Run? x) (Run-spec? x))))) 'backend) UNBOUNDED_INT_BACKENDS)
           (remove-duplicates (append* (map (lambda ([n : Symbol]) (primify run-or-state n)) (remove 'Int (map Sig-name (get-sigs run-or-state))))))
           (remove-duplicates (append* (map (lambda ([n : Symbol]) (primify run-or-state n)) (cons 'Int (map Sig-name (get-sigs run-or-state)))))))]
          [else
           (define the-sig (get-sig run-or-state signame))

           (define all-primitive-descendants : (Listof Symbol)
             (remove-duplicates
              (append*
               (map (lambda ([n : Sig]) (primify run-or-state (Sig-name n)))
                    (get-children run-or-state signame)))))
           (cond
             [(and the-sig (Sig-abstract the-sig))

             (if (empty? (get-children run-or-state signame))
                 (raise-forge-error
                  #:msg (format "The abstract sig ~a is not extended by any children" (symbol->string signame))
                  #:context the-sig)
                 all-primitive-descendants)]
             [else (cons
                        (string->symbol (string-append (symbol->string signame)
                            (if (empty? (get-children run-or-state signame))
                                ""
                                "_remainder")))
                         all-primitive-descendants)])])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Reversing `primify` to produce a shortest set of sig names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; We assume that the list of sigs given is already primified; i.e., there are no non-primitive
; sig names (X_remainder counts as a primitive sig) being passed to this function.
; This version works only for lists of primified sig symbols, e.g. (A B C D_remainder)
(: deprimify (-> Run-or-State (Listof Symbol) (Listof Symbol)))
(define (deprimify run-or-state primsigs)
  (let ([all-sigs (map Sig-name (get-sigs run-or-state))])
    (cond
      ; In case this is a singleton list, we can't improve anything
      [(and (list? primsigs) (equal? 1 (length primsigs)))
       primsigs]
      ; In case all sigs are represented here, it's univ
      [(equal? (list->set primsigs)
               (list->set (remove-duplicates (flatten (map (lambda ([n : Symbol]) (primify run-or-state n)) (cons 'Int all-sigs))))))
       '(univ)]
      ; Otherwise, compress as much as possible
      ; Use primify to handle the X_remainder cases.
      [else (define top-level (get-top-level-sigs run-or-state))
            (define pseudo-fold-lambda (lambda ([sig : Sig] [acc : (Listof Symbol)])
                                         (if (or (subset? (list->set (primify run-or-state (Sig-name sig))) (list->set (flatten primsigs)))
                                                 (equal? (list (car (primify run-or-state (Sig-name sig)))) (flatten primsigs)))
                                             ; the above check is added for when you have the parent sig, but are expecting the child
                                             (values (append acc (list (Sig-name sig))) #t) ; replace cons with values
                                             (values acc #f))))
            (define final-list (dfs-sigs run-or-state pseudo-fold-lambda top-level '()))
            final-list])))

; Runs a DFS over the sigs tree, starting from sigs in <sigs>.
; On each visited sig, <func> is called to obtain a new accumulated value
; and whether the search should continue to that sig's children.
(: dfs-sigs (All (A) (-> Run-or-State (-> Sig A (Values A Boolean)) (Listof Sig) A A)))
(define (dfs-sigs run-or-state func sigs init-acc)
    (define (dfs-sigs-helper [todo : (Listof Sig)] [acc : A]) : A
      (cond [(equal? (length todo) 0) acc]
      [else (define next (first todo))
      (define-values (new-acc stop)
        (func next acc)) ; use define-values with the return of func
        (cond [stop (dfs-sigs-helper (rest todo) new-acc)]
              [else (define next-list (if (empty? (get-children run-or-state next)) ; empty?
                                      (rest todo)
                                      (append (get-children run-or-state next) (rest todo)))) ; append instead
              (dfs-sigs-helper next-list new-acc)])]))
    (dfs-sigs-helper sigs init-acc)) ; maybe take in initial accumulator as well for more flexibility

; Be robust to callers who pass quantifier-vars as either (var . domain) or as '(var domain).
(: second/safe (-> (U (Listof Any) (Pairof Any Any)) Any))
(define (second/safe list-or-pair)
  (cond [(list? list-or-pair) (second list-or-pair)]
        [else (cdr list-or-pair)]))
