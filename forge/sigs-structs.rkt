#lang racket/base

; Structures and helper functions for running Forge, along with some constants and
; configuration code (e.g., most options).

(require (except-in forge/lang/ast -> set)
         forge/lang/bounds
         forge/breaks
         (only-in forge/shared get-verbosity VERBOSITY_HIGH))
(require (prefix-in @ (only-in racket hash not +)) 
         (only-in racket nonnegative-integer? thunk curry)
         (prefix-in @ racket/set))
(require racket/contract
         racket/match
         racket/set
         racket/list)
(require (for-syntax racket/base racket/syntax syntax/srcloc syntax/parse))
(require (prefix-in tree: forge/utils/lazy-tree))
(require syntax/srcloc)
(require (prefix-in pardinus: (only-in forge/pardinus-cli/server/kks clear cmd)))
(require (prefix-in cvc5: (only-in forge/solver-specific/smtlib-shared smtlib-display)))

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Data Structures ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Results from solver

; For a non-temporal result, just take the first element of instances
(struct/contract Sat (
  [instances any/c] ; list of hashes            
  [stats any/c]     ; association list
  [metadata any/c]  ; association list
  ) #:transparent)

(struct/contract Unsat (               
  [core (or/c #f (listof any/c))]; list-of-Formula-string-or-formulaID)]
  [stats any/c] ; association list
  [kind symbol?] ; symbol
  ) #:transparent)

; For SMT backends only, may yield "unknown"
(struct/contract Unknown (
  [stats any/c]    ; data on performance, translation, etc. 
  [metadata any/c] ; any solver-specific data provided about the unknown result
  )#:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sigs and Relations enrich the "relation" AST node with
; Forge-specific information, which often leads to added
; constraints.

; DO NOT EXTEND THIS STRUCT
(struct Sig node/expr/relation (
  name ; symbol?
  one ; boolean?
  lone ; boolean?
  abstract ; boolean?
  extends ; (or/c Sig? #f)
  ) #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "(Sig ~a)" (Sig-name self)))])

; DO NOT EXTEND THIS STRUCT
; TODO: really this should be called "Field", since it represents that at the surface/core level.
(struct Relation node/expr/relation (
  name ; symbol?
  sigs-thunks ; (listof (-> Sig?))
  breaker ; (or/c node/breaking/break? #f)
  ) #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "(Relation ~a)" (Relation-name self)))])

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
(struct/contract Range (
  [lower (or/c nonnegative-integer? #f)]
  [upper (or/c nonnegative-integer? #f)]
  ) #:transparent)

; A Scope represents the numeric size limitations on sigs in a run.
; This includes the range of possible bitwidths, and a default range
; to use for sigs whose scope is undefined.
(struct/contract Scope (
  [default-scope (or/c Range? #f)]
  [bitwidth (or/c nonnegative-integer? #f)]
  [sig-scopes (hash/c symbol? Range?)]
  ) #:transparent)

; A PiecewiseBound represents an atom-indexed, incomplete partial bound. E.g., one might write:
;   `Alice.father in `Bob + `Charlie
;   `Bob.father in `Charlie + `David
; Note that a piecewise bound is not the same as a "partial" bound; a partial bound is complete,
; in the sense that only one bind declaration is possible for that relation.
(struct/contract PiecewiseBound (
  [tuples (listof any/c)]                  ; first element is the indexed atom in the original piecewise bounds
  [atoms (listof any/c)]                   ; which atoms have been bound? (distinguish "given none" from "none given")
  [operator (one-of/c '= 'in 'ni)]         ; which operator mode?
  ) #:transparent)
(define PiecewiseBounds/c (hash/c node/expr/relation? PiecewiseBound?))

; A Bound represents the set-based size limitations on sigs and relations in a run.
; Information from Scope(s) and Bounds(s) will be combined only once a run executes.
(struct/contract Bound (
  ; pbindings: partial (but complete) bindings for a given relation
  [pbindings (hash/c node/expr/relation? sbound?)]
  ; tbindings: total (and complete) bindings for a given relation; also known as an exact bound.
  [tbindings (hash/c node/expr/relation? any/c)]
  ; incomplete bindings for a given relation, indexed by first column
  [piecewise PiecewiseBounds/c]
  ; original AST nodes, for improving errors, indexed by relation
  [orig-nodes (hash/c node/expr/relation? (listof node?))]
  ) #:transparent)
                                
; An Inst function is an accumulator of bounds information. It doesn't (necessarily)
; contain the full information about a run's scope, bounds, etc. Rather, it allows for
; the aggregation of this info across multiple `inst` declarations.
(struct/contract Inst (
  [func (Scope? Bound? . -> . (values Scope? Bound?))]
  ) #:transparent)

; A Target describes the goal of a target-oriented model-finding run.
(struct/contract Target (
  [instance (hash/c symbol? (listof (listof symbol?)))]
  ; This is not the same as option target_mode, which provides a global default.
  ; Rather, this is per target.
  [distance (or/c 'close_noretarget 'far_noretarget 'close_retarget 'far_retarget 'hamming_cover)]
  ) #:transparent)

(struct/contract expression-type (
  [type (listof (listof symbol?))]
  [multiplicity (or/c 'set 'lone 'one 'no 'func 'pfunc)]
  [temporal-variance (or/c boolean? string?)]
  [top-level-types (listof (or/c 'Int 'univ))]
  ) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; If adding new option fields, remember to update all of:
;  -- DEFAULT_OPTIONS
;  -- symbol->proc
;  -- option-types
;  -- option-types-names
;  -- state-set-option (in sigs.rkt)
(struct/contract Options (
  [eval-language symbol?]
  [solver (or/c string? symbol?)]
  [backend symbol?]
  [sb nonnegative-integer?]
  [coregranularity nonnegative-integer?]
  [logtranslation nonnegative-integer?]
  [min_tracelength nonnegative-integer?]
  [max_tracelength nonnegative-integer?]
  [problem_type symbol?]
  [target_mode symbol?]
  [core_minimization symbol?]  
  [skolem_depth integer?] ; allow -1 (disable Skolemization entirely)
  [local_necessity symbol?]
  [run_sterling (or/c string? symbol?)]
  [sterling_port nonnegative-integer?]
  [engine_verbosity nonnegative-integer?]
  [test_keep symbol?]
  [no_overflow symbol?]
  [java_exe_location (or/c false/c string?)]
  ) #:transparent)

(struct/contract State (
  [sigs (hash/c symbol? Sig?)]
  [sig-order (listof symbol?)]
  [relations (hash/c symbol? Relation?)]
  [relation-order (listof symbol?)]
  [pred-map (hash/c symbol? (or/c (unconstrained-domain-> node/formula?)
                                  node/formula?))]
  [fun-map (hash/c symbol? (unconstrained-domain-> node?))]
  [const-map (hash/c symbol? node?)]
  [inst-map (hash/c symbol? Inst?)]
  [options Options?]
  [runmap (hash/c symbol? any/c)] ; TODO: any/c -> Run?
  ) #:transparent)

(struct/contract Run-spec (
  [state State?]  ; Model state at the point of this run 
  [preds (listof node/formula?)] ; predicates to run, conjoined
  [scope Scope?]  ; Numeric scope(s)
  [bounds Bound?] ; set-based upper and lower bounds
  [target (or/c Target? #f)] ; target-oriented model finding
  ) #:transparent)

(struct Server-ports (
  stdin
  stdout
  stderr
  shutdown
  is-running?) #:transparent)

(struct/contract Kodkod-current (
  [[formula #:mutable] nonnegative-integer?]
  [[expression #:mutable] nonnegative-integer?]
  [[int #:mutable] nonnegative-integer?]))

(struct/contract Run (
  [name symbol?]
  [command syntax?]
  [run-spec Run-spec?]
  ; This is the *start* of the exploration tree.
  [result tree:node?]
  [server-ports Server-ports?]
  [atoms (listof (or/c symbol? number?))]
  [kodkod-currents Kodkod-current?]
  [kodkod-bounds (listof any/c)]
  ; This is Sterling's current cursor into the exploration tree.
  ; It is mutated whenever Sterling asks for a new instance. We keep this
  ; separately, since there may be multiple cursors into the lazy tree if
  ; the run is also being processed in a script, but the programmatic cursor
  ; and the Sterling cursor should not interfere. 
  [last-sterling-instance (box/c (or/c Sat? Unsat? Unknown? false/c))]
  ) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;    Defaults     ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define DEFAULT-BITWIDTH 4)
(define DEFAULT-SIG-SCOPE (Range 0 4))
; an engine_verbosity of 1 logs SEVERE level in the Java engine;
;   this will send back info about crashes, but shouldn't spam (and possibly overfill) stderr.
(define DEFAULT-OPTIONS (Options 'surface 'SAT4J 'pardinus 20 0 0 1 5 'default
                                 'close-noretarget 'fast 0 'off 'on 0 1 'first 'false #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;    Constants    ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax Int (lambda (stx) (syntax-case stx ()
  [val (identifier? (syntax val)) (quasisyntax/loc stx (Sig (nodeinfo #,(build-source-location stx) 'checklangplaceholder #f) 1 "Int" (thunk '("Int")) "univ" #f 'Int #f #f #f #f))])))
(define-syntax succ (lambda (stx) (syntax-case stx ()
  [val (identifier? (syntax val)) (quasisyntax/loc stx (Relation (nodeinfo #,(build-source-location stx) 'checklangplaceholder #f) 2 "succ" (thunk '("Int" "Int")) "Int" #f 'succ (list (thunk Int) (thunk Int)) #f))])))

(define (max s-int)
  (sum (- s-int (join (^ succ) s-int))))
(define (min s-int)
  (sum (- s-int (join s-int (^ succ)))))

(define symbol->proc
  (hash 'eval-language Options-eval-language
        'solver Options-solver
        'backend Options-backend
        'sb Options-sb
        'coregranularity Options-coregranularity
        'logtranslation Options-logtranslation
        'min_tracelength Options-min_tracelength
        'max_tracelength Options-max_tracelength
        'problem_type Options-problem_type
        'target_mode Options-target_mode
        'core_minimization Options-core_minimization
        'skolem_depth Options-skolem_depth
        'local_necessity Options-local_necessity
        'run_sterling Options-run_sterling
        'sterling_port Options-sterling_port
        'engine_verbosity Options-engine_verbosity
        'test_keep Options-test_keep
        'no_overflow Options-no_overflow
        'java_exe_location Options-java_exe_location))

(define (oneof-pred lst)
  (lambda (x) (member x lst)))

(define option-types
  (hash 'eval-language symbol?
        'solver (lambda (x) (or (symbol? x) (string? x))) ; allow for custom solver path
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
        'run_sterling (lambda (x) (or (symbol? x) (string? x))) ; allow for custom visualizer path
        'sterling_port exact-nonnegative-integer?
        'engine_verbosity exact-nonnegative-integer?
        'test_keep (oneof-pred '(first last))
        'no_overflow (oneof-pred '(false true))
        'java_exe_location (lambda (x) (or (equal? x #f) (string? x)))))

(define option-types-names
  (hash 'eval-language "symbol"
        'solver "symbol or string"
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
        'run_sterling "symbol or string"
        'sterling_port "non-negative integer"
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
(define init-pred-map (@hash))
(define init-fun-map (@hash))
(define init-const-map (@hash))
(define init-inst-map (@hash))
(define init-runmap (@hash))
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

; get-run-spec :: Run-or-State -> Run-spec
(define (get-run-spec run-or-state)
    (cond [(Run? run-or-state)
         (Run-run-spec run-or-state)]
        [(Run-spec? run-or-state)
         run-or-state]))

; get-sig :: Run-or-State (|| Symbol Sig*) -> Sig
; Returns the Sig of a given name/ast-relation from a run/state.
(define (get-sig run-or-state sig-name-or-rel)
  (define sig-name
    (cond [(symbol? sig-name-or-rel) sig-name-or-rel]
          [(Sig? sig-name-or-rel)
           (Sig-name sig-name-or-rel)]
          [(node/expr/relation? sig-name-or-rel)
           (string->symbol (relation-name sig-name-or-rel))]
          [else (error (format "get-sig failed to locate: ~a" sig-name-or-rel))]))
  (hash-ref (State-sigs (get-state run-or-state)) sig-name))

; get-sigs :: Run-or-State, Relation*? -> List<Sig>
; If a relation is provided, returns the column sigs;
; otherwise, returns the Sigs of the given relation in a run/state.
(define (get-sigs run-or-state [relation #f])
  (define state (get-state run-or-state))
  (if relation
      (map (compose (curry get-sig state) (lambda (sig-thunk) (sig-thunk)))
           (Relation-sigs-thunks (get-relation state relation)))
      (map (curry hash-ref (State-sigs state))
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
; Note that this will return the procedure, not the macro (no stx loc capture)
(define (get-pred run-or-state name)
  (define state (get-state run-or-state))
  (hash-ref (State-pred-map state) name))

; get-fun :: Run-or-State, Symbol -> Function
; Gets a function by name from a given state
; Note that this will return the procedure, not the macro (no stx loc capture)
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
  (define sigs (get-sigs run-or-state))
  (define parent (get-sig run-or-state sig-or-rel))
  (filter (lambda (sig) (equal? (Sig-extends sig) parent)) sigs))

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
(define (get-relation-map run-or-spec)
  (for/hash ([rel (get-all-rels run-or-spec)])
    (values (relation-name rel) rel)))

; get-option :: Run-or-state Symbol -> Any
(define (get-option run-or-state option)
  (define state (get-state run-or-state))
  ((hash-ref symbol->proc option) (State-options state)))

; is-sat? :: Run -> boolean
; Checks if a given run result is 'sat
(define (is-sat? run)
  (define first-instance (tree:get-value (Run-result run)))
  (Sat? first-instance))

; is-unsat? :: Run -> boolean
; Checks if a given run result is 'unsat
(define (is-unsat? run)
  (define first-instance (tree:get-value (Run-result run)))
  (Unsat? first-instance))

; is-unknown? :: Run -> boolean
; Checks if a given run result is 'unknown. This kind of result won't be given
; by all kinds of solver backends, but some do produce it. 
(define (is-unknown? run)
  (define first-instance (tree:get-value (Run-result run)))
  (Unknown? first-instance))


; get-stdin :: Run -> input-port?
(define (get-stdin run)
  (assert-is-running run)
  (Server-ports-stdin (Run-server-ports run)))

; get-stdout :: Run -> output-port?
(define (get-stdout run)
  (assert-is-running run)
  (Server-ports-stdout (Run-server-ports run)))

; get-stderr :: Run -> output-port?
(define (get-stderr run)
  (assert-is-running run)
  (Server-ports-stderr (Run-server-ports run)))

;;;;;;;;;;;;;;;;;;;;;;;
; Per-run closed status

; Keep track of which runs have been closed via close-run
(define closed-run-names (box (list)))
; Allows other modules to let this layer know a run is closed; this box
; is referenced by the instance generator for each run.
(define (add-closed-run-name! name)
  (set-box! closed-run-names (cons name (unbox closed-run-names))))
(define (is-run-closed? name-or-run)
  (define (truthify x) (if x #t #f))
  (truthify
   (cond [(Run? name-or-run) 
          (member (Run-name name-or-run) (unbox closed-run-names))]
         [else
          (member name-or-run (unbox closed-run-names))])))

; close-run :: Run -> void
(define (close-run run)
  (assert-is-running run)
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
     (pardinus:cmd [(get-stdin run)] (pardinus:clear (Run-name run)))]
    ['smtlibtor
     (cvc5:smtlib-display (get-stdin run) "(reset)")]
    [else
     (raise-forge-error #:msg (format "Unsupported backend when closing solver run: ~a" backend)
                        #:context #f)]))

; is-running :: Run -> Boolean
; This reports whether the _solver server_ is running;
; *NOT* whether an individual run is still open.
(define (is-running? run)
  ((Server-ports-is-running? (Run-server-ports run))))

(define (assert-is-running run)
  (unless (is-running? run)
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

; for ifte, use struct type to decide whether this is a formula (sugar)
; or expression form (which has its own AST node). Avoid exponential
; blowup from chained IFTEs by expanding to a chain of function calls.
(define (ifte-disambiguator info a b c)
  (unless (node/formula? a)
    (raise-forge-error
     #:msg ("If-then-else needed a boolean-valued formula for its first argument; got ~a." (pretty-type-of a))
     #:context a))
  (cond
    ; It's a formula if-then-else
    [(and (node/formula? b) (node/formula? c))
     (&&/info info
              (=>/info info a b)
              (=>/info info (!/info info a) c))]
    ; It's an expression if-then-else (note: mixing int-expr and rel-expr is OK)
    [(and (or (node/expr? b) (node/int? b) (integer? b))
          (or (node/expr? c) (node/int? c) (integer? c)))
     (ite/info info a b c)]
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
(define (<:helper a b info)
  (domain-check<: a b (nodeinfo-loc info))
  (&/info info
            b 
            (->/info info a univ)))

(define-syntax (:> stx) 
  (syntax-case stx () 
    [(_ a b) 
      (quasisyntax/loc stx 
        (:>helper a b (nodeinfo #,(build-source-location stx) 'checklangNoCheck #f)))]
    [(_ (#:lang check-lang) a b) 
      (quasisyntax/loc stx 
        (:>helper a b (nodeinfo #,(build-source-location stx) check-lang #f)))]))

; TODO: this only functions for binary relations
(define (:>helper a b info)
  (domain-check:> a b (nodeinfo-loc info))
  (&/info info
            a 
            (->/info info univ b)))

(define (domain-check<: a b loc) 
  (unless (equal? (node/expr-arity b)
                  (@+ 1 (node/expr-arity a))) 
    (raise-forge-error
     #:msg (format "<: argument has incorrect arity (~a vs. ~a) in ~a <: ~a" 
                   (node/expr-arity a) (node/expr-arity b) (deparse a) (deparse b))
     #:context loc)))

(define (domain-check:> a b loc) 
  (unless (equal? (node/expr-arity a)
                  (@+ 1 (node/expr-arity b))) 
    (raise-forge-error
     #:msg (format ":> argument has incorrect arity (~a vs. ~a) in ~a :> ~a" 
                   (node/expr-arity a) (node/expr-arity b) (deparse a) (deparse b))
     #:context loc)))

; A Field relation is functional if it has a functional breaker assigned. 
(define (Relation-is-functional? r)
  (or (Relation-is? r '(pfunc func))))

(define (Relation-is? r sym-list)
  (and (Relation? r)
       (node/breaking/break? (Relation-breaker r))
       (member (node/breaking/break-break (Relation-breaker r)) sym-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; "Primification"-related utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Do not check integer literals with respect to bitwidth for these backends
(define UNBOUNDED_INT_BACKENDS '(smtlibtor))

; Turn signame into list of all primsigs it contains
; Note we use Alloy-style "_remainder" names here; these aren't necessarily embodied in Forge
(define/contract (primify run-or-state raw-signame)
  (-> (or/c Run? State? Run-spec?) (or/c symbol? string?) (listof symbol?))  
  (let ([signame (cond [(string? raw-signame) (string->symbol raw-signame)]
                       [(Sig? raw-signame) (Sig-name raw-signame)]
                       [else raw-signame])])
    (cond [(equal? 'Int signame)           
           '(Int)]
          [(equal? 'univ signame)
           (if (member (get-option (get-run-spec run-or-state) 'backend) UNBOUNDED_INT_BACKENDS)       
           (remove-duplicates (flatten (map (lambda (n) (primify run-or-state n)) (remove 'Int (map Sig-name (get-sigs run-or-state))))))  
           (remove-duplicates (flatten (map (lambda (n) (primify run-or-state n)) (cons 'Int (map Sig-name (get-sigs run-or-state)))))))]
          [else           
           (define the-sig (get-sig run-or-state signame))
           (define all-primitive-descendants
             (remove-duplicates
              (flatten
               (map (lambda (n) (primify run-or-state n))
                    (get-children run-or-state signame)))))
           (cond
             [(Sig-abstract the-sig)
              
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

(define (deprimify run-or-state primsigs)
  (let ([all-sigs (map Sig-name (get-sigs run-or-state))])
    (cond
      [(equal? primsigs '(Int))
       'Int]
      [(equal? primsigs (remove-duplicates (flatten (map (lambda (n) (primify run-or-state n)) (cons 'Int all-sigs)))))
       'univ]
      [else (define top-level (get-top-level-sigs run-or-state))
            (define pseudo-fold-lambda (lambda (sig acc) (if (or (subset? (primify run-or-state (Sig-name sig)) (flatten primsigs))
                                                                 (equal? (list (car (primify run-or-state (Sig-name sig)))) (flatten primsigs)))
                                                                 ; the above check is added for when you have the parent sig, but are expecting the child
                                      (values (append acc (list (Sig-name sig))) #t) ; replace cons with values
                                      (values acc #f))))
            (define final-list (dfs-sigs run-or-state pseudo-fold-lambda top-level '()))
            final-list])))

; Runs a DFS over the sigs tree, starting from sigs in <sigs>.
; On each visited sig, <func> is called to obtain a new accumulated value
; and whether the search should continue to that sig's children.
(define (dfs-sigs run-or-state func sigs init-acc)
    (define (dfs-sigs-helper todo acc)
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
(define (second/safe list-or-pair)
  (cond [(list? list-or-pair) (second list-or-pair)]
        [else (cdr list-or-pair)]))
