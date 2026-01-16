#lang typed/racket/base/optional

;; Typed adapter for forge/sigs-structs
;; Provides type definitions for structs from the untyped sigs-structs module

(require forge/types/ast-adapter)
(require forge/types/lazy-tree-adapter)
(require forge/shared) ; provides FAtom and Tuple
(require forge/breaks) ; provides sbound

(define-type PiecewiseBounds (HashTable node/expr/relation PiecewiseBound))

(require/typed forge/sigs-structs
  [#:struct Sat (
    [instances : Any] ; list of hashes
    [stats : Any]     ; association list
    [metadata : Any])]  ; association list)
  [#:struct Unsat (
    [core : (U False (Listof Any))] ; list-of-Formula-string-or-formulaID
    [stats : Any] ; association list
    [kind : Symbol] ; symbol
  )]
  [#:struct Unknown (
    [stats : Any]    ; data on performance, translation, etc.
    [metadata : Any] ; any solver-specific data provided about the unknown result
  )]
  [#:struct Kodkod-current (
    [formula : Integer]
    [expression : Integer]
    [int : Integer])]
  [#:struct (Relation node/expr/relation) (
    [name : Symbol] ; symbol?
    [sigs-thunks : (Listof (-> Sig))]
    [breaker : (U node/breaking/break False)]
  )]
  [#:struct Server-ports (
    [stdin : Output-Port]
    [stdout : Input-Port]
    [stderr : Input-Port]
    [shutdown : (-> Void)]
    [is-running? : (-> Boolean)])]
  [#:struct (Sig node/expr/relation) (
    [name : Symbol] ; symbol?
    [one : Boolean] ; boolean?
    [lone : Boolean] ; boolean?
    [abstract : Boolean] ; boolean?
    [extends : (U Sig False)] ; (or/c Sig? #f)
  )]
  [#:struct Run-spec (
    [state : State]  ; Model state at the point of this run
    [preds : (Listof node/formula)] ; predicates to run, conjoined
    [scope : Scope] ; Numeric scope(s)
    [bounds : Bound] ; set-based upper and lower bounds
    [target : Any] ;(or/c Target? #f) ; target-oriented model finding
  )]
  [#:struct Bound (
    ; pbindings: partial (but complete) bindings for a given relation
    [pbindings : (HashTable node/expr/relation sbound)]
    ; tbindings: total (and complete) bindings for a given relation; also known as an exact bound.
    [tbindings : (HashTable node/expr/relation Any)]
    ; incomplete bindings for a given relation, indexed by first column
    [piecewise : PiecewiseBounds]
    ; original AST nodes, for improving errors, indexed by relation
    [orig-nodes : (HashTable node/expr/relation (Listof node))]
  )]
  [#:struct PiecewiseBound (
    [tuples : (Listof Tuple)]                  ; first element is the indexed atom in the original piecewise bounds
    [atoms : (Listof FAtom)]                   ; which atoms have been bound? (distinguish "given none" from "none given")
    [operator : (U '= 'in 'ni)])]               ; which operator mode?
  [#:struct State (
    [sigs : (HashTable Symbol Sig)]
    [sig-order : (Listof Symbol)]
    [relations : (HashTable Symbol Relation)]
    [relation-order : (Listof Symbol)]
    [pred-map : (HashTable Symbol node/formula)] ;(hash/c symbol? (or/c (unconstrained-domain-> node/formula?) node/formula?))
    [fun-map : (HashTable Symbol node)] ; (hash/c symbol? (unconstrained-domain-> node?))
    [const-map : (HashTable Symbol node)]
    [inst-map : (HashTable Symbol Any)] ; (hash/c symbol? Inst?)
    [options : Any] ; Options?
    [runmap : (HashTable Symbol Run)])]
  [#:struct Run (
    [name : Symbol]
    [command : Syntax]
    [run-spec : Run-spec]
    [result : Any] ;tree:node
    [server-ports : Any] ;Server-ports?]
    [atoms : (Listof FAtom)]
    [kodkod-currents : Any] ; Kodkod-current?]
    [kodkod-bounds : (Listof Any)]
    [last-sterling-instance : Any ])] ; (box/c (or/c Sat? Unsat? Unknown? false/c))
  [#:struct Range (
    [lower : (U Integer False)]
    [upper : (U Integer False)])]
  [#:struct Scope (
    [default-scope : (U Range False)]
    [bitwidth : (U Integer False)]
    [sig-scopes : (HashTable Symbol Range)])]
  [get-relations (-> (U Run State Run-spec) (Listof Relation))]
  [get-sigs (->* ((U Run State Run-spec)) ((U False node/expr/relation)) (Listof Sig))]
  [get-sig (-> (U Run State Run-spec) (U Symbol node/expr/relation) (U Sig False))]
  [get-option (case->
    (-> (U Run State Run-spec) 'backend Symbol)
    (-> (U Run State Run-spec) 'solver (U String Symbol))
    (-> (U Run State Run-spec) 'java_exe_location (U False Path-String))
    (-> (U Run State Run-spec) 'problem_type Symbol)
    (-> (U Run State Run-spec) Symbol Any))]
  [get-state (-> (U Run Run-spec State) State)]
  [get-bitwidth (-> (U Run-spec Scope) Integer)]
  [get-children (-> (U Run State Run-spec) Sig (Listof Sig))]
  [DEFAULT-SIG-SCOPE Range]
  [get-top-level-sigs (-> (U Run State Run-spec) (Listof Sig))]
  ;; TODO TYPES: these are macros, but they has no parameters, so they are being immediately
  ;; expanded here to the relations they denote.
  [Int Sig]
  [succ Relation]
)

(provide (all-defined-out))
