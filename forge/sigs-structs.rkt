#lang racket

(require (except-in "lang/ast.rkt" ->)
         "lang/bounds.rkt"
         "breaks.rkt")
(require forge/lang/deparser)
(require (prefix-in @ racket) 
         (prefix-in @ racket/set))
(require racket/contract)
(require (for-syntax racket/syntax syntax/srcloc))
(require (prefix-in tree: "lazy-tree.rkt"))
(require syntax/srcloc)

(provide (all-defined-out))
; (provide (except-out (all-defined-out) (struct-out Sig) (struct-out Relation)))
; (provide (contract-out (struct Sig ([info nodeinfo?]
                                    
;                                     [arity nonnegative-integer?]
;                                     [name string?]
;                                     [typelist (listof string?)]
;                                     [parent string?]
;                                     [is-variable boolean?]

;                                     [name symbol?]
;                                     [one boolean?]
;                                     [abstract boolean?]
;                                     [extends (or/c Sig? #f)]))))
; (provide (contract-out (struct Relation ([info nodeinfo?]
                                    
;                                          [arity nonnegative-integer?]
;                                          [name string?]
;                                          [typelist (listof string?)]
;                                          [parent string?]
;                                          [is-variable boolean?]
                                    
;                                          [name symbol?]
;                                          [sigs (listof Sig?)]
;                                          [breaker (or/c node/breaking/break? #f)]))))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(struct Sig node/expr/relation (
  name ; symbol?
  one ; boolean?
  abstract ; boolean?
  extends ; (or/c Sig? #f)
  ) #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "(Sig ~a)" (Sig-name self)))])

(struct Relation node/expr/relation (
  name ; symbol?
  sigs-thunks ; (listof (-> Sig?))
  breaker ; (or/c node/breaking/break? #f)
  ) #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "(Relation ~a)" (Relation-name self)))])

(struct/contract Range (
  [lower (or/c nonnegative-integer? #f)]
  [upper (or/c nonnegative-integer? #f)]
  ) #:transparent)

(struct/contract Scope (
  [default-scope (or/c Range? #f)]
  [bitwidth (or/c nonnegative-integer? #f)]
  [sig-scopes (hash/c symbol? Range?)]
  ) #:transparent)

(struct/contract Bound (
  [pbindings (hash/c node/expr/relation? sbound?)]
  [tbindings (hash/c node/expr/relation? any/c)] ; (hash/c symbol? (listof symbol?))] ; TODO
  ) #:transparent)

(struct/contract Inst (
  [func (Scope? Bound? . -> . (values Scope? Bound?))]
  ) #:transparent)

(struct/contract Target (
  [instance (hash/c symbol? (listof (listof symbol?)))]
  [distance (or/c 'close 'far)]
  ) #:transparent)

; If adding new option fields, remember to update all of:
;  --default value -- state-set-option and -- get-option
(struct/contract Options (
  [eval-language symbol?]
  [solver symbol?]
  [backend symbol?]
  [sb nonnegative-integer?]
  [coregranularity nonnegative-integer?]
  [logtranslation nonnegative-integer?]
  [min_tracelength nonnegative-integer?]
  [max_tracelength nonnegative-integer?]
  [problem_type symbol?]
  [target_mode symbol?]
  [core_minimization symbol?]
  [skolem_depth nonnegative-integer?]
  [local_necessity symbol?]
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
  [state State?]
  [preds (listof node/formula?)]
  [scope Scope?]
  [bounds Bound?]
  [target (or/c Target? #f)]
  ) #:transparent)

(struct Server-ports (
  stdin
  stdout
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
  [result tree:node?] ; TODO: specify
  [server-ports Server-ports?]
  [atoms (listof (or/c symbol? number?))]
  [kodkod-currents Kodkod-current?]
  [kodkod-bounds (listof any/c)] ; TODO: specify
  ) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;    Defaults     ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define DEFAULT-BITWIDTH 4)
(define DEFAULT-SIG-SCOPE (Range 0 4))
(define DEFAULT-OPTIONS (Options 'surface 'SAT4J 'pardinus 20 0 0 1 5 'default 'close-noretarget 'fast 0 'off))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;    Constants    ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax Int (lambda (stx) (syntax-case stx ()
  [val (identifier? (syntax val)) (quasisyntax/loc stx (Sig (nodeinfo #,(build-source-location stx) 'checklangplaceholder) 1 "Int" (thunk '("Int")) "univ" #f 'Int #f #f #f))])))
(define-syntax succ (lambda (stx) (syntax-case stx ()
  [val (identifier? (syntax val)) (quasisyntax/loc stx (Relation (nodeinfo #,(build-source-location stx) 'checklangplaceholder) 2 "succ" (thunk '("Int" "Int")) "Int" #f 'succ (list (thunk Int) (thunk Int)) #f))])))

(define (max s-int)
  (sum (- s-int (join (^ succ) s-int))))
(define (min s-int)
  (sum (- s-int (join s-int (^ succ)))))
; (define-syntax Int (lambda (stx) (syntax-case stx ()    
;     [val (identifier? (syntax val)) (quasisyntax/loc stx 
;       (Sig 'Int 
;             (node/expr/relation (nodeinfo #,(build-source-location stx)) 1 "Int" '(Int) "univ" #f)
;             #f #f #f))])))

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
          'local_necessity Options-local_necessity))
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



; get-stdin :: Run -> input-port?
(define (get-stdin run)
  (assert-is-running run)
  (Server-ports-stdin (Run-server-ports run)))

; get-stdin :: Run -> output-port?
(define (get-stdout run)
  (assert-is-running run)
  (Server-ports-stdout (Run-server-ports run)))

; close-run :: Run -> void
(define (close-run run)
  (assert-is-running run)
  ((Server-ports-shutdown (Run-server-ports run))))

; is-running :: Run -> Boolean
(define (is-running? run)
  ((Server-ports-is-running? (Run-server-ports run))))

(define (assert-is-running run)
  (unless (is-running? run)
    (raise "KodKod server is not running.")))

(require (for-syntax syntax/srcloc)) ; for these macros

;; Added sugar over the AST
;; It is vital to PRESERVE SOURCE LOCATION in these, or else errors and highlighting may focus on the macro definition point
(provide implies iff <=> ifte >= <= ni != !in !ni <: :>)

(define-syntax (implies stx) 
  (syntax-case stx () 
    [(_ check-lang a b) 
      (quasisyntax/loc stx  (=>/info (nodeinfo #,(build-source-location stx) check-lang) a b))]
    [(_ a b) 
      (quasisyntax/loc stx  (=>/info (nodeinfo #,(build-source-location stx)'checklangplaceholder) a b))]))

(define-syntax (iff stx) 
  (syntax-case stx () 
    [(_ check-lang a b) 
      (quasisyntax/loc stx 
        (&&/info (nodeinfo #,(build-source-location stx) check-lang)
                 (=>/info (nodeinfo #,(build-source-location stx) check-lang) a b)
                 (=>/info (nodeinfo #,(build-source-location stx) check-lang) b a)))]
    [(_ a b) 
      (quasisyntax/loc stx 
        (&&/info (nodeinfo #,(build-source-location stx) 'checklangplaceholder)
                 (=>/info (nodeinfo #,(build-source-location stx) 'checklangplaceholder) a b)
                 (=>/info (nodeinfo #,(build-source-location stx) 'checklangplaceholder) b a)))]))
(define-syntax (<=> stx) (syntax-case stx () [(_ a b) (quasisyntax/loc stx (&&/info (nodeinfo #,(build-source-location stx) 'checklangplaceholder)
                                                                (=>/info (nodeinfo #,(build-source-location stx) 'checklangplaceholder) a b)
                                                                (=>/info (nodeinfo #,(build-source-location stx) 'checklangplaceholder) b a)))]))

; for ifte, use struct type to decide whether this is a formula (sugar)
; or expression form (which has its own AST node). Avoid exponential
; blowup from chained IFTEs by expanding to a chain of function calls.
(define (ifte-disambiguator info a b c)
  (if (node/formula? b)
      (&&/info info
               (=>/info info a b)
               (=>/info info (! a) c))
      (ite/info info a b c)))
(define-syntax (ifte stx)
  (syntax-case stx ()
    [(_ a b c) (quasisyntax/loc stx
                 (ifte-disambiguator (nodeinfo #,(build-source-location stx) 'checklangplaceholder) a b c))]))

(define-syntax (ni stx) (syntax-case stx () [(_ a b) (quasisyntax/loc stx (in/info (nodeinfo #,(build-source-location stx) 'checklangplaceholder) b a))]))
(define-syntax (!= stx) (syntax-case stx () [(_ a b) (quasisyntax/loc stx (!/info (nodeinfo #,(build-source-location stx) 'checklangplaceholder)
                                                             (=/info (nodeinfo #,(build-source-location stx) 'checklangplaceholder) a b)))]
                                            [(_ (check-lang) a b) (quasisyntax/loc stx (!/info (nodeinfo #,(build-source-location stx) check-lang)
                                                             (=/info (nodeinfo #,(build-source-location stx) '(check-lang)) a b)))]))
(define-syntax (!in stx) (syntax-case stx () [(_ a b) (quasisyntax/loc stx  (!/info (nodeinfo #,(build-source-location stx) 'checklangplaceholder)
                                                              (in/info (nodeinfo #,(build-source-location stx) 'checklangplaceholder) a b)))]))
(define-syntax (!ni stx) (syntax-case stx () [(_ a b) (quasisyntax/loc stx (!/info (nodeinfo #,(build-source-location stx) 'checklangplaceholder)
                                                              (in/info (nodeinfo #,(build-source-location stx) 'checklangplaceholder) b a)))]))
(define-syntax (>= stx) (syntax-case stx () [(_ a b) (quasisyntax/loc stx (||/info (nodeinfo #,(build-source-location stx) 'checklangplaceholder)
                                                              (int>/info (nodeinfo #,(build-source-location stx) 'checklangplaceholder) a b)
                                                              (int=/info (nodeinfo #,(build-source-location stx) 'checklangplaceholder) a b)))]
                                            [(_ (check-lang) a b) (quasisyntax/loc stx (||/info (nodeinfo #,(build-source-location stx) check-lang)
                                                              (int>/info (nodeinfo #,(build-source-location stx) '(check-lang)) a b)
                                                              (int=/info (nodeinfo #,(build-source-location stx) '(check-lang)) a b)))]))
(define-syntax (<= stx) (syntax-case stx () [(_ a b) (quasisyntax/loc stx (||/info (nodeinfo #,(build-source-location stx) 'checklangplaceholder)
                                                              (int</info (nodeinfo #,(build-source-location stx) 'checklangplaceholder) a b)
                                                              (int=/info (nodeinfo #,(build-source-location stx) 'checklangplaceholder) a b)))]
                                            [(_ (check-lang) a b) (quasisyntax/loc stx (||/info (nodeinfo #,(build-source-location stx) check-lang)
                                                              (int</info (nodeinfo #,(build-source-location stx) '(check-lang)) a b)
                                                              (int=/info (nodeinfo #,(build-source-location stx) '(check-lang)) a b)))]))

(define-syntax (<: stx) 
  (syntax-case stx () 
    [(_ a b) 
      (quasisyntax/loc stx 
        (<:helper a b (nodeinfo #,(build-source-location stx) 'checklangplaceholder)))]))

(define (<:helper a b info)
  (domain-check<: a b (nodeinfo-loc info))
  (&/info info
            b 
            (->/info info a univ)))

(define-syntax (:> stx) 
  (syntax-case stx () 
    [(_ a b) 
      (quasisyntax/loc stx 
        (:>helper a b (nodeinfo #,(build-source-location stx) 'checklangplaceholder)))]))

(define (:>helper a b info)
  (domain-check:> a b (nodeinfo-loc info))
  (&/info info
            a 
            (->/info info univ b)))

(define (domain-check<: a b loc) 
  (let ([src-line (source-location-line loc)]
        [src-col (source-location-column loc)]
        [src-span (source-location-span loc)])
    (unless (equal? (node/expr-arity b)
                    (+ 1 (node/expr-arity a))) 
                    (raise-user-error (format "<: argument has incorrect arity in ~a <: ~a on line ~a, column ~a, span~a" (deparse a) (deparse b) src-line src-col src-span)))))

(define (domain-check:> a b loc) 
  (let ([src-line (source-location-line loc)]
        [src-col (source-location-column loc)]
        [src-span (source-location-span loc)])
    (unless (equal? (node/expr-arity a)
                    (+ 1 (node/expr-arity b))) 
                    (raise-user-error (format ">: argument has incorrect arity in ~a >: ~a on line ~a, column ~a, span~a" (deparse a) (deparse b) src-line src-col src-span)))))
