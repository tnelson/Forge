#lang racket

(require (prefix-in @ racket) 
         (prefix-in @ racket/set))
(require syntax/parse/define)
(require (for-syntax racket/match))

(require "shared.rkt")
(require "lang/ast.rkt"
         "lang/bounds.rkt"
         "breaks.rkt")
(require "server/eval-model.rkt")
(require "server/forgeserver.rkt" ; v long
         "kodkod-cli/server/kks.rkt" 
         "kodkod-cli/server/server.rkt"
         "kodkod-cli/server/server-common.rkt"
         "translate-to-kodkod-cli.rkt"
         "translate-from-kodkod-cli.rkt")

; Commands
(provide sig relation fun const pred run #|test|# #|check|# display inst with evaluate)

; Instance analysis functions
(provide is-sat? is-unsat?)

; AST values
; Expression
(provide Int iden univ none)
(provide ^ * ~ + - & join )

; Formula
(provide true false)
(provide -> => implies ! not and or && || ifte iff <=>)
(provide = in ni) 
(provide != !in !ni)
(provide no some one lone all set) ; two)

; Ints
(provide < > int= >= <=)
(provide add subtract multiply divide sign abs remainder)
(provide card sum sing succ max min sum-quant)
(provide node/int/constant)

; Racket stuff
(provide let)

; Technical stuff
(provide set-verbosity VERBOSITY_LOW VERBOSITY_HIGH)
(provide set-path!)
(define (set-path! path) #f)

; Data structures
(provide (prefix-out forge: (all-defined-out)))
(provide (prefix-out forge: (struct-out Sig))
         (prefix-out forge: (struct-out Relation))
         (prefix-out forge: (struct-out Range))
         (prefix-out forge: (struct-out Scope))
         (prefix-out forge: (struct-out Bound))
         (prefix-out forge: (struct-out Options))
         (prefix-out forge: (struct-out State))
         (prefix-out forge: (struct-out Run-spec))
         (prefix-out forge: (struct-out Run)))

(provide (prefix-out forge: curr-state)
         (prefix-out forge: update-state!))

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
(define init-relations (hash 'succ (Relation 'succ succ '(Int Int))))
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

; TODO: GET RID OF THIS
(define current-formula (make-parameter 0))
(define current-expression (make-parameter 0))
(define current-int-expression (make-parameter 0))

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

get-sig :: Run-or-State (|| Symbol AST-Relation) -> Sig
Returns the Sig of a given name/ast-relation from a run/state.

get-sigs :: Run-or-State, Relation*? -> List<Sig>
If a relation is provided, returns the column sigs;
otherwise, returns the Sigs of the given relation in a run/state.

get-top-level-sigs :: Run-or-State -> List<Sig>
Returns the Sigs in a run/state that do not extend another Sig.

get-fields :: Run-or-State Sig*) -> List<Relation>
Returns the relations whose first sig is the given sig.

get-relation :: Run-or-State, (|| Symbol AST-Relation) -> Relation
Returns the Relation of a given name/ast-relation from a run/state.

get-relations :: Run-or-State -> List<Relation>
Returns the Relations in a run/state.

get-children :: Run-or-State, Sig* -> List<Sig>
Returns the children Sigs of a Sig.

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

; get-children :: Run-or-State, Sig* -> List<Sig>
; Returns the children Sigs of a Sig.
(define (get-children run-or-state sig-or-rel)
  (define children-names (Sig-extenders (get-sig state sig-or-rel)))
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

; state-add-relation :: State, Symbol, List<Sig> -> State
; Adds a new relation to the given State.
(define (state-add-relation state name rel rel-sigs)
  (define new-relation (Relation name rel rel-sigs))
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


;; AST macros
(define-simple-macro (iff a b) (and (=> a b) (=> b a)))
(define-simple-macro (<=> a b) (and (=> a b) (=> b a)))
(define-simple-macro (ifte a b c) (and (=> a b) (=> (not a) c)))
(define-simple-macro (>= a b) (or (> a b) (int= a b)))
(define-simple-macro (<= a b) (or (< a b) (int= a b)))
(define-simple-macro (ni a b) (in b a))
(define-simple-macro (!= a b) (not (= a b)))
(define-simple-macro (!in a b) (not (in a b)))
(define-simple-macro (!ni a b) (not (ni a b)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Forge Commands  ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The environment threaded through commands
(define curr-state init-state)
(define (update-state! new-state) 
  (set! curr-state new-state))


; Declare a new sig.
; (sig name [|| [#:one] [#:abstract]] [#:extends parent])
(define-syntax (sig stx)
  (syntax-parse stx
    [(sig name:id (~alt (~optional (~seq #:extends parent:expr))
                        (~optional (~or (~seq (~and #:one one-kw))
                                        (~seq (~and #:abstract abstract-kw))))) ...)
    #'(begin
        (define true-name 'name)
        (define true-one (~? (~@ (or #t 'one-kw)) (~@ #f)))
        (define true-abstract (~? (~@ (or #t 'abstract-kw)) (~@ #f)))
        (define true-parent (~? (~@ 'parent) (~@ #f)))
        (define name (declare-relation (list (symbol->string true-name))
                                       (symbol->string (or true-parent 'univ))
                                       (symbol->string true-name)))
        (update-state! (state-add-sig curr-state true-name name true-one true-abstract true-parent)))]))

; Declare a new relation
; (relation name (sig sig sig ...))
(define-syntax (relation stx)
  (syntax-parse stx
    [(relation name:id (sig1:id sig2:id sigs ...))
     #'(begin
       (define true-name 'name)
       (define true-sigs (list 'sig1 'sig2 'sigs ...))
       (define name (declare-relation (map symbol->string true-sigs) (symbol->string 'sig1) (symbol->string true-name)))
       (update-state! (state-add-relation curr-state true-name name true-sigs)))]))

; Declare a new predicate
; (pred name cond ...)
; (pred (name var ...) cond ...)
(define-syntax (pred stx)
  (syntax-parse stx
    [(pred name:id conds:expr ...+) 
      #'(begin 
        (define name (&& conds ...))
        (update-state! (state-add-predicate curr-state 'name)))]
    [(pred (name:id args:id ...+) conds:expr ...+) 
      #'(begin 
        (define (name args ...) (&& conds ...))
        (update-state! (state-add-predicate curr-state 'name)))]))

; Declare a new function
; (fun (name var ...) result)
(define-syntax (fun stx)
  (syntax-parse stx
    [(fun (name:id args:id ...+) result:expr) 
      #'(begin 
        (define (name args ...) result)
        (update-state! (state-add-function curr-state 'name)))]))

; Declare a new constant
; (const name value)
(define-syntax (const stx)
  (syntax-parse stx
    [(const name:id value:expr) 
      #'(begin 
        (define name value)
        (update-state! (state-add-constant curr-state 'name)))]))

; Define a new bounding instance
; (inst name binding ...)
(define-syntax-rule (inst name binds ...)
  (define (name scope bound)
    (set!-values (scope bound) (bind scope bound binds)) ...
    (values scope bound)))

; Run a given spec
; (run name
;      [#:pred [(pred ...)]] 
;      [#:scope [((sig [lower 0] upper) ...)]]
;      [#:inst instance-name])
(define-syntax (run stx)
  (define command (format "~a" stx))

  (syntax-parse stx
    [(run name:id
          (~alt
            (~optional (~seq #:preds (pred ...)))
            (~optional (~seq #:scope ((sig:id (~optional lower:nat #:defaults ([lower #'0])) upper:nat) ...)))
            (~optional (~seq #:bounds (bound ...)))) ...)
      #`(begin
        (define run-name (~? (~@ 'name) (~@ 'no-name-provided)))
        (define run-state curr-state)
        (define run-preds (~? (~@ (list pred ...)) (~@ (list)))) 

        (define sig-scopes (~? 
          (~@
            (for/hash ([name (list 'sig ...)]
                       [lo (list lower ...)]
                       [hi (list upper ...)])
              (values name (Range lo hi))))
          (~@ (hash))))
        (define bitwidth (if (hash-has-key? sig-scopes 'Int)
                             (begin0 (Range-upper (hash-ref sig-scopes 'Int))
                                     (set! sig-scopes (hash-remove sig-scopes 'Int)))
                             #f))
        (define default-sig-scope (if (hash-has-key? sig-scopes 'default)
                                      (begin0 (hash-ref sig-scopes 'default)
                                              (set! sig-scopes (hash-remove sig-scopes 'default)))
                                      #f))
        (define base-scope (Scope default-sig-scope bitwidth sig-scopes))

        (define default-bound
          (Bound (hash) (hash)))
        (define (run-inst scope bounds)
          (for ([sigg (get-sigs run-state)])
            (when (Sig-one sigg)
              (set!-values (scope bounds) (bind scope bounds (one (Sig-rel sigg))))))
          (~? (~@ (set!-values (scope bounds) (bind scope bounds bound)) ...))
          (values scope bounds))
        (define-values (run-scope run-bound)
          (run-inst base-scope default-bound))

        (define run-command #,command)

        (define run-spec (Run-spec run-state run-preds run-scope run-bound))
        (define-values (run-result atom-rels) (send-to-kodkod run-spec))

        (define name (Run run-name run-command run-spec run-result atom-rels)))]))

; Test that a spec is sat or unsat
; (run name
;      [#:pred [(pred ...)]] 
;      [#:scope [((sig [lower 0] upper) ...)]]
;      [#:inst instance-name]
;      [|| sat unsat]))
(define-syntax (test stx)
  (define-syntax-class sat-or-unsat
    (pattern (~and (~or (~literal sat) (~literal unsat)) value)
      #:with val 'value))
  (syntax-parse stx
    [(test name:id 
           (~alt
            (~optional (~seq #:preds (pred:expr ...)))
            (~optional (~seq #:bounds ((sig:id (~optional lower:nat #:defaults ([lower #'0])) upper:nat) ...)))
            (~optional (~seq #:inst inst:expr))) ...
           sou:sat-or-unsat)
     #'(begin
       (run temp-run (~? (~@ #:preds (pred ...)) (~@)) (~? (~@ #:bounds ([sig lower upper] ...)) (~@)) (~? (~@ #:inst inst) (~@)))
       (define first-instance (stream-first (Run-result temp-run)))
       (when (@not (equal? (car first-instance) sou.val)) (raise "Failed test")))]))

; Exprimental: Run in the context of a given external Forge spec
; (with path-to-forge-spec commands ...)
(define-syntax (with stx)
  (syntax-parse stx
    [(with module-name exprs ...)
      #'(let ([temp-state curr-state])
          (local-require module-name)
          exprs ...
          (println "tests")
          (update-state! temp-state))]))

(define (evaluate run instance expression)
  (define-values (expr-name interpretter)
    (cond [(node/expr? expression) (begin0
           (values (e (current-expression))
                   interpret-expr)
           (current-expression (add1 (current-expression))))]
          [(node/formula? expression) (begin0
           (values (f (current-formula))
                   interpret-formula)
           (current-formula (add1 (current-formula))))]
          [(node/int? expression) (begin0
           (values (i (current-int-expression))
                   interpret-int)
           (current-int-expression (add1 (current-int-expression))))]))

  (define all-rels (get-all-rels run))

  (cmd 
    [(stdin)]
    (print-cmd-cont "(~a " expr-name)
    (interpretter expression all-rels '())
    (print-cmd ")")
    (print-cmd "(evaluate ~a)" expr-name)
    (print-eof))

  (define atom-rels (Run-atom-rels run))
  (translate-evaluation-from-kodkod-cli (read-evaluation (stdout)) atom-rels))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Result Functions ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; is-sat? :: Run -> boolean
; Checks if a given run result is 'sat
(define (is-sat? run)
  (define first-instance (stream-first (Run-result run)))
  (equal? (car first-instance) 'sat))

; is-unsat? :: Run -> boolean
; Checks if a given run result is 'unsat
(define (is-unsat? run)
  (define first-instance (stream-first (Run-result run)))
  (equal? (car first-instance) 'unsat))

; make-model-generator :: Stream<model> -> (-> model)
; Creates a thunk which generates a new model on each call.
(define (make-model-generator model-stream)
  (thunk
    (define ret (stream-first model-stream))
    (set! model-stream (stream-rest model-stream))
    ret))

; make-model-evaluator :: Run -> (String -> ???)
; Creates an evaluator function for a given Run. 
; Executes on the most recently generated instance.
(define (make-model-evaluator run)
  (lambda (command)
    (define name (substring command 1 3))
    (cmd [(stdin)] 
      (print-cmd command)
      (print-cmd "(evaluate ~a)" name)
      (print-eof))
    (define result (read (stdout)))
    result))
    ; (define u (read (open-input-string command)))
    ; (println u)
    ; u))

(provide nsa)
(define nsa (make-parameter #f))
; display :: Run -> void
; Lifted function which, when provided a Run,
; generates a Sterling instance for it.
(define (display arg1 [arg2 #f])
  (if (@not (Run? arg1))
      (if arg2 (@display arg1 arg2) (@display arg1))
      (let ()
        (define run arg1)
        (define model-stream (Run-result run))
        (define get-next-model (make-model-generator model-stream))
        (define (evaluate-str str-command)
          (define-values (in-pipe out-pipe) (make-pipe))

          ; Write string command to pipe
          (write-string str-command out-pipe)
          (close-output-port out-pipe)

          (with-handlers ([(lambda (x) #t) 
                           (lambda (exn) (exn-message exn))])
            ; Read command as syntax from pipe
            (define command-syntax (read-syntax 'eval-pipe in-pipe))
            (close-input-port in-pipe)

            ; Evaluate command
            (define ns (namespace-anchor->namespace (nsa)))
            (define command (eval command-syntax ns))
            (println command)
            (evaluate run '() command)))

        (display-model get-next-model evaluate-str
                       (Run-name run) 
                       (Run-command run) 
                       "/no-name.rkt" 
                       (get-bitwidth 
                         (Run-run-spec run)) 
                       empty
                       (Run-atom-rels run)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Scope/Bound Updaters ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; set-bitwidth :: Scope, int -> Scope
; Updates the bitwidth for the given Scope.
(define (set-bitwidth scope n)
  (struct-copy Scope scope
               [bitwidth n]))

; update-int-bound :: Scope, node/expr/relation, Range -> Scope
; Updates the scope (range) for a given sig in scope.
(define (update-int-bound scope rel given-scope)
  (define name (string->symbol (relation-name rel)))
  (define old-scope (get-scope scope name))

  (define lower (@max (Range-lower given-scope) (Range-lower old-scope)))
  (define upper (@min (Range-upper given-scope) (Range-upper old-scope)))
  (when (@< upper lower)
    (raise "Bound conflict."))

  (define new-scope (Range lower upper))
  (define new-sig-scopes (hash-set (Scope-sig-scopes scope) name new-scope))

  (struct-copy Scope scope
               [sig-scopes new-sig-scopes]))

; update-bindings :: Bound, node/expr/relation, List<Symbol>, List<Symbol>? -> Bound
; Updates the partial binding for a given sig or relation.
; If a binding already exists, takes the intersection.
; If this results in an exact bound, adds it to the total bounds.
(define (update-bindings bound rel lower [upper #f])
  (set! lower (list->set lower))
  (when upper (set! upper (list->set upper)))

  (define old-pbindings (Bound-pbindings bound))
  (define old-tbindings (Bound-tbindings bound))

  ; New bindings can only strengthen old ones
  (when (hash-has-key? old-pbindings rel)
    (let ([old (hash-ref old-pbindings rel)])
      (set! lower (set-union lower (sbound-lower old)))
      (set! upper (cond [(@and upper (sbound-upper old))
                         (set-intersect upper (sbound-upper old))]
                        [else (@or upper (sbound-upper old))]))))
  

  (unless (@or (@not upper) (subset? lower upper))
    (raise "Bound conflict."))

  (define new-pbindings
    (hash-set old-pbindings rel (sbound rel lower upper)))

  ; when exact bounds, put in bindings
  (define new-tbindings 
    (if (equal? lower upper) 
        (hash-set old-tbindings (string->symbol (relation-name rel)) 
                                (set->list lower))
        old-tbindings))

  (define new-bound (Bound new-pbindings new-tbindings))
  new-bound)

; update-bindings-at :: Bound, node/expr/relation, node/expr/relation, 
;                       List<Symbol>, List<Symbol>? 
;                         -> Bound
; To be implemented.
; Updates the partial binding for a given focused relation.
; Example use is (ni (join Thomas mentors) (+ Tim Shriram)).
; (define (update-bindings-at bound rel foc lower [upper #f])
;   scope)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  Bound Declarations  ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (bind scope bound bind-expression)
(define-syntax (bind stx)
  (match-define (list b scope bound binding) (syntax-e stx))
  (syntax-parse binding #:datum-literals (no one two lone <= = card is ~ join Int CompareOp QualName Const)
    
    ; Cardinality bindings
    [(no rel) #`(bind #,scope #,bound (= rel none))]
    [(one rel) #`(bind #,scope #,bound (= (card rel) 1))]
    [(two rel) #`(bind #,scope #,bound (= (card rel) 2))]
    [(lone rel) #`(bind #,scope #,bound (<= (card rel) 1))]

    [(= (card rel) n)
     #`(let* ([exact (caar (eval-exp 'n (Bound-tbindings #,bound) 8 #f))]
              [new-scope (update-int-bound #,scope rel (Range exact exact))])
         (values new-scope #,bound))]

    [(<= (card rel) upper)
     #`(let* ([upper-val (caar (eval-exp 'upper (Bound-tbindings #,bound) 8 #f))]
              [new-scope (update-int-bound #,scope rel (Range 0 upper-val))])
         (values new-scope #,bound))]

    [(<= lower (card rel) upper)
     #`(let* ([lower-val (caar (eval-exp 'lower (Bound-tbindings #,bound) 8 #f))]
              [upper-val (caar (eval-exp 'upper (Bound-tbindings #,bound) 8 #f))]
              [new-scope (update-int-bound #,scope rel (Range lower-val upper-val))])
         (values new-scope #,bound))]

    ; Strategies
    [(is (~ rel) strat)
     #`(begin
       (break rel (get-co 'strat))
       (values #,scope #,bound))]

    [(is rel strat)
     #`(begin
       (break rel 'strat)
       (values #,scope #,bound))]

    ; Other instances
    [f:id #`(f #,scope #,bound)]

    ; Particular bounds
    [(cmp rel expr)
     #`(let ([tups (eval-exp 'expr (Bound-tbindings #,bound) 8 #f)]) ; LOOK HERE
         (define new-scope #,scope)
           ; (if (@not (equal? (relation-arity rel) 1))
           ;     #,scope
           ;     (begin
           ;       ;; make sure all sub-sigs exactly defined
           ;       ; (for ([(sub sup) (in-hash extensions-store)] #:when (equal? sup rel))
           ;       ;   (unless (rel-is-exact sub)
           ;       ;           (error 'inst "sub-sig ~a must be exactly specified before super-sig ~a" 
           ;       ;                  (relation-name sub) (relation-name sup))))
           ;       (let ([exact (length tups)]) 
           ;         (update-int-bound #,scope rel (Range exact exact))))))

         (define new-bound (cond
           [(equal? 'cmp '=)  (update-bindings #,bound rel tups tups)]
           [(equal? 'cmp 'in) (update-bindings #,bound rel (@set) tups)]
           [(equal? 'cmp 'ni) (update-bindings #,bound rel tups)]))

         (values new-scope new-bound))]

    ; [(cmp (join foc rel) expr)
    ;  #`(let ([tups (eval-exp (alloy->kodkod 'expr) bindings 8 #f)])
    ;      (define new-bound (cond
    ;        [(equal? 'cmp '=)  (update-bindings-at #,bound rel 'foc tups tups)]
    ;        [(equal? 'cmp 'in) (update-bindings-at #,bound rel 'foc (@set) tups)]
    ;        [(equal? 'cmp 'ni) (update-bindings-at #,bound rel 'foc tups)]))
    ;      (values #,scope new-bound))]

    ; Bitwidth
    [(Int n:nat)
     #'(values (set-bitwidth #,scope n) #,bound)]

    [x (raise-syntax-error 'inst (format "Not allowed in bounds constraint") stx)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;    Run Logic    ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; send-to-kodkod :: Run-spec -> Stream<model>, List<Symbol>
; Given a Run-spec structure, processes the data and communicates it to KodKod-CLI;
; then produces a stream to produce instances generated by KodKod, 
; along with a list of all of the atom names for sig atoms.
(define (send-to-kodkod run-spec)
  ; Insert missing upper bounds of partial bindings
  (define pbindings (Bound-pbindings (Run-spec-bounds run-spec)))
  (define fixed-sigs
    (for/hash ([(rel pbinding) (in-hash pbindings)])
      (match-define (sbound rel lower upper) pbinding)
      (when (@and (@= (relation-arity rel) 1) (@not upper))
        (define sig (get-sig run-spec (string->symbol (relation-name rel))))
        (define sig-scope (get-scope run-spec sig))
        (define new-atoms (range (set-count lower) (Range-upper sig-scope)))
        (define new-names (map (lambda (n) (list (string->symbol (format "~a~a" (Sig-name sig) n)))) new-atoms))
        (define new-upper (set-union lower (list->set new-names)))
        (set! upper new-upper))
      (values rel (sbound rel lower upper))))
  (define fixed-relations
    (for/hash ([(rel pbinding) (in-hash fixed-sigs)])
      (match-define (sbound rel lower upper) pbinding)
      (when (@and (@> (relation-arity rel) 1) (@not upper))
        (define relation (get-relation run-spec (string->symbol (relation-name rel))))
        (define types (get-sigs run-spec relation))
        (define type-uppers (map (compose set->list
                                          sbound-upper
                                          (curry hash-ref fixed-sigs ))
                                 types))
        (define new-upper (list->set (apply cartesian-product type-uppers)))
        (set! upper new-upper))
      (values rel (sbound rel lower upper))))
  (set! pbindings fixed-relations)
  ; Send user defined partial bindings to breaks
  (map instance (hash-values pbindings))

  (printf "PBINDINGS ~a~n" pbindings)
  (printf "OG TBINDINGS ~a~n" (Bound-tbindings (Run-spec-bounds run-spec)))
  (define tbindings 
    (for/fold ([tbindings (Bound-tbindings (Run-spec-bounds run-spec))])
              ([(rel sb) (in-hash pbindings)])
      ; this nonsense is just for atom names
      (define name (string->symbol (relation-name rel)))
      (hash-set tbindings name (for/list ([tup (sbound-upper sb)]) (car tup)))))
  (printf "TBINDINGS ~a~n" tbindings)
  ; Get KodKod names, min sets, and max sets of Sigs and Relations
  (define-values (sig-to-bound all-atoms) ; Map<Symbol, bound>, List<Symbol>
    (get-sig-bounds run-spec tbindings))

  (define relation-to-bound ; Map<Symbol, bound>
    (get-relation-bounds run-spec sig-to-bound))

  ; Get new bounds and constraints from breaks
  (define-values (total-bounds break-preds)
    (let* ([sig-bounds (map (compose (curry hash-ref sig-to-bound )
                                     Sig-name)
                            (get-sigs run-spec))]
           [relation-bounds (map (compose (curry hash-ref relation-to-bound )
                                          Relation-name)
                                 (get-relations run-spec))]
           [total-bounds (append sig-bounds relation-bounds)]
           [sigs (get-sigs run-spec)]
           [sig-rels (map Sig-rel (filter (lambda (sig) (@not (equal? (Sig-name sig) 'Int))) sigs))]
           [upper-bounds (for/hash ([sig sigs]) 
                           (values (Sig-rel sig) 
                                   (map car (bound-upper (hash-ref sig-to-bound (Sig-name sig))))))]
           [relations-store (for/hash ([relation (get-relations run-spec)]
                                       #:unless (equal? (Relation-name relation) 'succ))
                              (values (Relation-rel relation) (map Sig-rel (get-sigs run-spec relation))))]
           [extensions-store (for/hash ([sig sigs]
                                        #:when (Sig-extends sig))
                               (values (Sig-rel sig) (Sig-rel (get-sig run-spec (Sig-extends sig)))))])
      (constrain-bounds total-bounds sig-rels upper-bounds relations-store extensions-store)))

  (define sigs-and-rels
    (append (State-sig-order (Run-spec-state run-spec))
            (State-relation-order (Run-spec-state run-spec))))
  (set! total-bounds (map (lambda (name) 
                            (findf (lambda (b) 
                                     (equal? name (string->symbol (relation-name (bound-relation b)))))
                                   total-bounds)) 
                          sigs-and-rels))


  #| Print to KodKod-CLI
    print configure
    declare univ size

    declare ints

    print Int sig (r0)
    print other sigs (r2 ... rm)

    print succ relation (r(m + 1))
    print other relations (r(m + 2) ... rn)

    print formula / assert formula (f0 ... fk)

    print solve
  |#

  ; Initializing our kodkod-cli process, and getting ports for communication with it
  (start-server)

  (define-syntax-rule (kk-print lines ...)
    (cmd 
      [(stdin)]
      lines ...))

  ; Print configure and declare univ size
  (define bitwidth (get-bitwidth run-spec))
  (kk-print
    (configure (format ":bitwidth ~a :solver ~a :max-solutions 1 :verbosity 7 :sb ~a :core-gran ~a :log-trans ~a"
                       bitwidth "SAT4J" 20 0 1))
    (declare-univ (length all-atoms)))

  ; Declare ints
  (define num-ints (expt 2 bitwidth))
  (kk-print
    (declare-ints (range (- (/ num-ints 2)) (/ num-ints 2)) ; ints
                  (range num-ints)))                        ; indexes

  ; to-tupleset :: List<List<int>>, int -> tupleset
  (define (to-tupleset arity eles)
    (if (empty? eles)
        (if (@= arity 1)
            'none
            (product 'none (to-tupleset (sub1 arity) eles)))
        (tupleset #:tuples eles)))

  (define (get-atoms sig-or-rel atom-names)
    (define arity 
      (if (Sig? sig-or-rel) 1 (length (Relation-sigs sig-or-rel))))
    (define atoms 
      (for/list ([tup atom-names])
        (for/list ([atom tup])
          (index-of all-atoms atom))))
    (define ret (to-tupleset arity atoms))
    ret)

  (for ([sig-or-rel (append (get-sigs run-spec) (get-relations run-spec))]
        [bound total-bounds]
        [index (in-naturals)])
    (kk-print
      (declare-rel
        (r index)
        (get-atoms sig-or-rel (bound-lower bound))
        (get-atoms sig-or-rel (bound-upper bound)))))

  (for ([atom all-atoms]
        [atom-int (in-naturals)]
        [index (in-naturals (length (get-all-rels run-spec)))])
    (define tup (to-tupleset 1 (list (list atom-int))))
    (kk-print
      (declare-rel (r index) tup tup)))
  

  ; Declare assertions
  (define all-rels (get-all-rels run-spec))

  ; Get and print predicates
  (define run-constraints 
    (append (Run-spec-preds run-spec)
            (get-sig-size-preds run-spec)
            (get-relation-preds run-spec)
            (get-extender-preds run-spec)
            break-preds))

  (for ([p run-constraints]
        [assertion-number (in-naturals)])
    (kk-print
      (print-cmd-cont "(f~a " assertion-number)
      (translate-to-kodkod-cli p all-rels '())
      (print-cmd ")")
      (assert (f assertion-number))
      (current-formula (add1 assertion-number))))

  (define atom-rels 
    (for/list ([atom-name all-atoms])
      (define atom-name-str 
        (if (symbol? atom-name)
            (symbol->string atom-name)
            (number->string atom-name)))
      (declare-relation (list atom-name-str) "univ" atom-name-str)))

  ; Print solve
  (define (get-next-model)
    (kk-print (solve))
    (match-define (cons restype inst) (translate-from-kodkod-cli 'run (read-solution (stdout)) (append all-rels atom-rels) all-atoms))
    (cons restype inst))

  (define (model-stream)
    (stream-cons (get-next-model) (model-stream)))

  (values (model-stream) atom-rels))


; get-sig-info :: Run-spec -> Map<Symbol, bound>, List<Symbol>
; Given a Run-spec, assigns names to each sig, assigns minimum and maximum 
; sets of atoms for each, and find the total number of atoms needed (including ints).
(define (get-sig-bounds run-spec tbindings)

  ; Map<Symbol, int>
  (define curr-atom-number (make-hash))
  ; Sig -> Symbol
  (define (get-next-name sig)
    (define atom-number (add1 (hash-ref curr-atom-number (Sig-name sig) -1)))
    (hash-set! curr-atom-number (Sig-name sig) atom-number)
    (define default-name (string->symbol (format "~a~a" (Sig-name sig) atom-number)))
    (if (hash-has-key? tbindings (Sig-name sig))
        (let ([bind-names (hash-ref tbindings (Sig-name sig))])
          (if (@< atom-number (length bind-names))
              (list-ref bind-names atom-number)
              default-name))
        default-name))
  ; Sig, int -> List<Symbol>
  (define (get-next-names sig num)
    (for/list ([_ (range num)]) (get-next-name sig)))

  ; Map<Symbol, List<Symbol>
  (define sig-to-lower (make-hash))
  ; Sig -> List<Symbol>
  (define (fill-lower sig)
    (define own-lower-int (Range-lower (get-scope run-spec sig)))
    (define children-lower (apply append (map fill-lower (get-children run-spec sig))))

    (define own-lower
      (let ([difference (@- own-lower-int (length children-lower))])
        (if (@> difference 0)
            (append (get-next-names sig difference) children-lower)
            children-lower)))

    (hash-set! sig-to-lower (Sig-name sig) own-lower)
    own-lower)

  ; Map<Symbol, List<List<Symbol>>
  (define sig-to-upper (make-hash))
  ; Sig, List<Symbol>? -> List<Symbol>
  (define (fill-upper sig [parent-names #f])
    (define own-upper-int (Range-upper (get-scope run-spec sig)))
    (define own-lower (hash-ref sig-to-lower (Sig-name sig)))
    (define difference (@- own-upper-int (length own-lower)))
    (when (@< difference 0)
      (raise "Illegal bounds"))

    (define new-names 
      (if parent-names
          (take parent-names (@min difference (length parent-names)))
          (get-next-names sig difference)))
    (define own-upper (append own-lower new-names))
    (hash-set! sig-to-upper (Sig-name sig) own-upper)

    (for ([child (get-children run-spec sig)]) (fill-upper child new-names))
    own-upper)

  (define int-atoms
    (let* ([bitwidth (get-bitwidth run-spec)]
           [max-int (expt 2 (sub1 bitwidth))])
      (range (- max-int) max-int)))
  (hash-set! sig-to-lower 'Int int-atoms)
  (hash-set! sig-to-upper 'Int int-atoms)

  (define top-level-sigs (get-top-level-sigs run-spec))
  (define sig-atoms (apply append
    (for/list ([sig top-level-sigs] 
               #:unless (equal? (Sig-name sig) 'Int))
      (fill-lower sig)
      (fill-upper sig))))

  (define all-atoms (append int-atoms sig-atoms))

  ; Map<Symbol, bound>
  (define bounds-hash
    (for/hash ([sig (get-sigs run-spec)])
      (let* ([name (Sig-name sig)]
             [rel (Sig-rel sig)]
             [lower (map list (hash-ref sig-to-lower name))]
             [upper (map list (hash-ref sig-to-upper name))])
        (values name (bound rel lower upper)))))

  (values bounds-hash all-atoms))

; get-relation-info :: Run-spec -> Map<Symbol, bound>
; Given a Run-spec, the atoms assigned to each sig, the atoms assigned to each name,
; and the starting relation name, assigns names to each relation
; and minimum and maximum sets of atoms for each relation.
(define (get-relation-bounds run-spec sig-to-bound)
  (define without-succ
    (for/hash ([relation (get-relations run-spec)]
               #:unless (equal? (Relation-name relation) 'succ))
      (define sigs (get-sigs run-spec relation))
      (define sig-atoms (map (compose (curry map car )
                                      bound-upper
                                      (curry hash-ref sig-to-bound )
                                      Sig-name) 
                             sigs))
      (define upper (apply cartesian-product sig-atoms))
      (define lower empty)
      (values (Relation-name relation) 
              (bound (Relation-rel relation) lower upper))))
  (define ints (map car (bound-upper (hash-ref sig-to-bound 'Int))))
  (define succ-tuples (map list (reverse (rest (reverse ints))) (rest ints)))
  (hash-set without-succ 'succ (bound succ succ-tuples succ-tuples)))

; get-sig-size-preds :: Run-spec -> List<node/formula>
; Creates assertions for each Sig to restrict
; it to the correct lower/upper bound.
(define (get-sig-size-preds run-spec) 
  (define max-int (expt 2 (sub1 (get-bitwidth run-spec))))
  (for/list ([sig (get-sigs run-spec)]
             #:unless (equal? (Sig-name sig) 'Int))
    (match-define (Range lower upper) (get-scope run-spec sig))
    (unless (@< upper max-int)
      (raise (format (string-append "Upper bound too large for given BitWidth; "
                                    "Sig: ~a, Upper-bound: ~a, Max-int: ~a")
                     sig upper (sub1 max-int))))
    (and (<= (node/int/constant lower) (card (Sig-rel sig)))
         (<= (card (Sig-rel sig)) (node/int/constant upper)))))

; get-extender-preds :: Run-spec -> List<node/formula>
; Creates assertions for each Sig which has extending Sigs so that:
; - if it is abstract, then it must equal the sum of its extenders
; -                    else it must contain the sum of its extenders
; - all extenders are pair-wise disjoint.
(define (get-extender-preds run-spec)
  (define sig-constraints (for/list ([sig (get-sigs run-spec)])
    ; get children information
    (define children-rels (map Sig-rel (get-children run-spec sig)))

    ; abstract and sig1, ... extend => (= sig (+ sig1 ...))
    ; not abstract and sig is parent of sig1 => (in sig1 sig)
    ; TODO: optimize by identifying abstract sigs as sum of children
    (define (abstract sig extenders)
      (if (@= (length extenders) 1)
          (= sig (car extenders))
          (= sig (apply + extenders))))
    (define (parent sig1 sig2)
      (in sig2 sig1))
    (define extends-constraints 
      (if (and (Sig-abstract sig) (cons? (Sig-extenders sig)))
          (list (abstract (Sig-rel sig) children-rels))
          (map (curry parent (Sig-rel sig)) children-rels)))

    ; sig1 and sig2 extend sig => (no (& sig1 sig2))
    (define (disjoin-pair sig1 sig2)
      (no (& sig1 sig2)))
    (define (disjoin-list a-sig a-list)
      (map (curry disjoin-pair a-sig) a-list))
    (define (disjoin a-list)
      (if (empty? a-list)
          empty
          (append (disjoin-list (first a-list) (rest a-list))
                  (disjoin (rest a-list)))))
    (define disjoint-constraints (disjoin children-rels))

    (append extends-constraints disjoint-constraints)))

  ; combine all constraints together
  (apply append sig-constraints))

; get-relation-preds :: Run-spec -> List<node/formula>
; Creates assertions for each Relation to ensure that it does not
; contain any atoms which don't populate their Sig.
(define (get-relation-preds run-spec)
  (for/list ([relation (get-relations run-spec)])
    (define sig-rels (map Sig-rel (get-sigs run-spec relation)))
    (in (Relation-rel relation) (apply -> sig-rels))))


