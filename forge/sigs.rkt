#lang racket/base

(require (only-in racket/function thunk)
         (only-in racket/list first rest empty empty? flatten remove-duplicates last)
         (only-in racket/pretty pretty-print)
         (prefix-in @ (only-in racket/base display max min - +)) 
         (prefix-in @ racket/set)
         (prefix-in @ (only-in racket/contract ->))
         (only-in racket/contract define/contract))
(require syntax/parse/define
         syntax/srcloc)
(require (for-syntax racket/base racket/syntax syntax/srcloc syntax/strip-context
                     (only-in racket/pretty pretty-print)))

(require forge/shared)
(require forge/lang/ast 
         forge/lang/bounds 
         forge/breaks)
(require (only-in forge/lang/reader [read-syntax read-surface-syntax]))
(require forge/server/eval-model)
(require forge/server/forgeserver)
(require forge/translate-to-kodkod-cli
         forge/translate-from-kodkod-cli
         forge/sigs-structs
         forge/evaluator
         (prefix-in tree: forge/lazy-tree)
         forge/send-to-solver)
(require (only-in forge/lang/alloy-syntax/parser [parse forge-lang:parse])
         (only-in forge/lang/alloy-syntax/tokenizer [make-tokenizer forge-lang:make-tokenizer]))
(require (only-in forge/sigs-functional
                  make-sig
                  make-relation
                  make-inst
                  run-from-state
                  ; the next ones are not used in this file
                  ; but are required so that they can be provided,
                  ; allowing users to use them in forge/core programs
                  make-run
                  check-from-state
                  make-check
                  test-from-state
                  make-test))
(require forge/choose-lang-specific)

; Commands
(provide sig relation fun const pred inst with)
(provide run check test example display execute)
(provide instance-diff solution-diff evaluate)

; Instance analysis functions
(provide is-unsat? is-sat? is-unknown?)

; export AST macros and struct definitions (for matching)
; Make sure that nothing is double-provided
(provide (all-from-out forge/lang/ast))

; Racket stuff
(provide let quote)

; Technical stuff
(provide set-verbosity VERBOSITY_LOW VERBOSITY_HIGH)
(provide set-path!)
(provide set-option!)
(define (set-path! path) #f)

; Data structures
(provide (prefix-out forge: (struct-out Sig))
         (prefix-out forge: (struct-out Relation))
         (prefix-out forge: (struct-out Range))
         (prefix-out forge: (struct-out Scope))
         (prefix-out forge: (struct-out Bound))
         (prefix-out forge: (struct-out Options))
         (prefix-out forge: (struct-out State))
         (prefix-out forge: (struct-out Run-spec))
         (prefix-out forge: (struct-out Run))         
         (prefix-out forge: (struct-out sbound)))

; Let forge/core work with the model tree without having to require helpers
; Don't prefix with tree:, that's already been done when importing
(provide (all-from-out forge/lazy-tree))

(provide (prefix-out forge: (all-from-out forge/sigs-structs)))

; Export these from structs without forge: prefix
(provide implies iff <=> ifte int>= int<= ni != !in !ni <: :> xor)
(provide Int succ min max)

; Export these from sigs-functional
; so that they can be used for scripting in forge/core
(provide make-sig
         make-relation
         make-inst
         run-from-state
         make-run
         check-from-state
         make-check
         test-from-state
         make-test)

; Export everything for doing scripting
(provide (prefix-out forge: (all-defined-out)))
(provide (prefix-out forge: (struct-out bound)))
(provide (prefix-out forge: relation-name))

(provide (prefix-out forge: curr-state)
         (prefix-out forge: update-state!))

(provide (struct-out Sat)
         (struct-out Unsat))

(provide (for-syntax add-to-execs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; State Updaters  ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; state-add-runmap :: State, Symbol, Run -> State
(define (state-add-runmap state name r)
  (struct-copy State state
               [runmap (hash-set (State-runmap state) name r)]))

; state-add-sig :: State, Symbol, Sig, (Symbol | #f) -> State
; Adds a new Sig to the given State; if new Sig extends some
; other Sig, then updates that Sig with extension.
(define (state-add-sig state name new-sig extends)
  (when (member name (State-sig-order state))
    (raise-user-error (format "tried to add sig ~a, but it already existed" name)))
  ;(define new-sig (Sig name rel one abstract extends))
  (when (and extends (not (member extends (State-sig-order state))))
    (raise-user-error "Can't extend nonexistent sig."))

  (define new-state-sigs (hash-set (State-sigs state) name new-sig))
  (define new-state-sig-order (append (State-sig-order state) (list name)))

  (struct-copy State state
               [sigs new-state-sigs]
               [sig-order new-state-sig-order]))

; state-add-relation :: State, Symbol, Relation -> State
; Adds a new relation to the given State.
(define (state-add-relation state name new-relation)
  (when (member name (State-relation-order state))
    (error (format "tried to add relation ~a, but it already existed" name)))
  ;(define new-relation (Relation name rel rel-sigs breaker))
  (define new-state-relations (hash-set (State-relations state) name new-relation))
  (define new-state-relation-order (append (State-relation-order state) (list name)))
  (struct-copy State state
               [relations new-state-relations]
               [relation-order new-state-relation-order]))

; state-add-pred :: State, Symbol, Predicate -> State
; Adds a new predicate to the given State.
(define (state-add-pred state name pred)
  (define new-state-pred-map (hash-set (State-pred-map state) name pred))
  (struct-copy State state
               [pred-map new-state-pred-map]))

; state-add-fun :: State, Symbol, Function -> State
; Adds a new function to the given State.
(define (state-add-fun state name fun)
  (define new-state-fun-map (hash-set (State-fun-map state) name fun))
  (struct-copy State state
               [fun-map new-state-fun-map]))

; state-add-const :: State, Symbol, Constant -> State
; Adds a new constant to the given State.
(define (state-add-const state name const)
  (define new-state-const-map (hash-set (State-const-map state) name const))
  (struct-copy State state
               [const-map new-state-const-map]))

; state-add-inst :: State, Symbol, Inst -> State
; Adds a new inst to the given State.
(define (state-add-inst state name inst)
  (define new-state-inst-map (hash-set (State-inst-map state) name inst))
  (struct-copy State state
               [inst-map new-state-inst-map]))

; this is not managed by Forge's "rolling state"; it should only be set by the command-line.
(define option-overrides (box '()))

(define (set-option! option value #:original-path [original-path #f])
  (cond [(member option (unbox option-overrides))
         (printf "Option ~a was given when Forge started with --override option; ignoring assignment to ~a.~n"
                 option value)]
        [(or (equal? option 'verbosity) (equal? option 'verbose))
         (set-verbosity value)]
        [else
         (update-state! (state-set-option curr-state option value #:original-path original-path))]))

; state-set-option :: State, Symbol, Symbol -> State
; Sets option to value for state.
(define (state-set-option state option value #:original-path [original-path #f])
  (define options (State-options state))

  (unless (hash-ref option-types option #f)
    (raise-user-error (format "No such option: ~a" option)))
  (unless ((hash-ref option-types option) value)
    (raise-user-error (format "Setting option ~a requires ~a; received ~a"
                              option (hash-ref option-types-names option) value)))
  
  (define new-options
    (cond
      [(equal? option 'eval-language)
       (unless (or (equal? value 'surface) (equal? value 'core))
         (raise-user-error (format "Invalid evaluator language ~a; must be surface or core.~n"
                                   value)))
       (struct-copy Options options
                    [eval-language value])]
      [(equal? option 'solver)
       (struct-copy Options options
                    [solver
                     (if (and (string? value) original-path)
                         (path->string (build-path original-path (string->path value)))
                         value)])]
      [(equal? option 'backend)
       (struct-copy Options options
                    [backend value])]
      [(equal? option 'sb)
       (struct-copy Options options
                    [sb value])]
      [(equal? option 'coregranularity)
       (struct-copy Options options
                    [coregranularity value])]
      [(equal? option 'logtranslation)
       (struct-copy Options options
                    [logtranslation value])]
      [(equal? option 'local_necessity)
       (struct-copy Options options
                    [local_necessity value])]
      [(equal? option 'min_tracelength)
       (let ([max-trace-length (get-option state 'max_tracelength)])
         (if (> value max-trace-length)
             (raise-user-error (format "Cannot set min_tracelength to ~a because min_tracelength cannot be greater than max_tracelength. Current max_tracelength is ~a."
                                       value max-trace-length))
             (struct-copy Options options
                          [min_tracelength value])))]
      [(equal? option 'max_tracelength)
       (let ([min-trace-length (get-option state 'min_tracelength)])
         (if (< value min-trace-length)
             (raise-user-error (format "Cannot set max_tracelength to ~a because max_tracelength cannot be less than min_tracelength. Current min_tracelength is ~a."
                                       value min-trace-length))
             (struct-copy Options options
                          [max_tracelength value])))]
      [(equal? option 'problem_type)
       (struct-copy Options options
                    [problem_type value])]
      [(equal? option 'target_mode)
       (struct-copy Options options
                    [target_mode value])]
      [(equal? option 'core_minimization)
       (struct-copy Options options
                    [core_minimization value])]
      [(equal? option 'skolem_depth)
       (struct-copy Options options
                    [skolem_depth value])]
      [(equal? option 'run_sterling)
       (struct-copy Options options
                    [run_sterling
                     (if (and (string? value) original-path)
                         (path->string (build-path original-path (string->path value)))
                         value)])]
      [(equal? option 'sterling_port)
       (struct-copy Options options
                    [sterling_port value])]
      [(equal? option 'engine_verbosity)
       (struct-copy Options options
                    [engine_verbosity value])]
      [(equal? option 'test_keep)
       (struct-copy Options options
                    [test_keep value])]
      [(equal? option 'no_overflow)
       (struct-copy Options options
                    [no_overflow value])]))

  (struct-copy State state
               [options new-options]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Forge Commands  ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The environment threaded through commands
(define curr-state init-state)
(define (update-state! new-state) 
  (set! curr-state new-state))

; check-temporal-for-var :: Boolean String -> void
; raises an error if is-var is true and the problem_type option is 'temporal
; uses the given name in the error message
; meant to only allow var sigs and relations in temporal specs
(define (check-temporal-for-var is-var name)
  (cond
    [(and is-var
          (not (equal? (get-option curr-state 'problem_type)
            'temporal)))
     (raise-user-error (format "Can't have var ~a unless problem_type option is temporal"
                               name))]))

; Declare a new sig.
; (sig name [|| [#:one] [#:abstract]] [#:is-var isv] [[|| #:in #:extends] parent])
; Extending a sig with #:in does NOT work yet,
; it's only been added here so that it throws the correct error
; when the Expander tries to do that
(define-syntax (sig stx)
  (syntax-parse stx
    [(sig (~optional (#:lang check-lang) #:defaults ([check-lang #''checklangNoCheck]))
          name:id
          (~alt (~optional (~seq #:in super-sig:expr)) ;check if this supports "sig A in B + C + D ..."
                (~optional (~seq #:extends parent:expr))
                (~optional (~or (~seq (~and #:one one-kw))
                                (~seq (~and #:lone lone-kw))
                                (~seq (~and #:abstract abstract-kw))))
                (~optional (~seq #:is-var isv) #:defaults ([isv #'#f]))) ...)
     (quasisyntax/loc stx
       (begin
         (define true-name 'name)
         (define true-one (~? (~@ (or #t 'one-kw)) (~@ #f)))
         (define true-lone (~? (~@ (or #t 'lone-kw)) (~@ #f)))
         (define true-abstract (~? (~@ (or #t 'abstract-kw)) (~@ #f)))
         (define true-parent (~? (get-sig curr-state parent)
                                 #f))
         (define true-parent-name
           (if true-parent (Sig-name true-parent) #f))
         (define name (make-sig true-name
                                #:one true-one
                                #:lone true-lone
                                #:abstract true-abstract
                                #:is-var isv
                                ;let #:in default to #f until it is implemented
                                #:extends true-parent
                                #:info (nodeinfo #,(build-source-location stx) check-lang #f)))
         ;make sure it isn't a var sig if not in temporal mode
         (~@ (check-temporal-for-var isv true-name))
         ;Currently when lang/expander.rkt calls sig with #:in,
         ;super-sig is #'(raise "Extending with in not yet implemented.")
         ;This is just here for now to make sure that error is raised.
         (~? super-sig)
         (update-state! (state-add-sig curr-state true-name name true-parent-name))))]))

; Declare a new relation (should probably be called "field") 
; (relation name (sig1 sig2 sigs ...) [|| [#:is breaker] [#:is-var isv]])
(define-syntax (relation stx)
  (syntax-parse stx
    [(relation name:id (sig1:id sig2:id sigs ...)
               (~optional (~seq #:is breaker:id))
               (~optional (~seq #:is-var isv) #:defaults ([isv #'#f])))
     (quasisyntax/loc stx
       (begin
         (define true-name 'name)
         (define true-sigs (list (thunk (get-sig curr-state sig1))
                                 (thunk (get-sig curr-state sig2))
                                 (thunk (get-sig curr-state sigs)) ...))
         ;(printf "relation sigs: ~a~n" (list sig1 sig2 sigs ...))
         ; (define true-sigs (map (compose Sig-name ;;; Bugged since relation before sig in #lang forge
         ;                                 (curry get-sig curr-state ))
         ;                        (list sig1 sig2 sigs ...)))
         (define true-breaker (~? breaker #f))
         (define checker-hash (get-ast-checker-hash))
         (when (hash-has-key? checker-hash 'field-decl) ((hash-ref checker-hash 'field-decl) true-breaker))
         (define name (make-relation true-name
                                     true-sigs
                                     #:is true-breaker
                                     #:is-var isv
                                     #:info (nodeinfo #,(build-source-location stx) 'checklangNoCheck #f)))
         ;make sure it isn't a var sig if not in temporal mode
         (~@ (check-temporal-for-var isv true-name))
         (update-state! (state-add-relation curr-state true-name name))))]
    ; Case: check-lang
    [(relation (#:lang check-lang) name:id (sig1:id sig2:id sigs ...)
               (~optional (~seq #:is breaker:id))
               (~optional (~seq #:is-var isv) #:defaults ([isv #'#f])))
     (quasisyntax/loc stx
       (begin
         (define true-name 'name)
         (define true-sigs (list (thunk (get-sig curr-state sig1))
                                 (thunk (get-sig curr-state sig2))
                                 (thunk (get-sig curr-state sigs)) ...))
         ;(printf "relation sigs: ~a~n" (list sig1 sig2 sigs ...))
         ; (define true-sigs (map (compose Sig-name ;;; Bugged since relation before sig in #lang forge
         ;                                 (curry get-sig curr-state ))
         ;                        (list sig1 sig2 sigs ...)))
         (define true-breaker (~? breaker #f))
         (define checker-hash (get-ast-checker-hash))
         (when (hash-has-key? checker-hash 'field-decl) ((hash-ref checker-hash 'field-decl) true-breaker))
         (define name (make-relation true-name
                                     true-sigs
                                     #:is true-breaker
                                     #:is-var isv
                                     #:info (nodeinfo #,(build-source-location stx) check-lang #f)))
         ;make sure it isn't a var sig if not in temporal mode
         (~@ (check-temporal-for-var isv true-name))
         (update-state! (state-add-relation curr-state true-name name))))]))

; Used for sealing formula structs that come from wheats, which should be obfuscated
(begin-for-syntax  
  (define-splicing-syntax-class pred-type
    #:description "optional pred flag"
    #:attributes ((seal 0))
    ; If this is a "wheat pred", wrap in a make-wheat call
    (pattern (~datum #:wheat)
      #:attr seal #'make-wheat)
    ; Otherwise, just pass-through
    (pattern (~seq)
      #:attr seal #'values))

  ; [v] | [v expr] | [v expr mult]
  ; We want to enable arbitrary code within the expr portion
  (define-splicing-syntax-class param-decl-class
    #:description "predicate or function variable declaration"
    #:attributes (mexpr name)
    (pattern name:id                            
      #:attr expr #'univ ; default domain
      #:attr mexpr #'(mexpr expr (if (> (node/expr-arity expr) 1) 'set 'one)))
    (pattern (name:id expr)
      #:attr mexpr #'(mexpr expr (if (> (node/expr-arity expr) 1) 'set 'one)))
    (pattern (name:id expr mult)
      #:attr mexpr #'(mexpr expr mult)))

  ; No variable ID, just a "result type":
  ; expr | [expr mult]
  (define-splicing-syntax-class codomain-class
    #:description "codomain expression in helper function declaration"
    #:attributes (mexpr)
    (pattern (expr mult:id)
      #:attr mexpr #'(mexpr expr 'mult))
    (pattern (expr mult)
      #:attr mexpr #'(mexpr expr mult))
    ; Catch expr without mult (but must come last, or will match both of above)
    (pattern expr                            
      #:attr mexpr #'(mexpr expr (if (> (node/expr-arity expr) 1) 'set 'one))))
  )

; Declare a new predicate
; Two cases: one with args, and one with no args
(define-syntax (pred stx)
  (syntax-parse stx
    ; no decls: predicate is already the AST node value, without calling it
    [(pred pt:pred-type
           (~optional (#:lang check-lang) #:defaults ([check-lang #''checklangNoCheck]))
           name:id conds:expr ...+)
     (with-syntax ([decl-info #`(nodeinfo #,(build-source-location stx) check-lang #f)]
                   [inner-unsyntax #'unsyntax])
       (quasisyntax/loc stx
         (begin
           ; - Use a macro in order to capture the location of the _use_.
           ; For 0-arg predicates, produce the AST node immediately
           (define-syntax (name stx2)
             (syntax-parse stx2
               [name
                (quasisyntax/loc stx2
                  ; - "pred spacer" still present, even if no arguments, to consistently record use of a predicate
                  (let* ([the-info (nodeinfo (inner-unsyntax (build-source-location stx2)) check-lang #f)]
                        [ast-node (pt.seal (node/fmla/pred-spacer the-info 'name '() (&&/info the-info conds ...)))])
                    (update-state! (state-add-pred curr-state 'name ast-node))
                    ast-node))])) )))]

    ; some decls: predicate must be called to evaluate it
    [(pred pt:pred-type
           (~optional (#:lang check-lang) #:defaults ([check-lang #''checklangNoCheck]))
           (name:id decls:param-decl-class  ...+) conds:expr ...+)
     (with-syntax ([decl-info #`(nodeinfo #,(build-source-location stx) check-lang #f)]
                   [inner-unsyntax #'unsyntax])
       (define result-stx
         (with-syntax ([functionname (format-id #'name "~a/func" #'name)])
           (quasisyntax/loc stx
             (begin
               ; - Use a macro in order to capture the location of the _use_.

               (define-syntax (name stx2)
                 ;(printf "in macro: ~a~n" stx2)
                 (syntax-parse stx2
                   ; If it's the macro name and all the args, expand to an invocation of the procedure
                   [(name args (... ...))
                    (quasisyntax/loc stx2
                      (functionname args (... ...) #:info (nodeinfo
                                                           (inner-unsyntax (build-source-location stx2)) check-lang #f)))]
                   ; If it's just the macro name, expand to a lambda that can take the same arguments when available
                   [name:id
                    (quasisyntax/loc stx2
                      (lambda (decls.name ...)
                        (functionname decls.name ...
                                      #:info (nodeinfo
                                              (inner-unsyntax (build-source-location stx2)) check-lang #f))))]
                   ))
               
               ; - "pred spacer" added to record use of predicate along with original argument declarations etc.
               (define (functionname decls.name ... #:info [the-info #f])
                 (unless (or (integer? decls.name) (node/expr? decls.name) (node/int? decls.name))
                   (error (format "Argument '~a' to pred ~a was not a Forge expression, integer-expression, or Racket integer. Got ~v instead."
                                  'decls.name 'name decls.name)))
                 ...
                 (pt.seal (node/fmla/pred-spacer the-info 'name (list (apply-record 'decls.name decls.mexpr decls.name) ...)
                                                 (&&/info the-info conds ...))))
               

               
               
               (update-state! (state-add-pred curr-state 'name functionname)))))) 
       result-stx)]))

(define/contract (repeat-product expr count)
  [@-> node/expr? number? node/expr?]
  (cond [(> count 1)
         (-> expr (repeat-product expr (@- count 1)))]
        [else expr]))

; Declare a new function
; (fun (name var ...) body)
; (fun (name (var expr <multiplicity>]) ...) body)
(define-syntax (fun stx)
  (syntax-parse stx
    [(fun (name:id decls:param-decl-class ...+)
          result:expr
          ; Note: default for *attribute*
          (~optional (~seq #:codomain codomain:codomain-class)
                     #:defaults ([codomain.mexpr #'(mexpr (repeat-product univ (node/expr-arity result))
                                                          (if (> (node/expr-arity result) 1) 'set 'one))])))

     ; TODO: there is no check-lang in this macro; does that mean that language-level details are lost within a helper fun?

     (with-syntax ([decl-info #`(nodeinfo #,(build-source-location stx) 'checklangNoCheck #f)]
                   [functionname (format-id #'name "~a/func" #'name)]
                   [inner-unsyntax #'unsyntax])
       (quasisyntax/loc stx
         (begin
           ; - create a macro that captures the syntax location of the _use_
           (define-syntax (name stx2)
             (syntax-parse stx2
               [(name args (... ...))
                (quasisyntax/loc stx2
                  (functionname args (... ...) #:info (nodeinfo
                                                       (inner-unsyntax (build-source-location stx2)) 'checklangNoCheck #f)))]
               [name:id
                (quasisyntax/loc stx2
                  (lambda (decls.name ...)
                    (functionname decls.name ... #:info (nodeinfo (inner-unsyntax (build-source-location stx2)) 'checklangNoCheck #f))))]))
           
           ; - "fun spacer" added to record use of function along with original argument declarations etc.           
           (define (functionname decls.name ... #:info [the-info #f])
             (unless (or (integer? decls.name) (node/expr? decls.name) (node/int? decls.name))
               (error (format "Argument '~a' to fun ~a was not a Forge expression, integer-expression, or Racket integer. Got ~v instead."
                              'decls.name 'name decls.name)))
             ...
             ; maintain the invariant that helper functions are always rel-expression valued
             (define safe-result
               (cond [(node/int? result)
                      (node/expr/op/sing (node-info result) 1 (list result))]
                     [else result]))
             (node/expr/fun-spacer
              the-info                      ; from node
              (node/expr-arity safe-result) ; from node/expr
              'name
              (list (apply-record 'decls.name decls.mexpr decls.name) ...)
              codomain.mexpr
              safe-result))
           (update-state! (state-add-fun curr-state 'name name)))))]))

; Declare a new constant
; (const name value)
(define-syntax (const stx)
  (syntax-parse stx
    [(const name:id value:expr) 
      #'(begin 
          (define name value)
          (update-state! (state-add-const curr-state 'name name)))]))

; Define a new bounding instance
; (inst name binding ...)
(define-syntax (inst stx)
  (syntax-parse stx
    [(inst name:id binds:expr ...)
     (syntax/loc stx
       (begin
         (define name (make-inst (flatten (list binds ...))))
         (update-state! (state-add-inst curr-state 'name name))))]))

; Run a given spec
; (run name
;      [#:pred [(pred ...)]] 
;      [#:scope [((sig [lower 0] upper) ...)]]
;      [#:inst instance-name])
(define-syntax (run stx)
  (define command stx)
  (syntax-parse stx
    [(run name:id
          (~alt
            (~optional (~or (~seq #:preds (preds ...))
                            (~seq #:preds pred)))
            (~optional (~seq #:scope ((sig:id (~optional lower:nat) upper:nat) ...)))
            (~optional (~or (~seq #:bounds (boundss ...))
                            (~seq #:bounds bound)))
            (~optional (~seq #:solver solver-choice)) ;unused
            (~optional (~seq #:backend backend-choice)) ;unused
            (~optional (~seq #:target target-instance))
            ;the last 3 appear to be unused in functional forge
            (~optional (~seq #:target-distance target-distance))
            (~optional (~or (~and #:target-compare target-compare)
                            (~and #:target-contrast target-contrast)))) ...)
     (quasisyntax/loc stx (begin
         ;(define checker-hash (get-ast-checker-hash))
         ;(printf "sigs run ~n ch= ~a~n" checker-hash)
         (define run-state curr-state)
         (define run-name (~? (~@ 'name) (~@ 'no-name-provided)))
         (define run-preds (~? (list preds ...) (~? (list pred) (list))))         
         (define run-scope
           (~? (~@ (list (~? (~@ (list sig lower upper))
                             (~@ (list sig upper))) ...))
               (~@ (list))))
         #;(define run-scope
           (~? (list (list sig (~? lower) upper) ...) (list)))
         #;(define run-scope
           (~? (list (~? (list sig lower upper) (list sig upper)) ...) (list)))
         (define run-bounds (~? (list boundss ...) (~? (list bound) (list))))                  
         (define run-solver (~? 'solver-choice #f))
         (define run-backend (~? 'backend #f))
         (define run-target
           (~? (Target target-instance ;(cdr target-instance)
                       (~? 'target-distance 'close_noretarget))
               #f))
         (define run-command #'#,command)         
         (define name
           (run-from-state run-state
                           #:name run-name
                           #:preds run-preds
                           #:scope run-scope
                           #:bounds run-bounds
                           #:solver run-solver
                           #:backend run-backend
                           #:target run-target
                           #:command run-command))
         (update-state! (state-add-runmap curr-state 'name name))))]))

; Test that a spec is sat or unsat
; (test name
;       [#:preds [(pred ...)]] 
;       [#:scope [((sig [lower 0] upper) ...)]]
;       [#:bounds [bound ...]]
;       [|| sat unsat]))
(define-syntax (test stx)
  (syntax-case stx ()
    [(test name args ... #:expect expected)  
     (add-to-execs
      (with-syntax ([loc (build-source-location stx)]
                    [run-stx (syntax/loc stx (run name args ...))]
                    [check-stx (syntax/loc stx (check name args ...))])
       (quasisyntax/loc stx 
         (cond
          [(equal? 'expected 'forge_error)
           ; Expecting an error. If we receive one, do nothing. 
           ; Otherwise, continue to report the error and then close the run.
           ; (N.B., this assumes the run isn't actually created or sent to the solver.)
           (define run-reference #f)
           (with-handlers ([exn:fail:user? void])
             ;#,(syntax/loc stx (run name args ...))
             run-stx
             ; Cannot throw the new "failed test" Forge error here, or it will be caught and ignored
             (set! run-reference name)
             (close-run name))
           ; Instead, wait and throw it here (note this will only happen if _NO_ user-error was
           ; produced by the run, and thus a run-reference is present.
           (when run-reference
             (report-test-failure
              #:name 'name
              #:msg (format "Failed test ~a. No Forge error was produced." 'name)
              #:context loc
              #:run run-reference
              #:sterling #f))
           (when (member 'name (hash-keys (State-runmap curr-state)))
             (printf "Warning: successful `is forge_error` test run left in state environment: ~a.~n" 'name))]

          ; It may not be immediately obvious why we would ever test for unknown,
          ; but it seems reasonable to support it in the engine, regardless.
          [(member 'expected '(sat unsat unknown))           
           run-stx
           (define first-instance (tree:get-value (Run-result name)))
           (if (not (equal? (if (Sat? first-instance) 'sat 'unsat) 'expected))
               (report-test-failure
                #:name 'name
                #:msg (format "Failed test ~a. Expected ~a, got ~a.~a"
                              'name 'expected (cond [(Sat? first-instance) 'sat]
                                                    [(Unsat? first-instance) 'unsat]
                                                    [(Unknown? first-instance) 'unknown])
                              ; Report additional info for Sat and Unsat. If Unknown, report nothing.
                              (cond [(Sat? first-instance)
                                     (format " Found instance ~a" first-instance)]
                                     [(and (Unsat? first-instance) (Unsat-core first-instance))
                                      (format " Core: ~a" (Unsat-core first-instance))]
                                     [else ""]))
                #:context loc
                #:instance first-instance
                #:run name)
               (close-run name))]

          [(equal? 'expected 'theorem)          
           ;#,(syntax/loc stx (check name args ...))
           check-stx
           (define first-instance (tree:get-value (Run-result name)))
           (cond [(Sat? first-instance)
                  (report-test-failure #:name 'name
                                       #:msg (format "Theorem ~a failed. Found instance:~n~a"
                                                     'name first-instance)
                                       #:context loc
                                       #:instance first-instance
                                       #:run name)]
                 [(Unknown? first-instance)
                  (report-test-failure #:name 'name
                                       #:msg (format "Theorem ~a failed. Solver returned Unknown.~n"
                                                     'name)
                                       #:context loc
                                       #:instance first-instance
                                       #:run name)]
                 [else 
                  (close-run name)])]

          [else (raise-forge-error                 
                 #:msg (format "Illegal argument to test. Received ~a, expected sat, unsat, or theorem."
                               'expected)
                 #:context loc)]))))]))

(define-syntax (example stx)  
  (syntax-parse stx
    [(_ name:id pred bounds ...)
     (add-to-execs
      (with-syntax* ([double-check-name (format-id #'name "double-check_~a_~a" #'name (gensym))]
                     [run-stx (syntax/loc stx (run name #:preds [pred] #:bounds [bounds ...]))]
                     [double-check-run-stx (syntax/loc stx (run double-check-name #:preds [] #:bounds [bounds ...]))])
       (quasisyntax/loc stx (begin
         (when (eq? 'temporal (get-option curr-state 'problem_type))
           (raise-forge-error
            #:msg (format "example ~a: Can't have examples when problem_type option is temporal" 'name)
            #:context #,(build-source-location stx)))
         run-stx
         (define first-instance (tree:get-value (Run-result name)))
         (cond
           [(Unsat? first-instance)
            ; Run a second check to see if {} would have also failed, meaning this example
            ; violates the sig/field declarations.
            double-check-run-stx
            (define double-check-instance (tree:get-value (Run-result double-check-name)))
            (close-run double-check-name) ;; always close the double-check run immediately
            
            (cond
              [(Sat? double-check-instance)
               (report-test-failure #:name 'name #:msg (format "Invalid example '~a'; the instance specified does not satisfy the given predicate." 'name)
                                     #:context #,(build-source-location stx)
                                     #:instance first-instance
                                     #:run name)]
              [(Unsat? double-check-instance)
               (report-test-failure #:name 'name #:msg (format (string-append "Invalid example '~a'; the instance specified is impossible. "
                                                                              "This means that the specified bounds conflict with each other "
                                                                              "or with the sig/field definitions.")
                                                               'name)
                                     #:context #,(build-source-location stx)
                                     #:instance first-instance
                                     #:run name)]
              [(Unknown? double-check-instance)
               (report-test-failure #:name 'name #:msg (format "Invalid example '~a'. Unable to determine if the instance given satisfies the sig/field definitions or specified bounds." 'name)
                                     #:context #,(build-source-location stx)
                                     #:instance first-instance
                                     #:run name)])]
           [else (close-run name)])))))]))

; Checks that some predicates are always true.
(define-syntax (check stx)
  (syntax-parse stx
    [(check name:id
            (~alt
             (~optional (~seq #:preds (pred ...)))
             (~optional (~seq #:scope ((sig:id (~optional lower:nat #:defaults ([lower #'0])) upper:nat) ...)))
             (~optional (~seq #:bounds (bound ...)))) ...)
     (with-syntax* ([pred-conj (syntax/loc stx (&& (~? (~@ pred ...))))]
                    [neg-pred-conj (syntax/loc stx (! pred-conj))])
       (quasisyntax/loc stx
         (run name (~? (~@ #:preds [neg-pred-conj]))
              (~? (~@ #:scope ([sig lower upper] ...)))
              (~? (~@ #:bounds (bound ...))))))]))


; Exprimental: Run in the context of a given external Forge spec
; (with path-to-forge-spec commands ...)
(define-syntax (with stx)
  (syntax-parse stx
    [(with (ids:id ... #:from module-name) exprs ...+)
     #'(let ([temp-state curr-state])
         (define ids (dynamic-require module-name 'ids)) ...
         (define result
           (let () exprs ...))
         (update-state! temp-state)
         result)]))

(define-for-syntax (add-to-execs stx)  
  (if (equal? (syntax-local-context) 'module)
      #`(module+ execs #,stx)
      stx))

; Experimental: Execute a forge file (but don't require any of the spec)
; (execute "<path/to/file.rkt>")
(define-syntax (execute stx)
  (syntax-case stx ()
    [(_ m) (replace-context stx (add-to-execs #'(require (submod m execs))))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Result Functions ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; make-model-generator :: Stream<model> -> (-> model)
; Creates a thunk which generates a new instance on each call.
(define (make-model-generator model-lazy-tree [mode 'next])
  (thunk
    (define ret (tree:get-value model-lazy-tree))
    (set! model-lazy-tree (tree:get-child model-lazy-tree mode))
    ret))

(provide (prefix-out forge: nsa))
(define nsa (make-parameter #f))
; display :: Run -> void
; Lifted function which, when provided a Run,
; generates a Sterling instance for it.
(define (true-display arg1 [arg2 #f])
  (if (not (Run? arg1))
      (if arg2 (@display arg1 arg2) (@display arg1))
      (let ()
        (define run arg1)
        (define model-lazy-tree (Run-result run))        
        (define (evaluate-str str-command)
          (define pipe1 (open-input-string str-command))
          (define pipe2 (open-input-string (format "eval ~a" str-command)))

          (with-handlers ([(lambda (x) #t) 
                           (lambda (exn) (exn-message exn))])
            ; Read command as syntax from pipe
            (define expr
              (cond [(equal? (get-option curr-state 'eval-language) 'surface)
                     (forge-lang:parse "/no-name" (forge-lang:make-tokenizer pipe2))]
                    [(equal? (get-option curr-state 'eval-language) 'core)
                     (read-syntax 'Evaluator pipe1)]
                    [else (raise-user-error
                           #:msg "Could not evaluate in current language - must be surface or core."
                           #:context #f)]))

            ;(printf "Run Atoms: ~a~n" (Run-atoms run))

            ; Evaluate command
            (define full-command (datum->syntax #f `(let
              ,(for/list ([atom (Run-atoms run)]
                          #:when (symbol? atom))
                 `[,atom (atom ',atom)])
                 ,expr)))

            (printf "full-command: ~a~n" full-command)
            
            (define ns (namespace-anchor->namespace (nsa)))
            (define command (eval full-command ns))
            
            (evaluate run '() command)))

        (define (get-contrast-model-generator model compare distance)
          (unless (member distance '(close far))
            (raise (format "Contrast model distance expected one of ('close, 'far); got ~a" distance)))
          (unless (member compare '(compare contrast))
            (raise (format "Contrast model compare expected one of ('compare, 'contrast); got ~a" compare)))

          (define new-state 
            (let ([old-state (get-state run)])
              (state-set-option (state-set-option old-state 'backend 'pardinus)
                                'solver 'TargetSATSolver)))
          (define new-preds
            (if (equal? compare 'compare)
                (Run-spec-preds (Run-run-spec run))
                (list (! (foldr (lambda (a b) (&& a b))
                                  true
                                  (Run-spec-preds (Run-run-spec run)))))))
          
          (define new-target
            (if (Unsat? model) ; if satisfiable, move target
                (Run-spec-target (Run-run-spec run))
                (Target
                 (for/hash ([(key value) (first (Sat-instances model))]
                            #:when (member key (append (get-sigs new-state)
                                                       (get-relations new-state))))
                   (values key value))
                 distance)))

          (define contrast-run-spec
            (struct-copy Run-spec (Run-run-spec run)
                         [preds new-preds]
                         [target new-target]
                         [state new-state]))
          (define-values (run-result atom-rels server-ports kodkod-currents kodkod-bounds)
                         (send-to-solver contrast-run-spec))
          (define contrast-run 
            (struct-copy Run run
                         [name (string->symbol (format "~a-contrast" (Run-name run)))]
                         [run-spec contrast-run-spec]
                         [result run-result]
                         [server-ports server-ports]
                         [kodkod-currents kodkod-currents]))
          (get-result contrast-run))

        (display-model run
                       model-lazy-tree 
                       (get-relation-map run)
                       evaluate-str
                       (Run-name run) 
                       (Run-command run) 
                       "/no-name.rkt" 
                       (get-bitwidth
                         (Run-run-spec run)) 
                       empty
                       get-contrast-model-generator))))

(define-syntax (display stx)
  (syntax-case stx ()
    [(display args ...)
      (add-to-execs #'(true-display args ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Scope/Bound Updaters ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; set-bitwidth :: Scope, int -> Scope
; Updates the bitwidth for the given Scope.
;(define (set-bitwidth scope n)
;  (struct-copy Scope scope
;               [bitwidth n]))
;

(define (solution-diff s1 s2)
  (map instance-diff (Sat-instances s1) (Sat-instances s2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Seq Library  ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; reference:
; https://github.com/AlloyTools/org.alloytools.alloy/blob/master/org.alloytools.alloy.core/src/main/resources/models/util/seqrel.als

; need to provide through expander

(provide isSeqOf seqFirst seqLast indsOf idxOf lastIdxOf elems inds isEmpty hasDups seqRest)

(define-syntax (define-builtin stx)
  (syntax-parse stx
   [(define-builtin:id (opName:id locArg:id args:id ...) body:expr)
    (with-syntax ([opName/func (format-id #'opName "~a/func" #'opName)]
                  [ellip '...])
      (syntax/loc stx (begin
        (define-syntax (opName stxx)
          (syntax-parse stxx
            ; For use in forge/core; full s-expression expands to 0-ary procedure
            ; Note use of "ellip" to denote "..." for the inner macro.
            [(opName inner-args:id ellip)
             (quasisyntax/loc stxx
               (opName/func (nodeinfo #,(build-source-location stxx) 'checklangNoCheck #f) inner-args ellip))]
            ; For use with #lang forge; identifier by itself expands to 3+-ary procedure
            [opName
             (quasisyntax/loc stxx
               (lambda (args ...)
                 (opName/func (nodeinfo #,(build-source-location stxx) 'checklangNoCheck #f) args ...)))]))
        
        (define (opName/func locArg args ...)
          body)
        )))
    ]))

(define-builtin (isSeqOf info r1 d)
  (&&/info info
      (in/info info r1 (->/info info Int univ))
      (in/info info (join/info info Int r1) d)
      (all/info info ([i1 (join/info info r1 univ)])
           (&&/info info (int>= (sum/info info i1) (int 0))
               (lone/info info (join/info info i1 r1))))
      (all/info info ([e (join/info info Int r1)])
           (some/info info (join/info info r1 e)))
      (all/info info ([i1 (join/info info r1 univ)])
           (=>/info info (!= i1 (sing/info info (int 0)))
                    (some/info info (join/info info
                     (sing/info info
                      (subtract/info info
                       (sum/info info i1) (int 1))) r1))))))

(define-builtin (seqFirst info r)
  (join/info info
    (sing/info info (int 0))
    r))

(define-builtin (seqLast info r)
  (join/info info
    (sing/info info
      (subtract/info info
        (card/info info r) (int 1)))
    r))

; precondition: r isSeqOf something
(define-builtin (seqRest info r)
  (-/info info 
    (join/info info succ r)
    (->/info info (int -1) univ)))

(define-builtin (indsOf info r e)
  (join/info info r e))

(define-builtin (idxOf info r e)
  (min (join/info info r e)))

(define-builtin (lastIdxOf info r e)
  (max (join/info info r e)))

(define-builtin (elems info r)
  (join/info info Int r))

(define-builtin (inds info r)
  (join/info info r univ))

(define-builtin (isEmpty info r)
  (no/func r #:info info))

(define-builtin (hasDups info r)
  (some ([e (elems/func info r)])
    (some ([num1 (indsOf/func info r e)] [num2 (indsOf/func info r e)])
      (!= num1 num2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Reachability Library  ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide reachable)

(define (srcloc->string loc)
  (format "line ~a, col ~a, span: ~a" (source-location-line loc) (source-location-column loc) (source-location-span loc)))

; a reachable from b through r1 + r2 + ...
(define-syntax (reachable stx)
  (syntax-parse stx
    ; For use in forge/core; full s-expression expands to 0-ary procedure
    [(reachable a b r ...)
     (quasisyntax/loc stx
       (reachablefun #,(build-source-location stx) a b (list r ...)))]
    ; For use with #lang forge; identifier by itself expands to 3+-ary procedure
    [reachable
     (quasisyntax/loc stx
       (lambda (a b . r) (reachablefun #,(build-source-location stx) a b r)))]))



(define (reachablefun loc a b r)
  (unless (equal? 1 (node/expr-arity a))
    (raise-forge-error #:msg (format "First argument \"~a\" to reachable is not a singleton" (deparse a))
                       #:context a))
  (unless (equal? 1 (node/expr-arity b))
    (raise-forge-error #:msg (format "Second argument \"~a\" to reachable is not a singleton" (deparse b))
                       #:context b))
  (when (and (list? r) (< (length r) 1))
    (raise-forge-error #:msg (format "The reachable predicate expected at least three arguments, given ~a" (@+ (length r) 2))
                       #:context loc))
  
  (in/info (nodeinfo loc 'checklangNoCheck #f) 
           a 
           (join/info (nodeinfo loc (get-check-lang) #f) 
                      b 
                      (^/info (nodeinfo loc 'checklangNoCheck #f) (union-relations loc r)))))

(define (union-relations loc r-or-rs)
  (cond
    [(empty? r-or-rs) (raise-forge-error
                 #:msg "Unexpected: union-relations given no arguments. Please report this error."
                 #:context loc)]
    [(empty? (rest r-or-rs))
     (unless (equal? 2 (node/expr-arity (first r-or-rs)))
         (raise-forge-error
          #:msg (format "Field argument given to reachable is not a field: ~a" (deparse (first r-or-rs)))
          #:context (first r-or-rs)))
     (first r-or-rs)]
    [else
     (for ([r r-or-rs])
       (unless (equal? 2 (node/expr-arity r))
         (raise-forge-error
          #:msg (format "Field argument given to reachable is not a field: ~a" (deparse r))
          #:context r)))
       (+/info (nodeinfo loc 'checklangNoCheck #f) (first r-or-rs) (union-relations loc (rest r-or-rs)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Infrastructure for handling multiple test failures / multiple runs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Struct to hold test-failure information for eventual reporting
(struct test-failure (name msg context instance run sterling))
; Mutable value to store list of test-failure structs
(define delayed-test-failures null)
; Called to clear the mutable list
(define (reset-test-failures!) (set! delayed-test-failures null))

; Record (or report, depending on the value of the delay-test-failure-reporting?
; parameter) a test failure. 
(define (report-test-failure #:name name #:msg msg #:context context
                             #:instance [instance #f] #:run run #:sterling [sterling #t])
  ; Default is to not delay, but options may affect this.
  (cond [(not (equal? (get-option run 'test_keep) 'first))
         (unless (equal? (get-verbosity) 0)
           (printf "Test ~a failed. Continuing to run and will report details at the end.~n" name))
         ; close previous failure run, since we are keeping only the final failure for Sterling
         (unless (empty? delayed-test-failures)
           (close-run (test-failure-run (first delayed-test-failures))))
         ; then add this failure to the queue
         (set! delayed-test-failures (cons (test-failure name msg context instance run sterling)
                                           delayed-test-failures))]
        
        [else
         ; Raise a Forge error and stop execution; show Sterling if enabled.
         (when (>= (get-verbosity) 1)
           (printf "Test ~a failed. Stopping execution.~n" name))
         (when sterling
           (true-display run))
         (raise-forge-error #:msg msg #:context context)]))

; To be run at the very end of the Forge execution; reports test failures and opens
; only one failure (as only one solver will be kept open, at the moment)
(define (output-all-test-failures)
  ; In order, for each failure:  
  (define failures (remove-duplicates (reverse delayed-test-failures)))
  (unless (or (< (get-verbosity) 1) (empty? failures))
    (printf "~nSome tests failed. Reporting failures in order:~n~n" ))

  (define last-failure (if (empty? failures) #f (last failures)))
  
  (for ([failure failures])
    (define-values (name msg context instance run sterling)
      (values (test-failure-name failure)    (test-failure-msg failure)
              (test-failure-context failure) (test-failure-instance failure)
              (test-failure-run failure) (test-failure-sterling failure)))
        
    ; Print the error (don't raise an actual exception)
    (define sterling-or-instance (if (or (not sterling) (equal? (get-option run 'run_sterling) 'off))
                                     (format "Sterling disabled for this test. Reporting raw instance data:~n~a" instance)
                                     (format "Running Sterling to show instance generated, if any.~n~a"
                                             (if (equal? failure last-failure)
                                                 "Solver is active; evaluator and next are available."
                                                 "For all but the final test failure, the solver was closed to save memory; evaluator and next are unavailable."))))
    (raise-forge-error #:msg (format "~a ~a~n~n" msg sterling-or-instance)
                       #:context context
                       #:raise? #f)
    ; Display in Sterling (if run_sterling is enabled)
    (when (and sterling (equal? failure last-failure))
      (true-display run)))
  
  ; Return to empty-failure-list state.
  (reset-test-failures!))

(provide output-all-test-failures
         report-test-failure
         reset-test-failures!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Support for providing options at the command line
;   sigs-structs.rkt provides an `option-types` value that
;   we can use to cast inputs to the appropriate type.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The lowercase -o will set an initial option value, but will be OVERRIDDEN
; by file-level option statements. E.g., this at the command line (note the
; need to escape the backquote):
; racket ring_of_lights.frg -o run_sterling \'off -o verbose 1
;   * will NOT run sterling (because the example doesn't give a value for that option)
;   * WILL give verbose 5 output (because the example gives a value of 5 for that option)

; In contrast, the uppercase -O will set the initial option value and disallow it changing.
; E.g.,
; racket ring_of_lights.frg -o run_sterling \'off -O verbose 1
; will give verbose 1 output. 

(define (string->option-type name value)
  (define type-pred (hash-ref option-types name #f))
  (cond
    ; Arrived as a number
    [(string->number value) (string->number value)]
    ; Arrived as a single-quote prefixed symbol
    [(equal? (string-ref value 0) #\') (string->symbol (substring value 1))]
    ; Otherwise, try to infer from the type predicate for the option
    [(equal? type-pred symbol?) (string->symbol value)]
    [(equal? type-pred exact-nonnegative-integer?) (string->number value)]
    [(equal? type-pred exact-positive-integer?) (string->number value)]
    [(equal? type-pred exact-integer?) (string->number value)]
    ; This covers too much at the moment (some of the option-types values are /more complex predicates/)
    [else value]))

(require racket/cmdline)

;; BEWARE: this appears to interact with `raco` when installing the Forge package. E.g.,
;; printing out `remaining-args` will actually print something when installing Forge, but
;; with arguments from `raco`:
;;    cl result: (pkg install ./forge ./froglet)
;; This could technically cause a conflict with any existing `raco` arguments.
;; -o and -O are not used by `raco pkg install` as of June 14, 2024.

(define remaining-args (command-line
 ; Default: 
 ;#:program (find-system-path 'run-file)
 ; Default:
 ;#:argv (current-command-line-arguments)
 #:usage-help
 "When running Forge from the command line, use the -o or --option flag to send options."
 "Format: -o <option name> <option value>."
 "If the upper-case -O is used, the option cannot be rewritten by the model file."

 #:multi
 [("-o" "--option") OPTION-NAME OPTION-VALUE
                    "Option set"
                    (begin 
                      (printf "Setting ~a = ~a~n" (string->symbol OPTION-NAME) OPTION-VALUE)
                      (set-option! (string->symbol OPTION-NAME)
                                   (string->option-type OPTION-NAME OPTION-VALUE)))]
 [("-O" "--override") OPTION-NAME OPTION-VALUE
                      "Option set and override"
                      (begin
                        (printf "Setting and overriding ~a = ~a~n" (string->symbol OPTION-NAME) OPTION-VALUE)
                        (set-option! (string->symbol OPTION-NAME)
                                     (string->option-type OPTION-NAME OPTION-VALUE))
                        ; Don't allow the Forge file to reset this option.
                        (set-box! option-overrides (cons (string->symbol OPTION-NAME) (unbox option-overrides))))]
 
 #:args remaining-args remaining-args))