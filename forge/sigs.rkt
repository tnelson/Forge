#lang racket/base

(require (only-in racket/function thunk)
         (only-in racket/list first second rest empty empty? flatten remove-duplicates last)
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
         forge/breaks
         forge/utils/target-oriented)
(require (only-in forge/lang/reader [read-syntax read-surface-syntax]))
(require forge/server/eval-model)
(require forge/server/forgeserver)
(require forge/solver-specific/translate-to-kodkod-cli
         forge/solver-specific/translate-from-kodkod-cli
         forge/sigs-structs
         forge/evaluator
         (prefix-in tree: forge/utils/lazy-tree)
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
                  make-test
                  state-set-option
                  stop-solver-process!))
(require forge/choose-lang-specific)

; Commands
(provide sig relation fun const pred inst with)
(provide run check test example display execute start-sterling-menu)
(provide solution-diff evaluate)

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
         (prefix-out forge: (struct-out State))
         (prefix-out forge: (struct-out Run-spec))
         (prefix-out forge: (struct-out Run))         
         (prefix-out forge: (struct-out sbound)))

; Let forge/core work with the model tree without having to require helpers
; Don't prefix with tree:, that's already been done when importing
(provide (all-from-out forge/utils/lazy-tree))

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
         make-test
         reset-run-name-history!
         stop-solver-process!)

; Export everything for doing scripting
(provide (prefix-out forge: (all-defined-out)))
(provide (prefix-out forge: (struct-out bound)))
(provide (prefix-out forge: relation-name))

(provide (prefix-out forge: curr-state)
         (prefix-out forge: update-state!)
         (prefix-out forge: current-options))

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
    (raise-user-error (format "Can't extend nonexistent sig ~a. Options were: ~a"
                              extends (State-sig-order state))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Command-line flag status variables
; These are not managed by Forge's "rolling state"; they should only be set by the command-line.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define option-overrides (box '()))
(define disable-tests (box #f))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-option! option value #:original-path [original-path #f])
  (cond [(member option (unbox option-overrides))
         (printf "Option ~a was given when Forge started with --override option; ignoring assignment to ~a.~n"
                 option value)]
        [(or (equal? option 'verbosity) (equal? option 'verbose))
         (set-verbosity value)]
        [else
         (define new-state (state-set-option curr-state option value #:original-path original-path))
         (update-state! new-state)
         ; Also update the current-options parameter so tests can snapshot it
         (current-options (State-options new-state))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Forge Commands  ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The environment threaded through commands
(define curr-state init-state)
(define (update-state! new-state)
  (set! curr-state new-state))

; Parameter for snapshotting options at test definition time.
; Tests capture this value when defined, then parameterize with it when executed.
(define current-options (make-parameter (State-options init-state)))

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

; A list of helper names that have been expanded to reach the current context.
; (This is roughly analogous to a call-stack in programming, although there is
;  no actual stack.) Used to detect cyclic predicate references.
;
; The check is done in phase 0, when all macros are done expanding and AST 
; construction is just a series of nested procedure calls, and `parameterize`
; will keep the stack context for checking. That is, if referencing or altering
; this parameter, make sure it is done in a normal function, not a macro.
(define helpers-enclosing (make-parameter '()))

; Check that parameter names don't shadow existing definitions.
; Called at runtime (file load time) when pred/fun is being defined.
(define (check-param-shadowing! param-names helper-kind helper-name context-info)
  (for ([param-name param-names])
    (define shadowed-kind
      (cond
        [(hash-has-key? (State-sigs curr-state) param-name) "sig"]
        [(hash-has-key? (State-relations curr-state) param-name) "field"]
        [(hash-has-key? (State-pred-map curr-state) param-name) "predicate"]
        [(hash-has-key? (State-fun-map curr-state) param-name) "function"]
        [else #f]))
    (when shadowed-kind
      (raise-forge-warning
       #:msg (format "Parameter '~a' in ~a '~a' shadows an existing ~a with the same name."
                     param-name helper-kind helper-name shadowed-kind)
       #:context context-info))))

; Declare a new predicate
; Two cases: one with args, and one with no args
(define-syntax (pred stx)
  (syntax-parse stx
    ;;;;;;;;;;;;;;;;;
    ; 0-args case: predicate is already the AST node value, without calling it
    [(pred pt:pred-type
           (~optional (#:lang check-lang) #:defaults ([check-lang #''checklangNoCheck]))
           name:id conds:expr ...+)
     (with-syntax ([decl-info #`(nodeinfo #,(build-source-location stx) check-lang #f)]
                   [inner-unsyntax #'unsyntax]
                   [functionname (format-id #'name "~a/func" #'name)])
       (quasisyntax/loc stx
         (begin
           ; Break the chain of macro expansions with a runtime procedure.
           (define (functionname #:info [the-info #f])
             ; Check: cyclic references?
             (when (member 'name (helpers-enclosing))
               (raise-forge-error #:msg (format "recursive predicate detected: ~a eventually called itself. The chain of calls involved: ~a.~n"
                                                'name (helpers-enclosing))
                                  #:context the-info))
             (parameterize ([helpers-enclosing (cons 'name (helpers-enclosing))])
               (pt.seal (node/fmla/pred-spacer the-info 'name '() (&&/info the-info conds ...)))))

           ; - Use a macro in order to capture the location of the _use_.
           ; For 0-arg predicates, produce the AST node immediately.
           (define-syntax (name stx2)
             (syntax-parse stx2
               [name
                (quasisyntax/loc stx2
                  (let ([ast-node (functionname #:info (nodeinfo (inner-unsyntax (build-source-location stx2)) check-lang #f))])
                    (update-state! (state-add-pred curr-state 'name ast-node))
                    ast-node))]))
           
           (update-state! (state-add-pred curr-state 'name functionname)))))]

    ;;;;;;;;;;;;;;;;;
    ; >= 1-args case: predicate must be called to evaluate it
    [(pred pt:pred-type
           (~optional (#:lang check-lang) #:defaults ([check-lang #''checklangNoCheck]))
           (name:id decls:param-decl-class  ...+) conds:expr ...+)
     (with-syntax ([decl-info #`(nodeinfo #,(build-source-location stx) check-lang #f)]
                   [inner-unsyntax #'unsyntax])
       (define result-stx
         (with-syntax ([functionname (format-id #'name "~a/func" #'name)])
           (quasisyntax/loc stx
             (begin
               ; Check for parameter name shadowing
               (check-param-shadowing! '(decls.name ...) "predicate" 'name decl-info)

               ; - Use a macro in order to capture the location of the _use_.
               (define-syntax (name stx2)
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
                                              (inner-unsyntax (build-source-location stx2)) check-lang #f))))]))
               
               ; - "pred spacer" added to record use of predicate along with original argument declarations etc.
               (define (functionname decls.name ... #:info [the-info #f])
                 (unless (or (integer? decls.name) (node/expr? decls.name) (node/int? decls.name))
                   (error (format "Argument '~a' to pred ~a was not a Forge expression, integer-expression, or Racket integer. Got ~v instead."
                                  'decls.name 'name decls.name)))
                 ...
                 ; Check: cyclic references?
                 (when (member 'name (helpers-enclosing))
                   (raise-forge-error #:msg (format "recursive predicate detected: ~a eventually called itself. The chain of calls involved: ~a.~n"
                                                    'name (helpers-enclosing))
                                      #:context the-info))
                 (parameterize ([helpers-enclosing (cons 'name (helpers-enclosing))])
                   (pt.seal (node/fmla/pred-spacer the-info 'name (list (apply-record 'decls.name decls.mexpr decls.name) ...)
                                                   (&&/info the-info conds ...)))))
               
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
     (define result-syntax
       (with-syntax ([decl-info #`(nodeinfo #,(build-source-location stx) 'checklangNoCheck #f)]
                     [functionname (format-id #'name "~a/func" #'name)]
                     [inner-unsyntax #'unsyntax])
         (quasisyntax/loc stx
           (begin
             ; Check for parameter name shadowing
             (check-param-shadowing! '(decls.name ...) "function" 'name decl-info)

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
             
             (define (functionname decls.name ... #:info [the-info #f])
               (unless (or (integer? decls.name) (node/expr? decls.name) (node/int? decls.name))
                 (error (format "Argument '~a' to fun ~a was not a Forge expression, integer-expression, or Racket integer. Got ~v instead."
                                'decls.name 'name decls.name)))
               ...
               (when (member 'name (helpers-enclosing))
                 (raise-forge-error #:msg (format "recursive helper function detected: ~a eventually called itself. The chain of calls involved: ~a.~n"
                                                  'name (helpers-enclosing))
                                    #:context the-info))
               (parameterize ([helpers-enclosing (cons 'name (helpers-enclosing))])
                 ; avoid expanding result more than once
                 (define result-once result)
                 ; maintain the invariant that helper functions are always rel-expression valued
                 (define safe-result
                   (cond [(node/int? result-once)
                          (node/expr/op-on-ints/sing (node-info result-once) 1 (list result-once))]
                         [else result-once]))
                 ; - "fun spacer" added to record use of function along with original argument declarations etc.           
                 (node/expr/fun-spacer
                  the-info                      ; from node
                  (node/expr-arity safe-result) ; from node/expr
                  'name
                  (list (apply-record 'decls.name decls.mexpr decls.name) ...)
                  codomain.mexpr
                  safe-result)))
             (update-state! (state-add-fun curr-state 'name functionname))))))
     result-syntax]))

; Declare a new constant
; (const name value)
(define-syntax (const stx)
  (syntax-parse stx
    [(const name:id value:expr)
      #'(begin
          ; TODO: this requires 0-ary helper functions to be defined in order.
          ; Note that if this issue is fixed, suitable checks for cyclic reference
          ; must be added (see the `pred` and `fun` macros).
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

            ; In forge/functional, these 2 options are passed together as part of a Target struct.
            (~optional (~seq #:target target-pi-or-int))
            (~optional (~seq #:target-distance target-distance))
            
            ;the last 2 appear to be unused in functional forge
            (~optional (~or (~and #:target-compare target-compare)
                            (~and #:target-contrast target-contrast)))) ...)
     
     (quasisyntax/loc stx (begin
         ;(define checker-hash (get-ast-checker-hash))
;         (printf "sigs run ~n ch= ~a~n" checker-hash)
                     
         (define run-name (~? (~@ 'name) (~@ 'no-name-provided)))
         (define run-preds (~? (list preds ...) (~? (list pred) (list))))         
         (define run-scope
           (~? (~@ (list (~? (~@ (list sig lower upper))
                             (~@ (list sig upper))) ...))
               (~@ (list))))
         (define run-bounds (~? (list boundss ...) (~? (list bound) (list))))                  
         (define run-solver (~? 'solver-choice #f))
         (define run-backend (~? 'backend #f))

         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ; If the run has asked for an _integer expression_ to be targeted,
         ; augment the run with a "gadget" which must appear in the relational bounds.
         (define-values (target-pi bounds-maybe-with-gadget preds-maybe-with-gadget)
           (~?
            (cond
              [(or (Inst? target-pi-or-int)
                   (hash? target-pi-or-int))
               (values target-pi-or-int run-bounds run-preds)]
              [(node/int? target-pi-or-int)
               ; curr-state should be a box. But that would be a wide-ranging change,
               ; so instead provide a getter lambda so call-by-value works for us.
               (build-int-opt-gadget target-pi-or-int run-scope run-bounds run-preds
                                     (lambda () curr-state)
                                     update-state! state-add-sig state-add-relation)]
              [else
               (raise-forge-error #:msg (format "Unexpected target type: ~a" target-pi-or-int)
                                  #:context name)])
            (values #f run-bounds run-preds)))
         
         (define run-target
           (if target-pi
               (Target target-pi
                       (~? 'target-distance 'close_noretarget))
               #f))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;; Use current-options parameter so tests can snapshot options at definition time.
         ;; The state gets current sigs/preds/etc, but options from the parameter.
         (define run-state (struct-copy State curr-state [options (current-options)])) 
         (define run-command #'#,command)
         (define name
           (run-from-state run-state
                           #:name run-name
                           #:preds preds-maybe-with-gadget
                           #:scope run-scope
                           #:bounds bounds-maybe-with-gadget
                           #:solver run-solver
                           #:backend run-backend
                           #:target run-target
                           #:command run-command))
         (update-state! (state-add-runmap curr-state 'name name))))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Primary testing form: check whether a constraint-set, under
; some provided bounds, is sat, unsat, or an error. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (report-passing-test #:name name)
  (when (>= (get-verbosity) VERBOSITY_LOW)
    (printf "    Test passed: ~a~n" name)))

(define-syntax (test stx)
  (syntax-case stx ()
    [(test name args ... #:expect expected)
     (syntax/loc stx (test name args ... #:expect expected #:expect-details #f))]
    [(test name args ... #:expect expected #:expect-details expected-details)
     ; Snapshot options at definition time, then parameterize at execution time
     (with-syntax ([snapshot-id (generate-temporary 'options-snapshot)]
                   [loc (build-source-location stx)]
                   [run-stx (syntax/loc stx (run name args ...))]
                   [check-stx (syntax/loc stx (check name args ...))])
       (if (equal? (syntax-local-context) 'module)
           ; At module level: capture options now, defer test with parameterize
           (quasisyntax/loc stx
             (begin
               (define snapshot-id (current-options))
               (module+ execs
                 (parameterize ([current-options snapshot-id])
                   #,(quasisyntax/loc stx (test-body name loc run-stx check-stx expected expected-details))))))
           ; Not at module level: just run directly
           (quasisyntax/loc stx
             (test-body name loc run-stx check-stx expected expected-details))))]))

; Helper macro for test body - factored out to avoid duplication
(define-syntax (test-body stx)
  (syntax-case stx ()
    [(test-body name loc run-stx check-stx expected expected-details)
     (quasisyntax/loc stx
       (cond
           ; TODO: isn't this known at expansion time? We'll have the value of <expected>.
          [(equal? 'expected 'forge_error)
           ; Expecting an error. If we receive one, do nothing.
           ; Otherwise, continue to report the error and then close the run.
           ; (N.B., this assumes the run isn't actually created or sent to the solver.)
           (define run-reference #f)
           (define handler-failed #f)

           ; `is forge_error` should be satisfied by an error produced via raise-forge-error. These
           ; are exn:fail:user values. Other Racket errors, such as arity errors, use a different
           ; error value type, and so would not be considered valid for `is forge_error` testing.
           (with-handlers ([exn:fail:user?
                            (lambda (e)
                              (unless (or (not expected-details)
                                          (regexp-match (regexp expected-details) (exn-message e)))
                                (set! handler-failed #t)
                                (report-test-failure
                                 #:name 'name
                                 #:msg (format "Failed test ~a. Forge error was produced with unexpected message. Expected match: ~a. Actual message: ~a."
                                               'name expected-details (exn-message e))
                                 #:context loc
                                 #:run curr-state ; no run, so pass current state instead
                                 #:sterling #f)))])
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

           ; Only report passing if error was caught/matched as expected
           (unless (or run-reference handler-failed)
             (when (member 'name (hash-keys (State-runmap curr-state)))
               (printf "Warning: successful `is forge_error` test run left in state environment: ~a.~n" 'name))
             (report-passing-test #:name 'name))]

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
               (begin (report-passing-test #:name 'name)
                      (close-run name)))]

          [(equal? 'expected 'checked)
           ;#,(syntax/loc stx (check name args ...))
           check-stx
           (define first-instance (tree:get-value (Run-result name)))
           (cond [(Sat? first-instance)
                  (report-test-failure #:name 'name
                                       #:msg (format "Test ~a failed. Found counterexample instance:~n~a"
                                                     'name first-instance)
                                       #:context loc
                                       #:instance first-instance
                                       #:run name)]
                 [(Unknown? first-instance)
                  (report-test-failure #:name 'name
                                       #:msg (format "Test ~a failed. Solver returned Unknown.~n"
                                                     'name)
                                       #:context loc
                                       #:instance first-instance
                                       #:run name)]
                 [else
                  (begin (report-passing-test #:name 'name)
                         (close-run name))])]

          [(equal? 'expected 'theorem)
           (raise-forge-error #:msg "The syntax 'is theorem' is deprecated and will be re-enabled in a future version for complete solver backends only; use 'is checked' instead."
                              #:context loc)]

          [else (raise-forge-error
                 #:msg (format "Illegal argument to test. Received ~a, expected sat, unsat, checked, or forge_error."
                               'expected)
                 #:context loc)]))]))

(define-syntax (example stx)
  (syntax-parse stx
    [(_ name:id pred bounds ...)
     ; Snapshot options at definition time, then parameterize at execution time
     (with-syntax* ([snapshot-id (generate-temporary 'options-snapshot)]
                    [double-check-name (format-id #'name "double-check_~a_~a" #'name (gensym))]
                    [run-stx (syntax/loc stx (run name #:preds [pred] #:bounds [bounds ...]))]
                    [double-check-run-stx (syntax/loc stx (run double-check-name #:preds [] #:bounds [bounds ...]))])
       (if (equal? (syntax-local-context) 'module)
           (quasisyntax/loc stx
             (begin
               (define snapshot-id (current-options))
               (module+ execs
                 (parameterize ([current-options snapshot-id])
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
                     [else
                      (report-passing-test #:name 'name)
                      (close-run name)])))))
           ; Not at module level: just run directly (original behavior)
           (quasisyntax/loc stx
             (begin
               (when (eq? 'temporal (get-option curr-state 'problem_type))
                 (raise-forge-error
                  #:msg (format "example ~a: Can't have examples when problem_type option is temporal" 'name)
                  #:context #,(build-source-location stx)))
               run-stx
               (define first-instance (tree:get-value (Run-result name)))
               (cond
                 [(Unsat? first-instance)
                  double-check-run-stx
                  (define double-check-instance (tree:get-value (Run-result double-check-name)))
                  (close-run double-check-name)
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
                 [else
                  (report-passing-test #:name 'name)
                  (close-run name)])))))]))

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
                       "/no-name.frg" 
                       (get-bitwidth (Run-run-spec run)) 
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

  ; a in b.^(r + ...)
  ; While a, b, and all rs should be checked by the specific language the user is running, the 
  ; AST nodes created specifically for helpers should not be.
  (in/info (nodeinfo loc 'checklangNoCheck #f) 
           a 
           (join/info (nodeinfo loc 'checklangNoCheck #f) 
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
(struct test-failure (name msg context instance run-or-state sterling))
; Mutable value to store list of test-failure structs
(define delayed-test-failures null)
; Called to clear the mutable list
(define (reset-test-failures!) (set! delayed-test-failures null))

; Record (or report, depending on the value of the delay-test-failure-reporting?
; parameter) a test failure. 
(define (report-test-failure #:name name #:msg msg #:context context
                             #:instance [instance #f] #:run run-or-state #:sterling [sterling #t])
  ; Default is to not delay, but options may affect this.
  (cond [(not (equal? (get-option run-or-state 'test_keep) 'first))
         (unless (equal? (get-verbosity) 0)
           (printf "Test ~a failed. Continuing to run and will report details at the end.~n" name))
         ; close previous failure run, since we are keeping only the final failure for Sterling
         (unless (or (empty? delayed-test-failures)
                     (not (Run? (test-failure-run-or-state (first delayed-test-failures)))))
           (close-run (test-failure-run-or-state (first delayed-test-failures))))
         ; then add this failure to the queue
         (set! delayed-test-failures (cons (test-failure name msg context instance run-or-state sterling)
                                           delayed-test-failures))]
        
        [else
         ; Raise a Forge error and stop execution; show Sterling if enabled.
         (when (>= (get-verbosity) 1)
           (printf "******************** TEST FAILED *******************~nTest ~a failed. Stopping execution.~n" name))
         (cond [(and (Run? run-or-state) sterling (Sat? instance))
                (when (>= (get-verbosity) 1)
                  (printf "Test failed due to finding a counterexample, which will be displayed in Sterling.~n")
                  (printf "*****************************************************~n"))
                (true-display run-or-state)]
               [else
                (when (>= (get-verbosity) 1)
                  (printf "Test failed due to unsat/inconsistency. No counterexample to display.~n")
                  (printf "*****************************************************~n"))])
             
         ;; The error below is raised only after Sterling terminates, so messaging above is vital.
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
              (test-failure-run-or-state failure) (test-failure-sterling failure)))
        
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
  (define type-pred (hash-ref option-types (if (string? name) (string->symbol name) name) #f))
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
 #:multi
 [("-O" "--override") OPTION-NAME OPTION-VALUE
                      "Option set and override"
                      (begin
                        (printf "Setting and overriding ~a = ~a~n" (string->symbol OPTION-NAME) OPTION-VALUE)
                        (set-option! (string->symbol OPTION-NAME)
                                     (string->option-type OPTION-NAME OPTION-VALUE))
                        ; Don't allow the Forge file to reset this option.
                        (set-box! option-overrides (cons (string->symbol OPTION-NAME) (unbox option-overrides))))]
 [("-L" "--logfile") LOGFILE-PATH
                     "Log filename"
                     (setup-logfile! LOGFILE-PATH)]
 [("-N" "--notests")
  "Disable tests for this model execution (NOT YET SUPPORTED)"
  (begin
    (printf "(NOT YET SUPPORTED) Tests disabled.~n")
    (set-box! disable-tests #t))]
 
 
 #:args remaining-args remaining-args))
