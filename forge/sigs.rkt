#lang racket

(require (prefix-in @ racket) 
         (prefix-in @ racket/set))
(require syntax/parse/define)
(require racket/match)
(require (for-syntax racket/match racket/syntax syntax/srcloc))
(require (for-syntax syntax/strip-context))

;(require forge/choose-lang-specific)

(require "shared.rkt")
(require "lang/ast.rkt"
         "lang/bounds.rkt"
         "breaks.rkt")
(require (only-in "lang/reader.rkt" [read-syntax read-surface-syntax]))
(require "server/eval-model.rkt")
(require "server/forgeserver.rkt") ; v long
;(require (prefix-in kodkod: "kodkod-cli/server/kks.rkt")
;         (prefix-in kodkod: "kodkod-cli/server/server.rkt")
;         (prefix-in kodkod: "kodkod-cli/server/server-common.rkt"))
;(require (prefix-in pardinus: "pardinus-cli/server/kks.rkt")
;         (prefix-in pardinus: "pardinus-cli/server/server.rkt")
;         (prefix-in pardinus: "pardinus-cli/server/server-common.rkt"))
(require "translate-to-kodkod-cli.rkt"
         "translate-from-kodkod-cli.rkt"
         ;"last-checker.rkt"
         "sigs-structs.rkt"
         "evaluator.rkt"
         (prefix-in tree: "lazy-tree.rkt")
         "send-to-kodkod.rkt")
(require (only-in "lang/alloy-syntax/parser.rkt" [parse forge-lang:parse])
         (only-in "lang/alloy-syntax/tokenizer.rkt" [make-tokenizer forge-lang:make-tokenizer]))
(require (only-in "sigs-functional.rkt"
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
(provide is-unsat? is-sat?)

; export AST macros and struct definitions (for matching)
; Make sure that nothing is double-provided
(provide (all-from-out "lang/ast.rkt"))

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
(provide (all-from-out "lazy-tree.rkt"))

(provide (prefix-out forge: (all-from-out "sigs-structs.rkt")))

; Export these from structs without forge: prefix
(provide implies iff <=> ifte >= <= ni != !in !ni <: :>)
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
  (when (@and extends (@not (member extends (State-sig-order state))))
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

(define (set-option! option value)
  (cond [(or (equal? option 'verbosity)
             (equal? option 'verbose))
         (set-verbosity value)]
        [else
         (update-state! (state-set-option curr-state option value))]))

; state-set-option :: State, Symbol, Symbol -> State
; Sets option to value for state.
(define (state-set-option state option value)
  (define options (State-options state))

  (unless ((hash-ref option-types option) value)
    (raise-user-error (format "Setting option ~a requires ~a; received ~a"
                              option (hash-ref option-types option) value)))

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
                    [solver value])]
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
         (if (@> value max-trace-length)
             (raise-user-error (format "Cannot set min_tracelength to ~a because min_tracelength cannot be greater than max_tracelength. Current max_tracelength is ~a."
                                       value max-trace-length))
             (struct-copy Options options
                          [min_tracelength value])))]
      [(equal? option 'max_tracelength)
       (let ([min-trace-length (get-option state 'min_tracelength)])
         (if (@< value min-trace-length)
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
                    [run_sterling value])]))

  (struct-copy State state
               [options new-options]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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
                (~optional (~seq #:is-var is-var) #:defaults ([is-var #'#f]))) ...)
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
         ; Temporary fix: if-for-bool :(
         ; needed for now because when Forge expands into core,
         ; is-var comes in as "var" instead of #t
         ; so the contract on make-sig break
         (define isv
           (if is-var #t #f))
         (define name (make-sig true-name
                                #:one true-one
                                #:lone true-lone
                                #:abstract true-abstract
                                #:is-var isv
                                ;let #:in default to #f until it is implemented
                                #:extends true-parent
                                #:info (nodeinfo #,(build-source-location stx) check-lang)))
         ;make sure it isn't a var sig if not in temporal mode
         (~@ (check-temporal-for-var is-var true-name))
         ;Currently when lang/expander.rkt calls sig with #:in,
         ;super-sig is #'(raise "Extending with in not yet implemented.")
         ;This is just here for now to make sure that error is raised.
         (~? super-sig)
         (update-state! (state-add-sig curr-state true-name name true-parent-name))))]))

; Declare a new relation
; (relation name (sig1 sig2 sigs ...) [|| [#:is breaker] [#:is-var isv]])
(define-syntax (relation stx)
  (syntax-parse stx
    [(relation name:id (sig1:id sig2:id sigs ...)
               (~optional (~seq #:is breaker:id))
               (~optional (~seq #:is-var is-var) #:defaults ([is-var #'#f])))
     (quasisyntax/loc stx
       (begin
         (define true-name 'name)
         (define true-sigs (list (thunk (get-sig curr-state sig1))
                                 (thunk (get-sig curr-state sig2))
                                 (thunk (get-sig curr-state sigs)) ...))
         ;(printf "relatoin sigs: ~a~n" (list sig1 sig2 sigs ...))
         ; (define true-sigs (map (compose Sig-name ;;; Bugged since relation before sig in #lang forge
         ;                                 (curry get-sig curr-state ))
         ;                        (list sig1 sig2 sigs ...)))
         (define true-breaker (~? breaker #f))
         ;(printf "relatoin breaker: ~a~n" true-breaker)
         (define checker-hash (get-ast-checker-hash))
         (when (hash-has-key? checker-hash 'field-decl) ((hash-ref checker-hash 'field-decl) true-breaker))
         ; Temporary fix: if-for-bool :(
         ; needed for now because when Forge expands into core,
         ; is-var comes in as "var" instead of #t
         ; so the contract on make-sig breaks
         (define isv
           (if is-var #t #f))
         (define name (make-relation true-name
                                     true-sigs
                                     #:is true-breaker
                                     #:is-var isv
                                     #:info (nodeinfo #,(build-source-location stx) 'checklangNoCheck)))
         ;make sure it isn't a var sig if not in temporal mode
         (~@ (check-temporal-for-var is-var true-name))
         (update-state! (state-add-relation curr-state true-name name))))]
    ; Case: check-lang
    [(relation (#:lang check-lang) name:id (sig1:id sig2:id sigs ...)
               (~optional (~seq #:is breaker:id))
               (~optional (~seq #:is-var is-var) #:defaults ([is-var #'#f])))
     (quasisyntax/loc stx
       (begin
         (define true-name 'name)
         (define true-sigs (list (thunk (get-sig curr-state sig1))
                                 (thunk (get-sig curr-state sig2))
                                 (thunk (get-sig curr-state sigs)) ...))
         ;(printf "relatoin sigs: ~a~n" (list sig1 sig2 sigs ...))
         ; (define true-sigs (map (compose Sig-name ;;; Bugged since relation before sig in #lang forge
         ;                                 (curry get-sig curr-state ))
         ;                        (list sig1 sig2 sigs ...)))
         (define true-breaker (~? breaker #f))
         ;(printf "relatoin breaker: ~a~n" true-breaker)
         (define checker-hash (get-ast-checker-hash))
         (when (hash-has-key? checker-hash 'field-decl) ((hash-ref checker-hash 'field-decl) true-breaker))
         ; Temporary fix: if-for-bool :(
         ; needed for now because when Forge expands into core,
         ; is-var comes in as "var" instead of #t
         ; so the contract on make-sig breaks
         (define isv
           (if is-var #t #f))
         (define name (make-relation true-name
                                     true-sigs
                                     #:is true-breaker
                                     #:is-var isv
                                     #:info (nodeinfo #,(build-source-location stx) check-lang)))
         ;make sure it isn't a var sig if not in temporal mode
         (~@ (check-temporal-for-var is-var true-name))
         (update-state! (state-add-relation curr-state true-name name))))]))

; Declare a new predicate
; (pred info name cond ...)
; (pred info (name var ...) cond ...)
;   or same without info
(define-syntax (pred stx)
  (syntax-parse stx
    [(pred name:id conds:expr ...+)
     (quasisyntax/loc stx
       (begin
         ; use srcloc of actual predicate, not this location in sigs
         (define name (&&/info (nodeinfo #,(build-source-location stx) 'checklangNoCheck) conds ...))
         (update-state! (state-add-pred curr-state 'name name))))]
    [(pred (name:id args:id ...+) conds:expr ...+)
     (quasisyntax/loc stx
       (begin 
         (define (name args ...) (&&/info (nodeinfo #,(build-source-location stx) 'checklangNoCheck) conds ...))
         (update-state! (state-add-pred curr-state 'name name))))]

    ; Case: check-lang
    [(pred (#:lang check-lang) name:id conds:expr ...+)
     (quasisyntax/loc stx
       (begin
         ; use srcloc of actual predicate, not this location in sigs
         (define name (&&/info (nodeinfo #,(build-source-location stx) check-lang) conds ...))
         (update-state! (state-add-pred curr-state 'name name))))]
    [(pred (#:lang check-lang) (name:id args:id ...+) conds:expr ...+)
     (quasisyntax/loc stx
       (begin 
         (define (name args ...) (&&/info (nodeinfo #,(build-source-location stx) check-lang) conds ...))
         (update-state! (state-add-pred curr-state 'name name))))]))
                                   
; Declare a new function
; (fun (name var ...) result)
(define-syntax (fun stx)
  (syntax-parse stx
    [(fun (name:id args:id ...+) result:expr) 
      #'(begin
          (define (name args ...) result)
          (update-state! (state-add-fun curr-state 'name name)))]))

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
         (define name (make-inst (list binds ...)))
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
     #`(begin
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
           (~? (Target (cdr target-instance)
                       (~? 'target-distance 'close))
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
         (update-state! (state-add-runmap curr-state 'name name)))]))

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
       (syntax/loc stx 
         (cond 
          [(member 'expected '(sat unsat))
           (run name args ...)
           (define first-instance (tree:get-value (Run-result name)))
           (unless (equal? (if (Sat? first-instance) 'sat 'unsat) 'expected)
             (raise (format "Failed test ~a. Expected ~a, got ~a.~a"
                            'name 'expected (if (Sat? first-instance) 'sat 'unsat)
                            (if (Sat? first-instance)
                                (format " Found instance ~a" first-instance)
                                (if (Unsat-core first-instance)
                                    (format " Core: ~a" (Unsat-core first-instance))
                                    "")))))
           (close-run name)]

          [(equal? 'expected 'theorem)
           (check name args ...)
           (define first-instance (tree:get-value (Run-result name)))
           (when (Sat? first-instance)
             (raise (format "Theorem ~a failed. Found instance:~n~a"
                            'name first-instance)))
           (close-run name)]

          [else (raise (format "Illegal argument to test. Received ~a, expected sat, unsat, or theorem."
                               'expected))])))]))

(define-syntax (example stx)  
  (syntax-parse stx
    [(_ name:id pred bounds ...)
     (add-to-execs
       (syntax/loc stx (begin
         (run name #:preds [pred] #:bounds [bounds ...])
         (define first-instance (tree:get-value (Run-result name)))
         (when (Unsat? first-instance)
           (run double-check #:preds [] #:bounds [bounds ...])
           (define double-check-instance (tree:get-value (Run-result double-check)))
           (if (Sat? double-check-instance)
               (raise-user-error (format "Invalid example '~a'; the instance specified does not satisfy the given predicate." 'name))
               (raise-user-error (format (string-append "Invalid example '~a'; the instance specified is impossible. "
                                             "This means that the specified bounds conflict with each other "
                                             "or with the sig/relation definitions.")
                              'name)))))))]))

; Checks that some predicates are always true.
; (check name
;        #:preds [(pred ...)]
;        [#:scope [((sig [lower 0] upper) ...)]]
;        [#:bounds [bound ...]]))
(define-syntax (check stx)
  (syntax-parse stx
    [(check name:id
            (~alt
              (~optional (~seq #:preds (pred ...)))
              (~optional (~seq #:scope ((sig:id (~optional lower:nat #:defaults ([lower #'0])) upper:nat) ...)))
              (~optional (~seq #:bounds (bound ...)))) ...)
     (syntax/loc stx
       (run name (~? (~@ #:preds [(! (&& pred ...))]))
                 (~? (~@ #:scope ([sig lower upper] ...)))
                 (~? (~@ #:bounds (bound ...)))))]))


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
; Creates a thunk which generates a new model on each call.
(define (make-model-generator model-lazy-tree [mode 'next])
  (thunk
    (define ret (tree:get-value model-lazy-tree))
    (set! model-lazy-tree (tree:get-child model-lazy-tree mode))
    ret))

; ; make-model-evaluator :: Run -> (String -> ???)
; ; Creates an evaluator function for a given Run. 
; ; Executes on the most recently generated instance.
; (define (make-model-evaluator run)
;   (lambda (command)
;     (define name (substring command 1 3))
;     (cmd [(stdin)] 
;       (print-cmd command)
;       (print-cmd "(evaluate ~a)" name)
;       (print-eof))
;     (define result (read (stdout)))
;     result))
;     ; (define u (read (open-input-string command)))
;     ; (println u)
;     ; u))

(provide (prefix-out forge: nsa))
(define nsa (make-parameter #f))
; display :: Run -> void
; Lifted function which, when provided a Run,
; generates a Sterling instance for it.
(define (true-display arg1 [arg2 #f])
  (if (@not (Run? arg1))
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
                    [else (raise-user-error "Could not evaluate in current language - must be surface or core.")]))

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
          (define-values (run-result atom-rels server-ports kodkod-currents kodkod-bounds) (send-to-kodkod contrast-run-spec))
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
(define (set-bitwidth scope n)
  (struct-copy Scope scope
               [bitwidth n]))

; update-int-bound :: Scope, node/expr/relation, Range -> Scope
; Updates the scope (range) for a given sig in scope.
(define (update-int-bound scope rel given-scope)
  (define old-sig-scopes (Scope-sig-scopes scope))
  (define name (string->symbol (relation-name rel)))
  (define-values (new-lower new-upper)
    (if (@not (hash-has-key? old-sig-scopes name))
        (values (Range-lower given-scope) (Range-upper given-scope))
        (let ([old-scope (hash-ref old-sig-scopes name)])
          (let ([old-lower (Range-lower old-scope)]
                [old-upper (Range-upper old-scope)]
                [given-lower (Range-lower given-scope)]
                [given-upper (Range-upper given-scope)])

            (define new-lower 
              (cond 
                [(and old-lower given-lower) (@max old-lower given-lower)]
                [old-lower old-lower]
                [given-lower given-lower]
                [else #f]))
            (define new-upper 
              (cond 
                [(and old-upper given-upper) (@min old-upper given-upper)]
                [old-upper old-upper]
                [given-upper given-upper]
                [else #f]))
            (values new-lower new-upper)))))

  
  (when (@< new-upper new-lower)
    (raise (format (string-append "Bound conflict: numeric upper bound on ~a was"
                                  " less than numeric lower bound (~a vs. ~a).") 
                   rel new-upper new-lower)))

  (define new-scope (Range new-lower new-upper))
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
    (raise (format "Bound conflict: upper bound on ~a was not a superset of lower bound. Lower=~a; Upper=~a." rel lower upper)))

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

#|
Originally needed for inst,
Now with functional forge, do-bind is used instead
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
     #`(let* ([exact (eval-int-expr 'n (Bound-tbindings #,bound) 8)]
              [new-scope (if (equal? (relation-name rel) "Int")
                             (set-bitwidth #,scope exact)
                             (update-int-bound #,scope rel (Range exact exact)))])
          (values new-scope #,bound))]

    [(<= (card rel) upper)
     #`(let* ([upper-val (eval-int-expr 'upper (Bound-tbindings #,bound) 8)]
              [new-scope (update-int-bound #,scope rel (Range #f upper-val))])
         (values new-scope #,bound))]

    [(<= lower (card rel))
     #`(let* ([lower-val (eval-int-expr 'lower (Bound-tbindings #,bound) 8)]
              [new-scope (update-int-bound #,scope rel (Range lower-val #f))])
         (values new-scope #,bound))]

    [(<= lower (card rel) upper)
     #`(let* ([lower-val (eval-int-expr 'lower (Bound-tbindings #,bound) 8)]
              [upper-val (eval-int-expr 'upper (Bound-tbindings #,bound) 8)]
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
     #:fail-unless (member (syntax->datum #'cmp) '(= in ni)) "expected a comparator"
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

    [x (raise-syntax-error 'inst (format "Not allowed in bounds constraint") binding)]))
|#

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
    (with-syntax ([opName/func (format-id #'opName "~a/func" #'opName)])
      (syntax/loc stx (begin
        (define-syntax (opName stxx)
          (syntax-parse stxx
           [opName
            (quasisyntax/loc stxx (lambda (args ...)
              (opName/func (nodeinfo #,(build-source-location stxx) 'checklangNoCheck) args ...)))]))

        (define (opName/func locArg args ...)
          body)
      )))
    ]))

(define-builtin (isSeqOf info r1 d)
  (&&/info info
      (in/info info r1 (-> Int univ))
      (in/info info (join/info info Int r1) d)
      (all ([i1 (join/info info r1 univ)])
           (&&/info info (>= (sum/info info i1) (int 0))
               (lone (join/info info i1 r1))))
      (all ([e (join/info info Int r1)])
           (some (join/info info r1 e)))
      (all ([i1 (join/info info r1 univ)])
           (implies (!= i1 (sing/info info (int 0)))
                    (some (join/info info
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

(define-builtin (seqRest info r)
  (join/info info succ r))

(define-builtin (indsOf info r e)
  (join/info info r e))

(define-builtin (idxOf info r e)
  (join/info info r e))

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

; a reachable from b through r1 + r2 + ...
(define-syntax (reachable stx)
  (syntax-parse stx
   [reachable
   (quasisyntax/loc stx
     (lambda (a b . r) (reachablefun #,(build-source-location stx) a b r)))]))

(define (reachablefun loc a b r)
  (in/info (nodeinfo loc 'checklangNoCheck) 
           a 
           (join/info (nodeinfo loc 'checklangNoCheck) 
                      b 
                      (^/info (nodeinfo loc 'checklangNoCheck) (union-relations loc r)))))

(define (union-relations loc r)
  (cond
    [(empty? r) (raise-user-error "contact course staff. Shouldn't have union of none")]
    [(empty? (rest r)) (first r)]
    [else (+/info (nodeinfo loc 'checklangNoCheck) (first r) (union-relations loc (rest r)))]))
  ; (in a (join b (^ r))))
; (define (reachable a b r)
;   (reachable2 a b r))
; (define-syntax (reachable2 stx)
; (println stx)
;   (syntax-case stx ()
;     [(_ a b r ...)
;       (quasisyntax/loc stx
;         (in/info (nodeinfo #,(build-source-location stx) 'checklangNoCheck)
;             a 
;             (join/info (nodeinfo #,(build-source-location stx) 'checklangNoCheck)
;                         b 
;                        (^/info (nodeinfo #,(build-source-location stx) 'checklangNoCheck) (+/info (nodeinfo #,(build-source-location stx) 'checklangNoCheck) r ...)))))]))

