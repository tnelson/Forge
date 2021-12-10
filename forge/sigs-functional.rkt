#lang racket

(require racket/contract)
(require (prefix-in @ racket) 
         (prefix-in @ racket/set))
(require syntax/parse/define)
(require racket/match)
(require (for-syntax racket/match syntax/srcloc))
(require syntax/srcloc)

(require "shared.rkt")
(require (prefix-in ast: "lang/ast.rkt")
         (prefix-in ast: (only-in "sigs-structs.rkt" implies iff <=> ifte >= <= ni != !in !ni))
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
         "send-to-kodkod.rkt")
(require (only-in "lang/alloy-syntax/parser.rkt" [parse forge-lang:parse])
         (only-in "lang/alloy-syntax/tokenizer.rkt" [make-tokenizer forge-lang:make-tokenizer]))
(require (prefix-in tree: "lazy-tree.rkt"))

; Commands
(provide make-sig make-relation make-inst)
(provide run-from-state
         make-run
         check-from-state
         make-check
         test-from-state
         make-test
         display)
(provide Int succ)
(provide (prefix-out forge: make-model-generator))
(provide solution-diff)
         ;instance-diff evaluate)

; ; Instance analysis functions
; (provide is-sat? is-unsat?)

; ; export AST macros and struct definitions (for matching)
; ; Make sure that nothing is double-provided
(require (except-in "lang/ast.rkt" ->))
(provide (rename-out [ast:-> ->]))
(provide (all-from-out "lang/ast.rkt"))

; ; Racket stuff
(provide let quote)

; ; Technical stuff
; (provide set-verbosity VERBOSITY_LOW VERBOSITY_HIGH)
; (provide set-path!)
(provide set-option!)
; (define (set-path! path) #f)

; ; Data structures
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

(provide (prefix-out forge: (all-from-out "sigs-structs.rkt")))
; ; Export these from structs without forge: prefix
(provide implies iff <=> ifte >= <= ni != !in !ni min max)

; Let forge/core work with the model tree without having to require helpers
; Don't prefix with tree:, that's already been done when importing
(provide (all-from-out "lazy-tree.rkt"))

; ; Export everything for doing scripting
; (provide (prefix-out forge: (all-defined-out)))
; (provide (prefix-out forge: (struct-out bound)))
; (provide (prefix-out forge: relation-name))

; (provide (struct-out Sat)
;          (struct-out Unsat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language-Specific Checks ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require forge/choose-lang-specific)

; TODO: Question: This part was not commented out; was there a reason to set forge-checker-hash?

; (require forge/lang/lang-specific-checks) ; TODO: can this be relative?
; ; ANSWER: maybe using dynamic-require
; ;(printf "ast-ch = ~a~n" (get-ast-checker-hash))
; (set-checker-hash! forge-checker-hash)
; (set-ast-checker-hash! forge-ast-checker-hash)
; ;(printf "ast-ch = ~a~n" (get-ast-checker-hash))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; State Updaters  ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-option! option value)
  (cond [(or (equal? option 'verbosity)
             (equal? option 'verbose))
         (set-verbosity value)]
        [else (raise (format "Can't set ~a in functional mode." option))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Forge Commands  ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; check-temporal-for-var :: Boolean String -> void
; raises an error if is-var is true and the problem_type option is 'temporal
; uses the given name in the error message
; meant to only allow var sigs and relations in temporal specs
; (define (check-temporal-for-var is-var name)
;   (cond
;     [(and is-var (not (equal? (get-option curr-state 'problem_type) 'temporal)))
;      (raise-user-error (format "Can't have var ~a unless problem_type option is temporal"
;                                name))]))


(define/contract (make-sig [raw-name #f]
                           #:one [one #f]
                           #:abstract [abstract #f]
                           #:is-var [is-var #f]
                           #:in [in #f]
                           #:extends [extends #f]
                           #:info [node-info empty-nodeinfo])
  (->* ()
       (symbol?
        #:one boolean?
        #:abstract boolean?
        #:is-var boolean?
        #:in (or/c Sig? #f)
        #:extends (or/c Sig? #f)
        #:info (or/c nodeinfo? #f))
       Sig?)
  ; (check-temporal-for-var is-var name)
  (define name (or raw-name (gensym 'sig)))

  ;(define source-info-loc (nodeinfo-loc node-info))
  ;(printf "SIG NAME: ~a.~n" name)
  ;(printf "SIG SOURCE LINE: ~a.~n" (source-location-line source-info-loc))
  ;(printf "SIG SOURCE COLUMN: ~a.~n" (source-location-column source-info-loc))
  ;(printf "SIG SOURCE SPAN: ~a.~n" (source-location-span source-info-loc))

  (Sig node-info ; info
        
       1 ; arity
       
       (symbol->string name) ; name

       ;see make-relation to see why this is a thunk
       (thunk (list (symbol->string name))) ; typelist 

       (if extends (symbol->string (Sig-name extends)) "univ") ; parent 
       is-var ; is-variable

       name
       one
       abstract
       extends))

(define/contract (make-relation name/sigs 
                                [raw-sigs #f] 
                                #:is [breaker #f] 
                                #:is-var [is-var #f] 
                                #:info [node-info empty-nodeinfo])
  (->* ((or/c symbol? (non-empty-listof Sig?)))
       ((non-empty-listof (or/c Sig? (-> Sig?)))
        #:is (or/c node/breaking/break? #f)
        #:is-var boolean?
        #:info (or/c nodeinfo? #f))
       Relation?)

  (define-values (name sigs)
    (if raw-sigs
        (values name/sigs raw-sigs)
        (values (gensym 'relation) name/sigs)))

  ;(define source-info-loc (nodeinfo-loc node-info))
  ;(printf "RELATION NAME: ~a.~n" name)
  ;(printf "RELATION SOURCE LINE: ~a.~n" (source-location-line source-info-loc))
  ;(printf "RELATION SOURCE COLUMN: ~a.~n" (source-location-column source-info-loc))
  ;(printf "RELATION SOURCE SPAN: ~a.~n" (source-location-span source-info-loc))

  ; sigs can contain sigs or thunks which return sigs
  ; in order to allow mutual references between sigs in forge surface
  ; while still being convenient for scripting in forge/core
  ; this makes sure every element of the sigs list is a thunk
  ; in order to ensure type consistency
  (define sigs-thunks
    (map (lambda (sig-or-thunk)
           (if (Sig? sig-or-thunk)
               (thunk sig-or-thunk)
               sig-or-thunk))
         sigs))

  ; (check-temporal-for-var is-var name)
  (Relation node-info ; info
        
            (length sigs-thunks) ; arity
            
            (symbol->string name) ; name

            ; this needs to be a thunk because in order to use Sig-name
            ; you need to execute the thunk which returns a Sig, and
            ; we can't guarantee all Sigs that are used are bound yet
            ; (in mutual references from forge surface,
            ; they won't all be bound yet)
            (thunk (map (compose symbol->string
                                 Sig-name
                                 (lambda (s) (s)))
                        sigs-thunks)) ; typelist-thunk

            ; In forge surface, the parent sig will always be defined
            ; before the relation is created, and it is possible to do that
            ; when scripting as well, so we can assume ((first sigs))
            ; is always bound here, which means this doesn't need to be a thunk
            (symbol->string (Sig-name ((first sigs-thunks)))) ; parent 
            is-var ; is-variable

            name
            sigs-thunks
            breaker))

(define/contract (do-bind bind scope bound)
  (-> (or/c ast:node/formula? ast:node/breaking/op? Inst?)
      Scope?
      Bound?
      (values Scope? Bound?))

  (define (fail [cond #f])
    (unless cond
      (raise (format "Invalid bind: ~a" bind))))
  (define inst-checker-hash (get-inst-checker-hash))
  ;(when (hash-has-key? checker-hash 'field-decl) ((hash-ref checker-hash 'field-decl) true-breaker))
  (define (inst-check formula to-handle)
    (when (hash-has-key? inst-checker-hash to-handle) ((hash-ref inst-checker-hash to-handle) formula)))
  (match bind
    ; no rel, one rel, two rel, lone rel, some rel
    [(ast:node/formula/multiplicity info mult rel)
     ; is it safe to use the info from above here?
     (let ([rel-card (ast:node/int/op/card info (list rel))])
       (do-bind
        (match mult
          ['no (ast:node/formula/op/= info (list rel none))]
          ['one (ast:node/formula/op/int= info (list rel-card 1))]
          ['two (ast:node/formula/op/int= info (list rel-card 2))]
          ['lone
           (ast:node/formula/op/|| info
                                   (list (ast:node/formula/op/int< info (list rel-card 1))
                                         (ast:node/formula/op/int= info (list rel-card 1))))]
          ; Why was some not in original sigs.rkt?? Does it need new tests?
          #;['some
             (ast:node/formula/op/|| info
                                     (list (ast:node/formula/op/int= info (list rel-card 1))
                                           (ast:node/formula/op/int> info (list rel-card 1))))])
        scope
        bound))]
    ; (= (card rel) n)
    [(ast:node/formula/op/int= eq-info (list left right))
     (match left
       [(ast:node/int/op/card c-info (list left-rel))
        (let* ([exact (eval-int-expr right (Bound-tbindings bound) 8)]
               ; do we need the if (equal? (relation-name rel) "Int")
               ; case that sigs.rkt has?
               [new-scope (update-int-bound scope left-rel (Range exact exact))])
          (values new-scope bound))]
       [_ (fail)])]

    ; (<= (card rel) upper)
    [(ast:node/formula/op/|| or-info
                             (list (ast:node/formula/op/int< lt-info (list lt-left lt-right))
                                   (ast:node/formula/op/int= eq-info (list eq-left eq-right))))
     (unless (@and (equal? lt-left eq-left) (equal? lt-right eq-right))
       (fail))
     (match lt-left
       [(ast:node/int/op/card c-info (list left-rel))
        (let* ([upper-val (eval-int-expr lt-right (Bound-tbindings bound) 8)]
               [new-scope (update-int-bound scope left-rel (Range 0 upper-val))])
          (values new-scope bound))]
       [_ (fail)])]

    ; (<= lower (card-rel))
    [(ast:node/formula/op/|| or-info
                             (list (ast:node/formula/op/int< lt-info (list lt-left lt-right))
                                   (ast:node/formula/op/int= eq-info (list eq-left eq-right))))
     (unless (@and (equal? lt-left eq-left) (equal? lt-right eq-right))
       (fail))
     (match lt-right
       [(ast:node/int/op/card c-info (list right-rel))
        (let* ([lower-val (eval-int-expr lt-left (Bound-tbindings bound) 8)]
               [new-scope (update-int-bound scope right-rel (Range lower-val 0))])
          (values new-scope bound))]
       [_ (fail)])]

    ; (<= lower (card rel) upper)
    ; Ask Tim is (<= a b c) equivalent to (and (<= a b) (<= a c))?
    ; so then in the ast would this be
    ; (node/formula/op/&& and-info
    ;   (list (node/formula/op/|| or1-info
    ;           (list (node/formula/op/int< lt1-info (list a b))
    ;                 (node/formula/op/int= eq1-info (list a b))))
    ;         (node/formula/op/|| or2-info
    ;           (list (node/formula/op/int< lt2-info (list b c))
    ;                 (node/formula/op/int= eq2-info (list b c))))))

    ; need this in order to use some
    ; [(node/formula/op/|| (node/formula/op/int> left1 right1)  ; TODO: Add lower bounds
    ;                      (node/formula/op/int= left2 right2))
    ;   (unless (@and (equal? left left) (equal? right1 right2))
    ;     (fail))
    ;   (match left
    ;     [(node/int/op/card info left-rel)
    ;       (let* ([upper-val (eval-int-expr right (Bound-tbindings bound) 8)]
    ;              [new-scope (update-int-bound scope rel (Range 0 upper-val))])
    ;         (values new-scope bound))]
    ;     [_ (fail)])]

    ; Strategies
    [(ast:node/breaking/op/is info (list left right))
     (define breaker
       (match right
         [(ast:node/breaking/break _ breaker) breaker]
         [_ (fail)]))
     (match left
       [(? ast:node/expr/relation?) (break left right)]
       [(ast:node/expr/op/~ info arity (list left-rel))
        (break left-rel (get-co right))]
       [_ (fail)])
     ; hopefully the above calls to break update these somehow
     ; and hopefully they don't rely on state :(
     (values scope bound)]

    ; Other instances
    [(Inst func) (func scope bound)]

    ; rel = expr
    [(ast:node/formula/op/= info (list left right))
     (unless (ast:node/expr/relation? left)
       (fail))
     (let ([tups (eval-exp right (Bound-tbindings bound) 8 #f)])
       (define new-scope scope)
       (define new-bound (update-bindings bound left tups tups))
       (values new-scope new-bound))]

    ; rel in expr
    ; expr in rel
    [(ast:node/formula/op/in info (list left right))
     (inst-check bind ast:node/formula/op/in)
     (cond
       [(ast:node/expr/relation? left)
        (let ([tups (eval-exp right (Bound-tbindings bound) 8 #f)])
          (define new-bound (update-bindings bound left (@set) tups))
          (values scope new-bound))]
       [(ast:node/expr/relation? right)
        (let ([tups (eval-exp left (Bound-tbindings bound) 8 #f)])
          (define new-bound (update-bindings bound right tups))
          (values scope new-bound))]
       [else (fail)])]

    ; original sigs.rkt has (cmp (join foc rel) expr) commented out here

    ; Bitwidth
    ; what does (Int n:nat) look like in the AST?

    [_ (fail)]))

(define/contract (make-inst binds)
  (-> (listof (or/c ast:node/formula? ast:node/breaking/op? Inst?))
      Inst?)

  (define (inst-func scope bound)
    (for/fold ([scope scope] [bound bound])
              ([bind binds])
      (do-bind bind scope bound)))

  (Inst inst-func))

(define (update-bitwidth base-scope bitwidth)
  (-> Scope? nonnegative-integer?
      Scope?)
  (struct-copy Scope base-scope
               [bitwidth bitwidth]))

(define (update-scope base-scope sig lower upper)
  (-> Scope? Sig? nonnegative-integer? nonnegative-integer?
      Scope?)
  (define old-sig-scopes (Scope-sig-scopes base-scope))

  (match-define (Range old-lower old-upper) (hash-ref old-sig-scopes (Sig-name sig) (Range lower upper)))
  (define new-sig-scope (Range (@max old-lower lower) (@min old-upper upper)))

  (define new-sig-scopes (hash-set old-sig-scopes (Sig-name sig) new-sig-scope))
  (struct-copy Scope base-scope
               [sig-scopes new-sig-scopes]))

; used by make-run and make-check and make-test
; so that they can call run-from-state and check-from-state and test-from-state
(define/contract (make-state-for-run #:sigs [sigs-input (list)]
                                     #:relations [relations-input (list)]
                                     #:options [options-input #f])
  (->* () 
       (#:sigs (listof Sig?)
        #:relations (listof Relation?)
        #:options (or/c Options? #f))
       State?)

  (define sigs-with-Int (append sigs-input (list Int)))
  (define sigs
    (for/hash ([sig sigs-with-Int])
      (values (Sig-name sig) sig)))
  (define sig-order (map Sig-name sigs-with-Int))

  (define relations-with-succ (append relations-input (list succ)))
  (define relations
    (for/hash ([relation relations-with-succ])
      (values (Relation-name relation) relation)))
  (define relation-order (map Relation-name relations-with-succ))

  (define pred-map (hash))
  (define fun-map (hash))
  (define const-map (hash))
  (define inst-map (hash))
  (define options (or options-input DEFAULT-OPTIONS))
  (define run-map (hash))

  (State sigs
         sig-order
         relations
         relation-order
         pred-map
         fun-map
         const-map
         inst-map
         options
         run-map))

(define (run-from-state state #:name [name 'unnamed-run]
                        #:preds [preds (list)]
                        #:scope [scope-input (list)]
                        #:bounds [bounds-input (make-inst (list))] 
                        #:solver [solver #f]
                        #:backend [backend #f]
                        #:target [target #f]
                        #:command [command #'(run a b c)])
  (->* (State?)
       (#:name symbol?
        #:preds (listof ast:node/formula)
        #:scope (or/c Scope? (listof (or/c (list/c Sig? nonnegative-integer?)
                                           (list/c Sig? nonnegative-integer? nonnegative-integer?))))
        #:bounds (or/c Inst? (listof (or/c Inst? ast:node/formula)))
        #:solver (or/c symbol? #f)
        #:backend (or/c symbol? #f)
        #:target (or/c Target? #f)
        #:command syntax?)
       Run?)

  (define/contract base-scope Scope?
    (cond
      [(Scope? scope-input) scope-input]
      [(list? scope-input)
       (for/fold ([scope (Scope #f #f (hash))])
                 ([triple scope-input])
         (match triple
           [(list sig upper)
            (if (equal? sig Int)
                (update-bitwidth scope upper)
                (update-scope scope sig 0 upper))]
           [(list sig lower upper)
            (if (equal? sig Int)
                (update-bitwidth scope upper)
                (update-scope scope sig lower upper))]
           [_ (raise (format "Invalid scope: ~a" triple))]))]))

  (define/contract scope-with-ones Scope?
    (for/fold ([scope base-scope])
              ([sig (get-sigs state)])
      (if (Sig-one sig)
          (update-scope scope sig 1 1)
          scope)))

  (define/contract default-bounds Bound?
    (let* ([bitwidth (Scope-bitwidth scope-with-ones)]
           [max-int (expt 2 (sub1 (or bitwidth DEFAULT-BITWIDTH)))]
           [ints (map int-atom (range (- max-int) max-int))]
           [succs (map list (reverse (rest (reverse ints)))
                       (rest ints))])
      (Bound (hash)
             (hash Int (map list ints)
                   succ succs))))

  (define/contract wrapped-bounds-inst Inst?
    (if (Inst? bounds-input)
        bounds-input
        (make-inst bounds-input)))

  (define-values (scope bounds) 
    ((Inst-func wrapped-bounds-inst) scope-with-ones default-bounds))

  (define spec (Run-spec state preds scope bounds target))        
  (define-values (result atoms server-ports kodkod-currents kodkod-bounds) (send-to-kodkod spec command))
  
  (Run name command spec result server-ports atoms kodkod-currents kodkod-bounds))


(define/contract (make-run #:name [name 'unnamed-run]
                           #:preds [preds (list)]
                           #:scope [scope-input (list)]
                           #:bounds [bounds-input (list)]
                           #:target [target #f]
                           #:sigs [sigs-input (list)]
                           #:relations [relations-input (list)]
                           #:options [options-input #f])
  (->* () 
       (#:name symbol?
        #:preds (listof ast:node/formula)
        #:scope (or/c Scope? (listof (or/c (list/c Sig? nonnegative-integer?)
                                           (list/c Sig? nonnegative-integer? nonnegative-integer?))))
        #:bounds (or/c Inst? (listof (or/c Inst? ast:node/formula)))
        #:target (or/c Target? #f)
        #:sigs (listof Sig?)
        #:relations (listof Relation?)
        #:options (or/c Options? #f))
       Run?)

  (define state (make-state-for-run #:sigs sigs-input
                                    #:relations relations-input
                                    #:options options-input))

  ;what about the other arguments to run-from-state?
  (run-from-state state 
                  #:name name
                  #:preds preds
                  #:scope scope-input
                  #:bounds bounds-input))

(define/contract (check-from-state state
                                   #:name [name 'unnamed-check]
                                   #:preds [preds (list)]
                                   #:scope [scope-input (list)]
                                   #:bounds [bounds-input (make-inst (list))] 
                                   #:solver [solver #f]
                                   #:backend [backend #f]
                                   #:target [target #f]
                                   #:command [command #'(run a b c)])
  (->* (State?) ;todo: try this with State? instead of (State?)
       (#:name symbol?
        #:preds (listof ast:node/formula)
        #:scope (or/c Scope? (listof (or/c (list/c Sig? nonnegative-integer?)
                                           (list/c Sig? nonnegative-integer? nonnegative-integer?))))
        #:bounds (or/c Inst? (listof (or/c Inst? ast:node/formula)))
        #:solver (or/c symbol? #f)
        #:backend (or/c symbol? #f)
        #:target (or/c Target? #f)
        #:command syntax?)
       Run?)
  ;; FIX THIS TO TRACK SOURCE LOCATION
  (let ([new-preds (list (! (apply &&/func preds)))])
    (run-from-state state
                    #:name name
                    #:preds new-preds
                    #:scope scope-input
                    #:bounds bounds-input
                    #:solver solver
                    #:backend backend
                    #:target target
                    #:command command)))

(define/contract (make-check #:name [name 'unnamed-run]
                             #:preds [preds (list)]
                             #:scope [scope-input (list)]
                             #:bounds [bounds-input (list)]
                             #:target [target #f]
                             #:sigs [sigs-input (list)]
                             #:relations [relations-input (list)]
                             #:options [options-input #f])
  (->* () 
       (#:name symbol?
        #:preds (listof ast:node/formula)
        #:scope (or/c Scope? (listof (or/c (list/c Sig? nonnegative-integer?)
                                           (list/c Sig? nonnegative-integer? nonnegative-integer?))))
        #:bounds (or/c Inst? (listof (or/c Inst? ast:node/formula)))
        #:target (or/c Target? #f)
        #:sigs (listof Sig?)
        #:relations (listof Relation?)
        #:options (or/c Options? #f))
       Run?)
  (let ([state (make-state-for-run #:sigs sigs-input
                                   #:relations relations-input
                                   #:options options-input)])
    ;what about the other arguments to check-from-state?
    (check-from-state state 
                      #:name name
                      #:preds preds
                      #:scope scope-input
                      #:bounds bounds-input)))

(define/contract (test-from-state state
                                  #:expect expected
                                  #:name [name 'unnamed-check]
                                  #:preds [preds (list)]
                                  #:scope [scope-input (list)]
                                  #:bounds [bounds-input (make-inst (list))] 
                                  #:solver [solver #f]
                                  #:backend [backend #f]
                                  #:target [target #f]
                                  #:command [command #'(run a b c)])
  ; TODO: make #:expect only accept 'sat or 'unsat or 'theorem
  ; through its contract, not just by throwing an error
  (->* (State?
        #:expect symbol?)
       (#:name symbol?
        #:preds (listof ast:node/formula)
        #:scope (or/c Scope? (listof (or/c (list/c Sig? nonnegative-integer?)
                                           (list/c Sig? nonnegative-integer? nonnegative-integer?))))
        #:bounds (or/c Inst? (listof (or/c Inst? ast:node/formula)))
        #:solver (or/c symbol? #f)
        #:backend (or/c symbol? #f)
        #:target (or/c Target? #f)
        #:command syntax?)
       void?)
  (cond
    [(equal? expected 'theorem)
     (let* ([test-check (check-from-state state
                                          #:name name
                                          #:preds preds
                                          #:scope scope-input
                                          #:bounds bounds-input
                                          #:solver solver
                                          #:backend backend
                                          #:target target
                                          #:command command)]
            [first-instance (tree:get-value (Run-result test-check))])
       (if (Sat? first-instance)
           (raise (format "Theorem ~a failed. Found instance:~n~a"
                          name first-instance))
           (close-run test-check)))]
    [(or (equal? expected 'sat) (equal? expected 'unsat))
     (let* ([test-run (run-from-state state
                                      #:name name
                                      #:preds preds
                                      #:scope scope-input
                                      #:bounds bounds-input
                                      #:solver solver
                                      #:backend backend
                                      #:target target
                                      #:command command)]
            [first-instance (tree:get-value (Run-result test-run))])
       (if (equal? (if (Sat? first-instance) 'sat 'unsat) expected)
           (close-run test-run)
           (raise (format "Failed test ~a. Expected ~a, got ~a.~a"
                          name expected (if (Sat? first-instance) 'sat 'unsat)
                          (if (Sat? first-instance)
                              (format " Found instance ~a" first-instance)
                              (if (Unsat-core first-instance)
                                  (format " Core: ~a" (Unsat-core first-instance))
                                  ""))))))]
    [else (raise (format "Illegal argument to test. Received ~a, expected sat, unsat, or theorem."
                         expected))]))

; Creates a new run to use for a test, then calls test-from-run
; to execute the test
(define/contract (make-test #:expect expected
                            #:name [name 'unamed-test]
                            #:preds [preds (list)]
                            #:scope [scope-input (list)]
                            #:bounds [bounds-input (list)]
                            #:target [target #f]
                            #:sigs [sigs-input (list)]
                            #:relations [relations-input (list)]
                            #:options [options-input #f])
  ; TODO: make #:expect only accept 'sat or 'unsat or 'theorem
  ; through its contract, not just by throwing an error
  (->* (#:expect symbol?)
       (#:name symbol?
        #:preds (listof ast:node/formula)
        #:scope (or/c Scope? (listof (or/c (list/c Sig? nonnegative-integer?)
                                           (list/c Sig? nonnegative-integer? nonnegative-integer?))))
        #:bounds (or/c Inst? (listof (or/c Inst? ast:node/formula)))
        #:target (or/c Target? #f)
        #:sigs (listof Sig?)
        #:relations (listof Relation?)
        #:options (or/c Options? #f))
       void?)
  (let ([state (make-state-for-run #:sigs sigs-input
                                   #:relations relations-input
                                   #:options options-input)])
    ;what about the other arguments to test-from-state?
    (test-from-state state
                     #:name name
                     #:preds preds
                     #:scope scope-input
                     #:bounds bounds-input
                     #:expect expected)))

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
(define (display arg1 [arg2 #f])
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
              (with-handlers ([(lambda (x) #t) (lambda (exn) 
                                                 (read-syntax 'Evaluator pipe1))])
                (forge-lang:parse "/no-name" (forge-lang:make-tokenizer pipe2))))

            ; Evaluate command
            (define full-command (datum->syntax #f `(let
                                                        ,(for/list ([atom (Run-atoms run)]
                                                                    #:when (symbol? atom))
                                                           `[,atom (atom ',atom)])
                                                      ,expr)))
            
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
                (list (ast:! (foldr (lambda (a b) (ast:&& a b))
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
          (make-model-generator (get-result contrast-run)))

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

(define (solution-diff s1 s2)
  (map instance-diff (Sat-instances s1) (Sat-instances s2)))


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
  (define name (string->symbol (ast:relation-name rel)))
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
        (hash-set old-tbindings rel (set->list lower))
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

; state-set-option :: State, Symbol, Symbol -> State
; Sets option to value for state.
(define (state-set-option state option value)
  (define options (State-options state))

  (define option-types
    (hash 'solver (lambda (x) (or (symbol? x) (string? x))) ; allow for custom solver path
          'backend symbol?
          ; 'verbosity exact-nonnegative-integer?
          'sb exact-nonnegative-integer?
          'coregranularity exact-nonnegative-integer?
          'logtranslation exact-nonnegative-integer?
          'min_tracelength exact-positive-integer?
          'max_tracelength exact-positive-integer?
          'problem_type symbol?
          'target_mode symbol?
          'core_minimization symbol?
          'skolem_depth exact-integer?
          'local_necessity symbol?))
  (unless ((hash-ref option-types option) value)
    (raise-user-error (format "Setting option ~a requires ~a; received ~a"
                              option (hash-ref option-types option) value)))

  (define new-options
    (cond
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
                    [skolem_depth value])]))

  (struct-copy State state
               [options new-options]))
