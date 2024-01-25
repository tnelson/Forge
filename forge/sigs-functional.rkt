#lang racket/base

; Functional interface to the Forge library and solver. 
;   Formula/expression macros should largely be kept in sigs.rkt instead. 
;   The design intent is: forge -> forge/core (in sigs.rkt) -> functional forge (this module)

;; TODO: there is still some duplicate logic (+ importing?) between this module + sigs, and possibly
;;   still unused imports...

(require racket/contract)
(require racket/match)
(require (prefix-in @ (only-in racket max min - display set))
         (only-in racket/function thunk)
         (only-in racket/math nonnegative-integer?)
         (only-in racket/list first second range rest empty flatten)
         (only-in racket/set list->set set->list set-union set-intersect subset? set-count))
(require (only-in syntax/srcloc build-source-location-syntax))

(require (except-in forge/lang/ast ->)
         (rename-in forge/lang/ast [-> ast:->]) ; don't clash with define/contract
         (only-in forge/sigs-structs implies iff <=> ifte int>= int<= ni != !in !ni)
         forge/breaks)
(require (only-in forge/lang/reader [read-syntax read-surface-syntax]))
(require forge/server/eval-model)
(require forge/server/forgeserver) ; v long
(require forge/shared
         forge/sigs-structs
         forge/evaluator
         forge/send-to-kodkod)

(require (only-in forge/lang/alloy-syntax/parser [parse forge-lang:parse])
         (only-in forge/lang/alloy-syntax/tokenizer [make-tokenizer forge-lang:make-tokenizer]))
(require (prefix-in tree: forge/lazy-tree))

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

; ; Instance analysis functions
; (provide is-sat? is-unsat?)

; ; export AST macros and struct definitions (for matching)
; ; Make sure that nothing is double-provided
(provide (rename-out [ast:-> ->]))
(provide (all-from-out forge/lang/ast))

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

(provide (all-from-out forge/sigs-structs))
; ; Export these from structs without forge: prefix
(provide implies iff <=> ifte int>= int<= ni != !in !ni min max)

; Let forge/core work with the model tree without having to require helpers
; Don't prefix with tree:, that's already been done when importing
(provide (all-from-out forge/lazy-tree))

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

(define/contract (make-sig [raw-name #f]
                           #:one [one #f]
                           #:lone [lone #f]
                           #:abstract [abstract #f]
                           #:is-var [is-var #f]
                           #:in [in #f]
                           #:extends [extends #f]
                           #:info [node-info empty-nodeinfo])
  (->* ()
       (symbol?
        #:one boolean?
        #:lone boolean?
        #:abstract boolean?
        #:is-var (or/c string? #t #f)
        #:in (or/c Sig? #f)
        #:extends (or/c Sig? #f)
        #:info (or/c nodeinfo? #f))
       Sig?)
  ; (check-temporal-for-var is-var name)
  (define name (or raw-name (gensym 'sig)))
  
  (when (and one lone)
    (raise-user-error (format "Sig ~a cannot be both 'one' and 'lone'." name)))

  (Sig node-info ; info
        
       1 ; arity
       
       (symbol->string name) ; name

       ;see make-relation to see why this is a thunk
       (thunk (list (symbol->string name))) ; typelist 

       (if extends (symbol->string (Sig-name extends)) "univ") ; parent 
       is-var ; is-variable

       name
       one
       lone
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
        #:is-var (or/c string? #t #f)
        #:info (or/c nodeinfo? #f))
       Relation?)

  (define-values (name sigs)
    (if raw-sigs
        (values name/sigs raw-sigs)
        (values (gensym 'relation) name/sigs)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Handling bounds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The eval-model module, which is used to evaluate `inst` block expressions, distinguishes
; between integer atoms and raw integers. This is the right choice for debugging etc. using
; these evaluator functions. However, the bounds pipeline uses only raw integers.
(define/contract (de-integer-atomize tuples)
  (-> (listof (listof (or/c number? symbol? int-atom?)))
      (listof (listof (or/c number? symbol?))))
  (map (lambda (tup)
         (map (lambda (a)
                (cond [(int-atom? a) (int-atom-n a)]
                      [else a])) tup))
       tuples))

(define/contract (safe-fast-eval-exp e binding bitwidth [safe #t])
  (->* (node/expr? hash? number?) (boolean?)
       (listof (listof (or/c number? symbol?))))
  (define result (eval-exp e binding bitwidth safe))
  (de-integer-atomize result))

(define (safe-fast-eval-int-expr ie binding bitwidth)
  (-> node/int? hash? number? (listof (listof (or/c number? symbol?))))
  (define result (eval-int-expr ie binding bitwidth))
  (caar (de-integer-atomize `((,result)))))

; Processing bind declarations requires expression evaluation. Thus, we need
; a nominal bitwidth. Since we're speaking of sig cardinalities, assume this suffices.
(define SUFFICIENT-INT-BOUND 8)

; Helper to update provenance information within a Bound struct
(define (new-orig-nodes bound the-relation bind)
  (cond
    [(not bind) (Bound-orig-nodes bound)]
    [(hash-has-key? (Bound-orig-nodes bound) the-relation)
     (hash-set (Bound-orig-nodes bound) the-relation
               (cons bind (hash-ref (Bound-orig-nodes bound) the-relation)))]
    [else (hash-set (Bound-orig-nodes bound) the-relation (list bind))]))


; Functionally update "scope" and "bound" based on this "bind" declaration
(define/contract (do-bind bind scope bound)
  (-> (or/c node/formula? node/breaking/op? Inst?)
      Scope?
      Bound?
      (values Scope? Bound?))

  (when (>= (get-verbosity) VERBOSITY_HIGH)
    (printf "  do-bind: ~a~n" bind))
  ;(when (node? bind) (printf "bind info: ~a~n" (nodeinfo-loc (node-info bind))))

  ; In case of error, highlight an AST node if able. Otherwise, do the best we can:
  (define (fail msg [condition #f])    
    (unless condition      
      (cond [(and (node? bind) (nodeinfo? (node-info bind)))             
             (raise-syntax-error #f msg
                                 (datum->syntax #f (build-source-location-syntax (nodeinfo-loc (node-info bind)))))]
            [else
             (raise (format "Invalid binding expression (~a): ~a; unable to extract syntax location." msg bind))])))
  
  
  ; Lang-specific instance checker
  (define inst-checker-hash (get-inst-checker-hash))
  (define (inst-check formula to-handle)    
    (when (and inst-checker-hash
               (hash-has-key? inst-checker-hash to-handle))
      ((hash-ref inst-checker-hash to-handle) formula)))

  ; Functionally update piecewise-binding dictionary to support per-atom definition of field bounds
  ; Cannot use update-bindings for this; just accumulate and resolve when sending to kodkod.
  ; Implicit assumption: can use "bound" from outer function declaration
  (define/contract (update-piecewise-binds op the-relation the-atom the-rhs-expr)
    (-> symbol?
        node/expr/relation? ; the-relation
        node/expr/atom?     ; the-atom
        node?               ; rhs
        Bound?)

    (define piecewise-binds (Bound-piecewise bound))
    
    ; Add the atom before evaluation, so that (atom ...) will be consistent with non-piecewise bounds.
    (define the-tuples (safe-fast-eval-exp (ast:-> the-atom the-rhs-expr) (Bound-tbindings bound) SUFFICIENT-INT-BOUND #f))
    (define the-atom-evaluated (first (first (safe-fast-eval-exp the-atom (Bound-tbindings bound) SUFFICIENT-INT-BOUND #f))))
    
    (printf "TESTING: new-orig-nodes: ~a: ~a~n" the-relation (new-orig-nodes bound the-relation bind))
    (cond [(hash-has-key? piecewise-binds the-relation)
           (define former-pwb (hash-ref piecewise-binds the-relation))
           (unless (equal? op (PiecewiseBound-operator former-pwb))
             (fail (format "mixed operators not allowed in piecewise bounds; prior had ~a, got ~a" (PiecewiseBound-operator former-pwb) op)))
           
           ; Consistency check for this combination of atom and relation; disallow re-binding.           
           (when (member the-atom-evaluated (PiecewiseBound-atoms former-pwb))
             (fail (format "rebinding detected for ~a.~a; this is not allowed." the-atom the-relation)))
           
           (define new-tuples (append (PiecewiseBound-tuples former-pwb) the-tuples))
           (Bound (Bound-pbindings bound) (Bound-tbindings bound)
                  (hash-set piecewise-binds the-relation (PiecewiseBound new-tuples (cons the-atom-evaluated (PiecewiseBound-atoms former-pwb)) op))
                  (new-orig-nodes bound the-relation bind))]
          [else
           (Bound (Bound-pbindings bound) (Bound-tbindings bound)
                  (hash-set piecewise-binds the-relation (PiecewiseBound the-tuples (list the-atom-evaluated) op))
                  (new-orig-nodes bound the-relation bind))]))
  
  (match bind
    
    ; no rel, one rel, two rel, lone rel
    [(node/formula/multiplicity info mult rel)
     ; is it safe to use the info from above here?
     (let ([rel-card (node/int/op/card info (list rel))])
       (do-bind
        (match mult
          ['no (node/formula/op/= info (list rel none))]
          ['one (node/formula/op/int= info (list rel-card 1))]
          ['two (node/formula/op/int= info (list rel-card 2))]
          ['lone
           (node/formula/op/|| info
                                   (list (node/formula/op/int< info (list rel-card 1))
                                         (node/formula/op/int= info (list rel-card 1))))])
        scope
        bound))]
    
    ; (= (card rel) n)
    [(node/formula/op/int= eq-info (list left right))
     (match left
       [(node/int/op/card c-info (list left-rel))
        (let* ([exact (safe-fast-eval-int-expr right (Bound-tbindings bound) SUFFICIENT-INT-BOUND)]
               [new-scope (if (equal? (relation-name left-rel) "Int")
                              (update-bitwidth scope exact)
                              (update-int-bound scope left-rel (Range exact exact)))])
          (values new-scope bound))]
       [_ (fail "int=")])]

    ; (<= (card rel) upper)
    [(node/formula/op/|| or-info
                             (list (node/formula/op/int< lt-info (list lt-left lt-right))
                                   (node/formula/op/int= eq-info (list eq-left eq-right))))
     (unless (and (equal? lt-left eq-left) (equal? lt-right eq-right))
       (fail "int<="))
     (match lt-left
       [(node/int/op/card c-info (list left-rel))
        (let* ([upper-val (safe-fast-eval-int-expr lt-right (Bound-tbindings bound) SUFFICIENT-INT-BOUND)]
               [new-scope (update-int-bound scope left-rel (Range 0 upper-val))])
          (values new-scope bound))]
       [_ (fail "int<=")])]

    ; (<= lower (card-rel))
    [(node/formula/op/|| or-info
                             (list (node/formula/op/int< lt-info (list lt-left lt-right))
                                   (node/formula/op/int= eq-info (list eq-left eq-right))))
     (unless (and (equal? lt-left eq-left) (equal? lt-right eq-right))
       (fail "int>="))
     (match lt-right
       [(node/int/op/card c-info (list right-rel))
        (let* ([lower-val (safe-fast-eval-int-expr lt-left (Bound-tbindings bound) SUFFICIENT-INT-BOUND)]
               [new-scope (update-int-bound scope right-rel (Range lower-val 0))])
          (values new-scope bound))]
       [_ (fail "int>=")])]

    ; Strategies
    [(node/breaking/op/is info (list left right))
     (define breaker
       (match right
         [(node/breaking/break _ breaker) breaker]
         [_ (fail "is")]))
     (match left
       [(? node/expr/relation?) (break left right)]
       [(node/expr/op/~ info arity (list left-rel))
        (break left-rel (get-co right))]
       [_ (fail "is")])
     ; hopefully the above calls to break update these somehow
     ; and hopefully they don't rely on state :(
     (values scope bound)]

    ; Other instances (which may add new scope, bound, piecewise-bound information)
    [(Inst func) (func scope bound)]

    ; rel = expr [absolute bound]
    ; (atom . rel) = expr  [partial bound, indexed by atom]
    [(node/formula/op/= info (list left right))
     (inst-check bind node/formula/op/=) 
     (cond [(node/expr/relation? left)
            (let ([tups (safe-fast-eval-exp right (Bound-tbindings bound) SUFFICIENT-INT-BOUND #f)])
              (define new-scope scope)
              (define new-bound (update-bindings bound left tups tups #:node bind))
              (values new-scope new-bound))]
           [(and (node/expr/op/join? left)
                 (list? (node/expr/op-children left))
                 (equal? 2 (length (node/expr/op-children left)))
                 (node/expr/atom? (first (node/expr/op-children left)))
                 (node/expr/relation? (second (node/expr/op-children left))))
            (define the-atom (first (node/expr/op-children left)))
            (define the-relation (second (node/expr/op-children left)))
            (when (< (node/expr-arity the-relation) 2)
              (raise (error (format "Piecewise bound for ~a: piecewise bounds may only be used for fields, not sigs." the-relation))))
            (values scope (update-piecewise-binds '= the-relation the-atom right))]
           [else
            (fail "rel=")])]     

    ; rel in expr
    ; expr in rel
    ; (atom . rel) in/ni expr  [partial bound, indexed by atom]
    ; note: "ni" is handled by desugaring to "in" with reversed arguments.
    [(node/formula/op/in info (list left right))
     (inst-check bind node/formula/op/in)
     (cond
       ; rel in expr
       [(node/expr/relation? left)
        (let ([tups (safe-fast-eval-exp right (Bound-tbindings bound) SUFFICIENT-INT-BOUND #f)])
          (define new-bound (update-bindings bound left (@set) tups #:node bind))
          (values scope new-bound))]
       ; atom.rel in expr
       [(and (node/expr/op/join? left)
             (list? (node/expr/op-children left))
             (equal? 2 (length (node/expr/op-children left)))
             (node/expr/atom? (first (node/expr/op-children left)))
             (node/expr/relation? (second (node/expr/op-children left))))
        (define the-atom (first (node/expr/op-children left)))
        (define the-relation (second (node/expr/op-children left)))
        (when (< (node/expr-arity the-relation) 2)
          (raise (error (format "Piecewise bound for ~a: piecewise bounds may only be used for fields, not sigs." the-relation))))
        (values scope (update-piecewise-binds 'in the-relation the-atom right))]
       ; rel ni expr
       [(node/expr/relation? right)
        (let ([tups (safe-fast-eval-exp left (Bound-tbindings bound) SUFFICIENT-INT-BOUND #f)])
          (define new-bound (update-bindings bound right tups #:node bind))
          (values scope new-bound))]
       ; atom.rel ni expr
       [(and (node/expr/op/join? right)
             (list? (node/expr/op-children right))
             (equal? 2 (length (node/expr/op-children right)))
             (node/expr/atom? (first (node/expr/op-children right)))
             (node/expr/relation? (second (node/expr/op-children right))))
        (define the-atom (first (node/expr/op-children right)))
        (define the-relation (second (node/expr/op-children right)))
        (when (< (node/expr-arity the-relation) 2)
          (raise (error (format "Piecewise bound for ~a: piecewise bounds may only be used for fields, not sigs." the-relation))))
        (values scope (update-piecewise-binds 'ni the-relation the-atom left))]
       ; anything else is unexpected
       [else (fail "rel in/ni")])]    

    ; Bitwidth
    ; what does (Int n:nat) look like in the AST?

    [_ (fail "unsupported")]))

; Create a new Inst struct; fold over all bind declarations in "binds".
(define/contract (make-inst binds)
  (-> (listof (or/c node/formula? node/breaking/op? Inst?))
      Inst?)
  
  (define (inst-func scope bound)
    (define-values (new-scope new-bound) 
      (for/fold ([scope scope] [bound bound])
                ([bind binds])
        (do-bind bind scope bound)))
    (values new-scope new-bound)) 

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
        #:preds (listof node/formula)
        #:scope (or/c Scope? (listof (or/c (list/c Sig? nonnegative-integer?)
                                           (list/c Sig? nonnegative-integer? nonnegative-integer?))))
        #:bounds (or/c Inst? (listof (or/c Inst? node/formula)))
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
      (cond [(Sig-one sig)
             (update-scope scope sig 1 1)]
            [(Sig-lone sig)
             (update-scope scope sig 0 1)]
            [else
             scope])))

  (define/contract default-bounds Bound?
    (let* ([bitwidth (Scope-bitwidth scope-with-ones)]
           [max-int (expt 2 (sub1 (or bitwidth DEFAULT-BITWIDTH)))]
           [ints (map int-atom (range (@- max-int) max-int))]
           [succs (map list (reverse (rest (reverse ints)))
                       (rest ints))])
      (Bound (hash)
             (hash Int (map list ints)
                   succ succs)
             (hash)
             (hash))))

  (define/contract wrapped-bounds-inst Inst?
    (if (Inst? bounds-input)
        bounds-input
        (make-inst (flatten bounds-input))))
  
  ; Invoking the Inst func folds over all involved `inst` declarations to produce a final triplet.
  (define-values (scope bounds) 
    ((Inst-func wrapped-bounds-inst) scope-with-ones default-bounds))

  ; Piecewise bounds must be resolved. Along the way, confirm that if one relation is bound
  ; piecewise (incomplete), it cannot be bound complete. E.g., we cannot mix "`Alice.father = ..."
  ; and "father = " ...
  ; However, we cannot actually convert 'in/'= piecewise bounds to complete upper bounds until
  ; send-to-kodkod, when we have created the universe of atoms and can "fill in" missing upper bounds.
  ; (Piecewise 'ni bounds should be fine to convert here.)
  (for/list ([rel (hash-keys (Bound-piecewise bounds))])
    (when (or (hash-has-key? (Bound-tbindings bounds) rel)
              (hash-has-key? (Bound-pbindings bounds) rel))
      (raise (error (format "Piecewise bounds (on ~a) may not be combined with complete bounds; remove one or the other." rel)))))
  
  (define bounds-with-piecewise-lower
    (for/fold ([bs bounds])
              ([rel (hash-keys (Bound-piecewise bounds))])
      (define pwb (hash-ref (Bound-piecewise bounds) rel))
      (define tups (PiecewiseBound-tuples pwb))
      (cond [(equal? '= (PiecewiseBound-operator pwb))
             ; update only lower bound, not upper (handled in send-to-kodkod)
             (update-bindings bs rel tups #f #:node #f)] 
            [(equal? 'in (PiecewiseBound-operator pwb))
             ; do nothing (upper bound handled in send-to-kodkod)
             bs]
            [(equal? 'ni (PiecewiseBound-operator pwb))
             ; update lower-bound
             (update-bindings bs rel tups #f #:node #f)]
            [else 
             (raise (error (format "unsupported comparison operator; got ~a, expected =, ni, or in" (PiecewiseBound-operator pwb))))])))
     
  (define spec (Run-spec state preds scope bounds-with-piecewise-lower target))        
  (define-values (result atoms server-ports kodkod-currents kodkod-bounds) 
                 (send-to-kodkod spec command #:run-name name))
  
  (Run name command spec result server-ports atoms kodkod-currents kodkod-bounds))


;; NOTE WELL: make sure not to re-use run names; this will cause an 
;; error message that might be somewhat confusing ("don't re-use run names")
(define/contract (make-run #:name [name (string->symbol (string-append "unnamed_run" (symbol->string (gensym))))]
                           #:preds [preds (list)]
                           #:scope [scope-input (list)]
                           #:bounds [bounds-input (list)]
                           #:target [target #f]
                           #:sigs [sigs-input (list)]
                           #:relations [relations-input (list)]
                           #:options [options-input #f])
  (->* () 
       (#:name symbol?
        #:preds (listof node/formula)
        #:scope (or/c Scope? (listof (or/c (list/c Sig? nonnegative-integer?)
                                           (list/c Sig? nonnegative-integer? nonnegative-integer?))))
        #:bounds (or/c Inst? (listof (or/c Inst? node/formula)))
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
        #:preds (listof node/formula)
        #:scope (or/c Scope? (listof (or/c (list/c Sig? nonnegative-integer?)
                                           (list/c Sig? nonnegative-integer? nonnegative-integer?))))
        #:bounds (or/c Inst? (listof (or/c Inst? node/formula)))
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
        #:preds (listof node/formula)
        #:scope (or/c Scope? (listof (or/c (list/c Sig? nonnegative-integer?)
                                           (list/c Sig? nonnegative-integer? nonnegative-integer?))))
        #:bounds (or/c Inst? (listof (or/c Inst? node/formula)))
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
  (->* (State?
        #:expect symbol?)
       (#:name symbol?
        #:preds (listof node/formula)
        #:scope (or/c Scope? (listof (or/c (list/c Sig? nonnegative-integer?)
                                           (list/c Sig? nonnegative-integer? nonnegative-integer?))))
        #:bounds (or/c Inst? (listof (or/c Inst? node/formula)))
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
  (->* (#:expect symbol?)
       (#:name symbol?
        #:preds (listof node/formula)
        #:scope (or/c Scope? (listof (or/c (list/c Sig? nonnegative-integer?)
                                           (list/c Sig? nonnegative-integer? nonnegative-integer?))))
        #:bounds (or/c Inst? (listof (or/c Inst? node/formula)))
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

(provide (prefix-out forge: nsa))
(define nsa (make-parameter #f))
; display :: Run -> void
; Lifted function which, when provided a Run,
; generates a Sterling instance for it.
(define (display arg1 [arg2 #f])
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
                         (send-to-kodkod contrast-run-spec))
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
  (define name (string->symbol (relation-name rel)))
  (define old-scope (get-scope scope name))

  (define lower (@max (Range-lower given-scope) (Range-lower old-scope)))
  (define upper (@min (Range-upper given-scope) (Range-upper old-scope)))
  (when (< upper lower)
    (raise (format (string-append "Bound conflict: numeric upper bound on ~a was"
                                  " less than numeric lower bound (~a vs. ~a).") 
                   rel upper lower)))    

  (define new-scope (Range lower upper))
  (define new-sig-scopes (hash-set (Scope-sig-scopes scope) name new-scope))

  (struct-copy Scope scope
               [sig-scopes new-sig-scopes]))

; update-bindings :: Bound, node/expr/relation, List<Symbol>, List<Symbol>? -> Bound
; Updates the partial binding for a given sig or relation.
; If a binding already exists, takes the intersection.
; If this results in an exact bound, adds it to the total bounds.
(define (update-bindings bound rel lower [upper #f] #:node [node #f])

  (when (>= (get-verbosity) VERBOSITY_HIGH)
    (printf "  update-bindings for ~a; |lower|=~a; |upper|=~a~n"
            rel (if lower (set-count (list->set lower)) #f) 
                (if upper (set-count (list->set upper)) #f)))

  ; In case of error, highlight an AST node if able. Otherwise, focus on the offending run command.
  (define (raise-run-error message node)
        (raise-syntax-error #f message
                            (datum->syntax #f (build-source-location-syntax (nodeinfo-loc (node-info node))))))
  ; Produce a single AST node to blame for a given relation's bound, or #f if none available
  (define (get-blame-node the-rel)
    (define result (hash-ref (Bound-orig-nodes bound) the-rel #f))
    (and result (first result)))
  
  (unless lower
    (raise (error (format "Error: update-bindings for ~a expected a lower bound, got #f." rel))))  
  (set! lower (list->set lower))
  (when upper (set! upper (list->set upper)))
  
  (define old-pbindings (Bound-pbindings bound))
  (define old-tbindings (Bound-tbindings bound))  
  
  ; New bindings can only strengthen old ones
  (when (hash-has-key? old-pbindings rel)
    (let ([old (hash-ref old-pbindings rel)])
      (set! lower (set-union lower (sbound-lower old)))
      (set! upper (cond [(and upper (sbound-upper old))
                         (set-intersect upper (sbound-upper old))]
                        [else (or upper (sbound-upper old))]))))
  
  (unless (or (not upper) (subset? lower upper))
    (raise-run-error (format "Bound conflict: upper bound on sig or field ~a was not a superset of lower bound. Lower=~a; Upper=~a." 
                             rel lower upper)
                     (get-blame-node rel)))
  
  (define new-pbindings
    (hash-set old-pbindings rel (sbound rel lower upper)))

  ; when exact bounds, put in bindings
  (define new-tbindings 
    (if (equal? lower upper) 
        (hash-set old-tbindings rel (set->list lower))
        old-tbindings))
  
  (printf "TESTING: new-orig-nodes(update-bindings): ~a: ~a~n" rel (new-orig-nodes bound rel node))
  
  ; Functionally update; piecewise bounds are out of scope so keep them the same.
  ; Likewise, original AST nodes haven't changed
  (define new-bound (Bound new-pbindings new-tbindings (Bound-piecewise bound) (new-orig-nodes bound rel node)))  
  new-bound)

; state-set-option :: State, Symbol, Symbol -> State
; Sets option to value for state.
(define (state-set-option state option value)
  (define options (State-options state))

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
                    [run_sterling value])]))

  (struct-copy State state
               [options new-options]))
