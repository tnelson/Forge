#lang typed/racket/base/optional

; This module is concerned with "is linear" and other such "breaker" bind expressions.

(require forge/lang/bounds) ;; TYPED
(require forge/types/ast-adapter) ;; TYPED, contains needed AST functions (not macros)
(require forge/shared)

(require/typed typed/racket 
  ; Missing from typed racket's definitions as of 2025-oct-31.
  [set-subtract! (All (T) (-> (Setof T) (Setof T) Void))]
  [set-add! (All (T) (-> (Setof T) T Void))]
  [set-union! (All (T) (-> (Setof T) (Setof T) Void))]
  [mutable-set (All (T) (T * -> (Setof T)))]
  [set-remove! (All (T) (-> (Setof T) T Void))]
  [list->mutable-set (All (T) (-> (U (Listof T) (Setof T)) (Setof T)))]
  ; Present in typed racket's definitions
  [hash-set! (All (K V) (-> (HashTable K V) K V Void))]
  )

(require predicates)
(require (only-in typed/racket 
                         false true set set-union set-intersect set->list list->set set? first 
                         rest cartesian-product empty empty? in-set subset?
                         set-map append* set-member?
                         set-empty? drop-right take-right for/set for*/set filter-not
                         second set-add match identity))
(require/typed forge/shared 
  [get-verbosity (-> Integer)]
  [VERBOSITY_HIGH Integer])

(provide constrain-bounds break-rel break-bound break-formulas)
(provide (rename-out [add-instance instance]) clear-breaker-state)
(provide (struct-out sbound))

; The `rel` parameter needs to allow for join arguments, not just relations.
(define-type StrategyFunction 
  (->* (Integer node/expr bound (Listof Tuple) (Listof node/expr/relation)) 
       ((U srcloc #f))
       breaker))

;;;;;;;;;;;;;;
;;;; util ;;;;
;;;;;;;;;;;;;;

(: int-add1 (-> Integer Integer))
(define (int-add1 x) (+ x 1))
(define-syntax-rule (cons! xs x) (set! xs (cons x xs)))
(define-syntax-rule (add1! x)    (begin (set! x  (int-add1 x)) x))

;;;;;;;;;;;;;;;;
;;;; breaks ;;;;
;;;;;;;;;;;;;;;;

; An "sbound" is nearly identical to the "bound" struct defined in forge/lang/bounds,
; except that it contains sets rather than lists. #f is permitted to denote a lack of value.
; Also, it may represent a bound on a _join_, not just a relation. This is not the case for "bound".
(struct sbound 
   ([relation : node/expr]
    [lower : (Setof Tuple)]
    [upper : (U (Setof Tuple) #f)]) #:transparent)

(: make-sbound (->* (node/expr/relation (Setof Tuple)) ((Setof Tuple)) sbound))
(define (make-sbound relation lower [upper : (Setof Tuple) (set)]) 
  (sbound relation lower upper))

;; N.B. Not to be confused with node/breaking/break
(struct break ([sbound : sbound] 
               [formulas : (Setof node/formula)]) 
            #:transparent)

(: make-break (-> sbound (Setof node/formula) break))
(define (make-break sbound [formulas : (Setof node/formula) (set)]) (break sbound formulas))

; sigs  :: set<sig>
; edges :: set<set<sig>>
(struct break-graph ([sigs : (Setof node/expr/relation)] 
                     [edges : (Setof (Setof node/expr/relation))]) #:transparent)

; pri               :: Nat
; break-graph       :: break-graph
; make-break        :: () -> break
; make-default      :: () -> break

(struct breaker (
    ; priority level of the breaker
    [pri : Integer]
    [break-graph : break-graph]
    [make-break : (-> break)] 
    [make-default : (-> break)])
    #:transparent)

(: bound->sbound (-> bound sbound))
(define (bound->sbound bound) 
    (make-sbound (bound-relation bound)
                (list->set (bound-lower bound))
                (list->set (bound-upper bound))))

(: sbound->bound (-> sbound bound))
(define (sbound->bound sbound)
    (cond
      [(not (node/expr/relation? (sbound-relation sbound)))
       (raise-forge-error
         #:msg (format "Internal error: sbound->bound called on non-relation. sbound=~a" sbound)
         #:context #f)]
      [(not (sbound-upper sbound))
       (raise-forge-error
         #:msg (format "No upper bound for ~a. Cannot convert to solver bound."
                       (relation-name (sbound-relation sbound)))
         #:context (sbound-relation sbound))]
      [else
       (make-bound (sbound-relation sbound)
                   (set->list (sbound-lower sbound))
                   (set->list (sbound-upper sbound)))]))

(: break-bound (-> break bound))
(define break-bound (compose sbound->bound   break-sbound))


(: make-exact-break (-> node/expr (Setof Tuple) (Setof node/formula) break))
(define (make-exact-break relation contents [formulas : (Setof node/formula) (set)])
  (break (sbound relation contents contents) formulas))

(: make-upper-break (->* (node/expr/relation (Setof Tuple)) ((Setof node/formula)) break))
(define (make-upper-break relation contents [formulas : (Setof node/formula) (set)])
  (break (sbound relation (set) contents) formulas))

;;;;;;;;;;;;;;
;;;; data ;;;;
;;;;;;;;;;;;;;

; symbol |-> (pri rel bound atom-lists rel-list) -> breaker
(: strategies (HashTable Symbol StrategyFunction))
(define strategies (make-hash))

; list of partial instance breakers
(: instances (Listof sbound))
(define instances (list))

; a ∈ rel-breaks[r] => "user wants to break r with a"
(: rel-breaks (Mutable-HashTable node/expr/relation (Setof node/breaking/break)))
(define rel-breaks (make-hash))

; rel-break-pri[r][a] = i => "breaking r with a has priority i"
(: rel-break-pri (Mutable-HashTable node/expr/relation (Mutable-HashTable node/breaking/break Integer)))
(define rel-break-pri (make-hash))

; priority counter
(: pri_c Integer)
(define pri_c 0)

; clear all state
(: clear-breaker-state (-> Void))
(define (clear-breaker-state)
    (set! instances empty)
    (set! rel-breaks (ann (make-hash) (Mutable-HashTable node/expr/relation (Setof node/breaking/break))))
    (set! rel-break-pri (ann (make-hash) (Mutable-HashTable node/expr/relation (Mutable-HashTable node/breaking/break Integer))))
    (set! pri_c 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; methods for defining breaks ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; h :: type(k) |-> set<type(v)>
(: hash-add! (All (K V2) (-> (HashTable K (Setof V2)) K V2 Void)))
(define (hash-add! h k v)
    (if (hash-has-key? h k)
        (set-add! (hash-ref h k) v)
        (hash-set! h k (mutable-set v))))

; h :: type(k1) |-> type(k2) |-> type(v)
(: hash-add-set! (All (K1 K2 V) (-> (Mutable-HashTable K1 (Mutable-HashTable K2 V)) K1 K2 V Void)))
(define (hash-add-set! h k1 k2 v)
    (unless (hash-has-key? h k1) (hash-set! h k1 (ann (make-hash) (Mutable-HashTable K2 V))))
    (define h_k1 (hash-ref h k1))
    (unless (hash-has-key? h_k1 k2) (hash-set! h_k1 k2 v)))

; strategy :: () -> breaker
(: add-strategy (-> Symbol StrategyFunction Void))
(define (add-strategy a strategy)
    (hash-set! strategies a strategy))

(: break-rel (->* (node/expr/relation) () #:rest (U Symbol node/breaking/break) Void))
(define (break-rel rel . breaks)
    (for ([break breaks])
        (: break-key Symbol)
        (: break-node node/breaking/break)
        (define-values (break-key break-node)
            (cond [(symbol? break) (values break (node/breaking/break empty-nodeinfo break))]
                  [(node/breaking/break? break) (values (node/breaking/break-break break) break)]
                  [else (raise (format "Not a value break or break name: ~a" break))]))
        (unless (hash-has-key? strategies break-key)
                (error (format "break not implemented among ~a" strategies) break-key))
        (hash-add! rel-breaks rel break-node)
        (hash-add-set! rel-break-pri rel break-node (add1! pri_c))))

(: add-instance (-> sbound Void))
(define (add-instance i) (cons! instances i)) 

(: constrain-bounds (-> (Listof bound) 
                        (Listof node/expr/relation) 
                        (HashTable node/expr/relation (Listof FAtom))
                        (HashTable node/expr/relation (Listof node/expr/relation)) 
                        (HashTable node/expr/relation node/expr/relation) 
                        (Values (Listof bound) (Listof node/formula))))
(define (constrain-bounds total-bounds maybe-list-sigs bounds-store relations-store extensions-store) 
    (define name-to-rel (make-hash))
    (hash-for-each relations-store (λ ([k : node/expr/relation] v) (hash-set! name-to-rel (node/expr/relation-name k) k)))
    (for ([s maybe-list-sigs]) (hash-set! name-to-rel (node/expr/relation-name s) s))
    ; returns (values new-total-bounds (set->list formulas))
    (define new-total-bounds (ann (list) (Listof bound)))
    (define formulas (ann (mutable-set) (Setof node/formula)))
    ; unextended sets
    (define sigs (list->mutable-set maybe-list-sigs))

    ; maintain non-transitive reachability relation 
    (define reachable (ann (make-hash) (Mutable-HashTable (U node/expr/relation Symbol) 
                                                          (Setof (U node/expr/relation Symbol)))))
    (hash-set! reachable 'broken (mutable-set 'broken))
    (for ([sig sigs]) (hash-set! reachable sig (mutable-set sig)))

    (hash-for-each extensions-store (λ (k v) (set-remove! sigs v)))    

    ; First add all partial instances. 
    ; This is not obsolete! The breakers need to "play nicely" with user-defined `inst`s.
    (define defined-relations (mutable-set))
    (for ([b instances])
        (define rel-inst (sbound-relation b))
        (for ([bound total-bounds])
            (define rel (bound-relation bound))
            (when (equal? rel-inst rel) 
                  (begin
                    (define rel (sbound-relation b))
                    (if (equal? 'Sig (object-name rel))
                        ;; ni-only bounds have #f upper; use scope-resolved upper from total-bounds
                        (if (sbound-upper b)
                            (cons! new-total-bounds (sbound->bound b))
                            (cons! new-total-bounds
                                   (make-bound rel-inst
                                               (set->list (sbound-lower b))
                                               (bound-upper bound))))
                        (cons! new-total-bounds bound))
                    (set-add! defined-relations rel)
                    (define typelist ((node/expr/relation-typelist-thunk rel)))
                    (for ([t typelist]) (when (hash-has-key? name-to-rel t)
                        (set-remove! sigs (hash-ref name-to-rel t))))))))

        

    ; proposed breakers from each relation
    (define candidates (ann (list) (Listof breaker)))

    (for ([bound total-bounds])
        ; get declared breaks for the relation associated with this bound        
        (define rel (bound-relation bound))       

        (define breaks (hash-ref rel-breaks rel (ann (lambda () (set)) (-> (Setof node/breaking/break)) ))) 
        (define backup (ann (lambda () (make-hash)) (-> (Mutable-HashTable node/breaking/break Integer))))
        (define break-pris (ann (hash-ref rel-break-pri rel backup) (Mutable-HashTable node/breaking/break Integer)))

        (define defined (set-member? defined-relations rel))
        (cond [(set-empty? breaks)
            (unless defined (cons! new-total-bounds bound))
        ][else
            (unless (hash-has-key? relations-store rel)
              (raise-forge-error #:msg (format "Attempted to set or modify bounds of ~a, but the annotation given was of the wrong form (sig vs. field).~n" rel)
                                 #:context #f
                                 #:raise? #t))
            (define rel-list (ann (hash-ref relations-store rel) (Listof node/expr/relation)))
            (define atom-lists (map (λ ([b : node/expr/relation]) 
              (define sym-list (ann (hash-ref bounds-store b (lambda () (list))) (Listof FAtom)))
              (hash-ref bounds-store b)) rel-list))

            ; make all breakers
            ;; break is a "break", strategy returns a "breaker"

            (define breakers (map (lambda ([b : (U node/breaking/break Symbol)]) 
                (define-values (break-sym break-node)
                      (cond [(symbol? b) (values b (node/breaking/break empty-nodeinfo b))]
                            [(node/breaking/break? b) (values (node/breaking/break-break b) b)]
                            [else (raise-forge-error #:msg (format "constrain-bounds: not a valid break name: ~a~n" break)
                                                     #:context #f
                                                     #:raise? #t)]))
                (define loc (if (node? b) 
                                (nodeinfo-loc (node-info b)) 
                                #f))
                (define strategy (ann (hash-ref strategies break-sym) StrategyFunction))
                ; What if it's a symbol instead of a break?? check dev !!!! b could be maybe
                (define pri (ann (hash-ref break-pris break-node) Integer))
                (strategy pri rel bound atom-lists rel-list loc))                
              (set->list breaks)))

            (set! breakers (sort breakers (lambda ([x : breaker] [y : breaker]) 
              (< (breaker-pri x) (breaker-pri y)))))

            ; propose highest pri breaker that breaks only leaf sigs
            ; break the rest the default way (with get-formulas)
            (define broken defined)
            (for ([bkr breakers])
                (cond [broken
                    (define default ((breaker-make-default bkr)))
                    (set-union! formulas (break-formulas default))
                ][else
                    (define break-graph (breaker-break-graph bkr))
                    (define broken-sigs (break-graph-sigs break-graph))
                    (cond [(subset? broken-sigs sigs)
                        (cons! candidates bkr)
                        (set! broken #t)
                    ][else
                        (define default ((breaker-make-default bkr)))
                        (set-union! formulas (break-formulas default))
                    ])
                ])
            )
            (unless (or broken defined) (cons! new-total-bounds bound))
        ])     
    )

    #|
        Now we try to use candidate breakers, starting with highest priority.

        We maintain a reachability relation. If applying a breaker would create a loop,
        the breaker isn't applied and the default formulas are used instead.
        Otherwise, we do the break and update the relation.

        The implementation may seem wrong but the relation is intentionally non-transitive.
        Consider: sig A { fs: B->C }, so that fs: A->B->C is a set of functions.
        Information can flow between B<~>C and A<~>C, but not A<~>B!
        This is important to get right because of our design principle of wrapping instances
            (fs in this case) inside solution sigs (A in this case)

        Paths between broken sigs can also break soundness.
        Broken sigs are given an edge to a unique 'broken "sig", so we only need to check for loops.
    |#
    
    (set! candidates (sort candidates (lambda ([x : breaker] [y : breaker]) 
              (< (breaker-pri x) (breaker-pri y)))))

    (for ([breaker candidates])
        (define break-graph (breaker-break-graph breaker))
        (define broken-sigs (break-graph-sigs break-graph))
        (define broken-edges (break-graph-edges break-graph))

        (define edges (ann (list) (Listof (U (Pairof node/expr/relation Symbol) 
                                             (Pairof node/expr/relation node/expr/relation)))))
        
        ; reduce broken sigs to broken edges between those sigs and the auxiliary 'broken symbol
        ; TODO: replace 'broken with univ
        (for ([sig broken-sigs]) (cons! edges (cons sig 'broken)))
        ; get all pairs from sets
        (for ([edge-as-set broken-edges])
            ; TODO: make functional
            (define edge (set->list edge-as-set))
            (define L (length edge))
            (for* ([i (in-range 0 (- L 1))]
                   [j (in-range (+ i 1) L)])
                (cons! edges (cons (list-ref edge i) (list-ref edge j)))
            )
        )
    
        ; acceptable :<-> doesn't create loops <-> no edges already exist
        (define acceptable (foldl (lambda ([edge : (U (Pairof node/expr/relation Symbol) 
                                                      (Pairof node/expr/relation node/expr/relation))] 
                                           [res : Boolean]) 
            (define A (car edge))
            (define B (cdr edge))
            (define Aval (ann (hash-ref reachable A) (Setof (U node/expr/relation Symbol))))
            (and res (not (set-member? Aval B)))) #t edges))

        (cond 
          [acceptable
            ; update reachability. do all edges in parallel
            (define new-reachable (ann (make-hash) (Mutable-HashTable node/expr/relation 
                                                                      (Setof (U node/expr/relation Symbol)))))
            (for ([edge edges])
                (define A (ann (car edge) node/expr/relation))
                (define B (ann (cdr edge) (U node/expr/relation Symbol))) 
                (when (not (hash-has-key? new-reachable A)) 
                    (hash-set! new-reachable A (ann (mutable-set) (Setof (U node/expr/relation Symbol)))))
                (when (not (hash-has-key? new-reachable B)) 
                    (when (node/expr/relation? B) ; narrowing
                        (hash-set! new-reachable B (ann (mutable-set) (Setof (U node/expr/relation Symbol))))))
                (set-union! (hash-ref new-reachable A) (hash-ref reachable B))
                (when (node/expr/relation? B)
                    (set-union! (hash-ref new-reachable B) (hash-ref reachable A)))
            )
            
            (hash-for-each new-reachable (λ ([sig : node/expr/relation] 
                                             [newset : (Setof (U node/expr/relation Symbol))])
                ; set new sigs reachable from sig and vice versa
                (define oldset (ann (hash-ref reachable sig) (Setof (U node/expr/relation Symbol))))
                (set-subtract! newset oldset)
                (for ([sig2 newset])
                    (define oldset2 (hash-ref reachable sig2))
                    (set-add! oldset sig2)
                    (set-add! oldset2 sig)
                )
            ))

            ; do break
            (define break ((breaker-make-break breaker)))
            (cons! new-total-bounds (break-bound break))
            (set-union! formulas (break-formulas break))]
        [else
            ; do default break
            (define default ((breaker-make-default breaker)))
            (cons! new-total-bounds (sbound->bound (break-sbound default)))
            (set-union! formulas (break-formulas default))
        ])
    )

    (when (>= (get-verbosity) VERBOSITY_HIGH)
      (printf "~nBreakers ran.~n        New total bounds:~a~n        New formulas:~a~n" 
        new-total-bounds formulas))

    (values new-total-bounds (set->list formulas)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Strategy Combinators ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; turn a strategy on n-ary relations into one on arbitrary arity relations
; ex: (f:B->C) => (g:A->B->C) where f is declared 'foo
; we will declare with formulas that g[a] is 'foo for all a in A
; but we will only enforce this with bounds for a single a in A
;   Note: this is needed to support `is linear` on e.g., 
;   `sig N { next: lone N}` because `next` is ternary.
(: variadic (-> Integer StrategyFunction StrategyFunction))
(define (variadic n f)
    (λ ([pri : Integer] [rel : node/expr] [bound : bound] [atom-lists : (Listof Tuple)]
        [rel-list : (Listof node/expr/relation)] [loc : (U srcloc #f) #f])
        (define info (just-location-info loc))

        (cond [(= (length rel-list) n)
            (f pri rel bound atom-lists rel-list loc)
        ][else
            (define prefix (ann (drop-right rel-list n) (Listof node/expr)))
            (define postfix (take-right rel-list n))
            (define prefix-lists (ann (drop-right atom-lists n) (Listof Tuple)))
            (define postfix-lists (take-right atom-lists n))
            (define vars (for/list ([p prefix]) : (Listof node/expr/quantifier-var)
                (let ([symv (gensym "v")])
                    (node/expr/quantifier-var empty-nodeinfo 1 symv symv))
            ))
            (define new-rel (build-box-join rel vars))  ; rel[a][b]...
            (define sub-breaker (f pri new-rel bound postfix-lists postfix loc))
            (define sub-break-graph (breaker-break-graph sub-breaker))
            (define sigs (ann (break-graph-sigs sub-break-graph) (Setof node/expr/relation)))
            (define edges (break-graph-edges sub-break-graph))
            (define edgesAnd (for/set : (Setof (Setof node/expr/relation)) ([sig sigs] [p prefix])
               (if (node/expr/relation? p) 
                   (set sig p)
                   (raise (format "Internal error: breaks.variadic combining sigs and non-sigs")))))
            (define new-break-graph (break-graph sigs (set-union edges edgesAnd)))
            (breaker pri
                new-break-graph
                (λ ()
                    ; unpack results of sub-breaker
                    (define sub-break ((breaker-make-break sub-breaker)))
                    (define sub-sbound (break-sbound sub-break))
                    (define sub-lower (ann (sbound-lower sub-sbound) (Setof Tuple)))
                    (define sub-upper (or (sbound-upper sub-sbound)
                                          (raise-forge-error #:msg "Internal error: sub-breaker gave no upper bound" #:context #f)))

                    (cond [(set-empty? sigs)
                        ; no sigs are broken, so use sub-bounds for ALL instances
                        (define cart-pref (ann (apply cartesian-product prefix-lists) (Listof Tuple)))
                        (define lower (for*/set : (Setof Tuple) ([c cart-pref] [l sub-lower]) (append c l)))
                        (define upper (for*/set : (Setof Tuple) ([c cart-pref] [u sub-upper]) (append c u)))
                        (define bound (sbound rel lower upper))

                        (define sub-formulas (break-formulas sub-break))                        
                        (define formulas (for/set : (Setof node/formula) ([f sub-formulas])                            
                            (quantified-formula info 'all 
                              (map (ann cons (-> node/expr/quantifier-var node/expr (Pairof node/expr/quantifier-var node/expr))) 
                                   vars prefix) f)
                        )) ; info quantifier decls formula)

                        (break bound formulas)
                    ][else
                        ; just use the sub-bounds for a single instance of prefix
                        (define cars (map (ann car (-> Tuple FAtom)) prefix-lists))
                        (define cdrs (map (ann cdr (-> Tuple Tuple)) prefix-lists))
                        (define lower (for/set : (Setof Tuple) ([l sub-lower]) (append cars l)))
                        (define upper (set-union
                            (for/set : (Setof Tuple) ([u sub-upper]) (append cars u))
                            (list->set (apply cartesian-product (append cdrs postfix-lists)))
                        ))
                        (define bound (sbound rel lower upper))

                        ; use default formulas unless single instance
                        (define sub-formulas (if (> (apply * (map (ann length (-> (Listof Any) Integer)) prefix-lists)) 1)
                            (break-formulas ((breaker-make-default sub-breaker)))
                            (break-formulas sub-break)
                        ))
                        ; wrap each formula in foralls for each prefix rel 
                        (define formulas (for/set : (Setof node/formula) ([f sub-formulas])
                            (quantified-formula info 'all 
                               (map (ann cons (-> node/expr/quantifier-var node/expr (Pairof node/expr/quantifier-var node/expr)))
                                    vars prefix) f)
                        ))

                        (break bound formulas)
                    ])
                )
                (λ ()
                    (define sub-break ((breaker-make-default sub-breaker)));
                    (define sub-formulas (break-formulas sub-break))                                          
                    (define formulas (for/set : (Setof node/formula) ([f sub-formulas])
                        (quantified-formula info 'all 
                          (map (ann cons (-> node/expr/quantifier-var node/expr (Pairof node/expr/quantifier-var node/expr))) 
                               vars prefix) f)
                    ))
                    (break (bound->sbound bound) formulas)
                )
            )
        ])
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; define breaks and compositions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A->A Strategies ;;;
(add-strategy 'linear (λ (pri rel bound atom-lists rel-list [loc #f])     
    (define atoms (first atom-lists))
    (define sig (first rel-list))
    (define info (just-location-info loc))
    (define init (node/expr/quantifier-var (just-location-info loc) 1 'init 'init))
    (define term (node/expr/quantifier-var (just-location-info loc) 1 'term 'term))
    (define x (node/expr/quantifier-var (just-location-info loc) 1 'x 'x))
    (breaker pri
        (break-graph (set sig) (set))
        (λ () (make-exact-break rel (list->set (map (ann list (-> FAtom FAtom (Listof FAtom))) 
                                                  (drop-right atoms 1) (cdr atoms))) 
                                (set)))
        (λ () (break (bound->sbound bound) (set
            (quantified-formula info 'some (list (cons init sig)) (&&/func #:info info
                (multiplicity-formula info 'no (join/func #:info info rel init))
                (quantified-formula info 'all (list (cons x (-/func #:info info sig init)))
                                              (multiplicity-formula info 'one (join/func #:info info rel x)))
                (=/func #:info info (join/func #:info info init (*/func #:info info rel)) sig)
            ))
            
            (quantified-formula info 'some (list (cons term sig)) (&&/func #:info info
                (multiplicity-formula info 'no (join/func #:info info term rel))
                (quantified-formula info 'all (list (cons x (-/func #:info info sig term))) 
                                              (multiplicity-formula info 'one (join/func #:info info x rel)))
                (=/func #:info info (join/func #:info info (*/func #:info info rel) term) sig)
            ))
        )))
    )
))

(add-strategy 'plinear (λ (pri rel bound atom-lists rel-list [loc #f])     
    (define atoms (first atom-lists))
    (define sig (first rel-list))
    (define info (just-location-info loc))
    (define init (node/expr/quantifier-var (just-location-info loc) 1 'init 'init))
    (define x (node/expr/quantifier-var (just-location-info loc) 1 'x 'x))
    (breaker pri
        (break-graph (set sig) (set))
        (λ () (break
            (sbound rel 
                (set) ;(set (take atoms 2))
                (list->set (map (ann list (-> FAtom FAtom (Listof FAtom))) 
                                (drop-right atoms 1) (cdr atoms)))
            )
            (set
                (multiplicity-formula info 'lone 
                  (set/func #:info info (list (cons init sig)) (&&/func #:info info
                    (multiplicity-formula info 'no (join/func #:info info rel init))
                    (multiplicity-formula info 'some (join/func #:info info init rel)))))
            )
        ))
        (λ () (break (bound->sbound bound) (set
            (multiplicity-formula info 'lone (-/func #:info info (join/func #:info info rel sig) (join/func #:info info sig rel)))    ; lone init
            (multiplicity-formula info 'lone (-/func #:info info (join/func #:info info sig rel) (join/func #:info info rel sig)))    ; lone term
            (multiplicity-formula info 'no (&/func #:info info iden (^/func #:info info rel)))   ; acyclic
            (quantified-formula info 'all (list (cons x sig)) (&&/func #:info info       ; all x have
                (multiplicity-formula info 'lone (join/func #:info info x rel))   ; lone successor
                (multiplicity-formula info 'lone (join/func #:info info rel x))   ; lone predecessor
            ))
        )))
    )
))

;;; A->B Strategies ;;;
(add-strategy 'func (λ ([pri : Integer] [rel : node/expr] [bound : bound] [atom-lists : (Listof Tuple)] [rel-list : (Listof node/expr)] [loc : (U srcloc #f) #f])
    (: funcformulajoin (-> (Listof node/expr/quantifier-var) node/expr))
    (define (funcformulajoin quantvarlst) 
        (cond 
            [(empty? (rest quantvarlst)) (join/func #:info (just-location-info loc) (first quantvarlst) rel)]
            [else (join/func #:info (just-location-info loc) (first quantvarlst) (funcformulajoin (rest quantvarlst)))]))

    (: funcformula (-> (Listof node/expr) (Listof node/expr/quantifier-var) node/formula))
    (define (funcformula rllst quantvarlst) 
        (cond
          [(empty? (rest (rest rllst)))
           (let* ([var-id (gensym 'func)]
                  [a (node/expr/quantifier-var (just-location-info loc) 1 var-id var-id)])
             (quantified-formula (just-location-info loc) 'all
                                  (ann (list (cons a (first rllst))) (Listof (Pairof node/expr/quantifier-var node/expr)))
                                  (one/func #:info (just-location-info loc) (funcformulajoin (cons a quantvarlst)))))]
            [else
             (let* ([var-id (gensym 'func)]
                  [a (node/expr/quantifier-var (just-location-info loc) 1 var-id var-id)])
             (quantified-formula (just-location-info loc) 'all
                                  (list (cons a (first rllst)))
                                  (funcformula (rest rllst) (cons a quantvarlst))))]))
    (define formulas (set (funcformula rel-list (list))))
    
        (breaker pri 
            (break-graph (set) (set))
            (λ () (break (bound->sbound bound) formulas))
            (λ () (break (bound->sbound bound) formulas))
            )))

(add-strategy 'pfunc
              (λ (pri rel bound atom-lists rel-list [loc #f])
                (define info (just-location-info loc))

                (: pfuncformulajoin (-> (Listof node/expr/quantifier-var) node/expr))
                (define (pfuncformulajoin quantvarlst) 
                  (cond
                    ; x_n.rel
                    [(empty? (rest quantvarlst)) (join/func #:info info (first quantvarlst) rel)]
                    ; ... x_n-1.x_n.rel
                    [else (join/func #:info info (first quantvarlst) (pfuncformulajoin (rest quantvarlst)))]))

                (: pfuncformula (-> (Listof node/expr) (Listof node/expr/quantifier-var) node/formula))
                (define (pfuncformula rllst quantvarlst)
                  (cond
                    [(empty? (rest (rest rllst)))
                     (let* ([var-id (gensym 'pfunc)]
                            [a (node/expr/quantifier-var info 1 var-id var-id)])
                       (quantified-formula info 'all
                                            (list (cons a (first rllst)))
                                            (multiplicity-formula info 'lone (pfuncformulajoin (cons a quantvarlst)))))]
                    [else (let* ([var-id (gensym 'pfunc)]
                                 [a (node/expr/quantifier-var info 1 var-id var-id)])
                            (quantified-formula info 'all
                                                 (list (cons a (first rllst)))
                                                 (pfuncformula (rest rllst) (cons a quantvarlst))))]))
                (define pf-fmla (pfuncformula rel-list (list)))
                (define formulas (set pf-fmla))
                
        (breaker pri
            (break-graph (set) (set))
            (λ () (break (bound->sbound bound) formulas))
            (λ () (break (bound->sbound bound) formulas))
        )
    ))

; use to prevent breaks
(: defaultStrategy StrategyFunction)
(define defaultStrategy (λ ([pri : Integer] [rel : node/expr] [bound : bound] 
                           [atom-lists : (Listof Tuple)] 
                           [rel-list : (Listof node/expr)] [loc : (U srcloc #f) #f]) 
  (if (node/expr/relation? rel)
    (breaker pri
      (break-graph (set) (set))    
      (λ ()      
        (make-upper-break rel (list->set (apply cartesian-product atom-lists))))
      (λ () (break (bound->sbound bound) (set)))
      )
    (raise (format "Internal error in breaks.defaultStrategy. Given: ~a; from:~a~n" rel loc)))))

(add-strategy 'default defaultStrategy)

; this one cannot afford to go to purely formula break
(add-strategy 'linear (variadic 2 (hash-ref strategies 'linear)))
(add-strategy 'plinear (variadic 2 (hash-ref strategies 'plinear)))

(add-strategy 'func (hash-ref strategies 'func))
(add-strategy 'pfunc (hash-ref strategies 'pfunc))





#|
ADDING BREAKS
  - add breaks here with using add-strategy and the declare forms:
  - add a call to make-breaker to the bottom of ast.rkt for your new breaker
|#


;; Unsound as written (an empty break graph lets two breaks claim the same sig)
; (add-strategy 'acyclic (λ (pri rel bound [atom-lists : (Listof Any)] [rel-list : (Listof Any)] [loc #f]) 
;     (define atoms (first atom-lists))
;     (define sig (first rel-list))
;     (breaker pri
;         (break-graph (set) (set))
;         (λ ()
;             (make-upper-break rel
;                             (for*/list ([i (length atoms)]
;                                         [j (length atoms)]
;                                         #:when (< i j))
;                                     (list (list-ref atoms i) (list-ref atoms j)))))
;         (λ () (break bound (set
;             (@no/info (just-location-info loc) ([x sig])
;                 (@in x (@join x (@^ rel)))
;             )
;         )))
;     )
; ))
; (add-strategy 'tree (λ (pri rel bound atom-lists rel-list [loc #f]) 
;     (define atoms (first atom-lists))
;     (define sig (first rel-list))
;     (breaker pri
;         (break-graph (set) (set))
;         (λ ()
;             (make-break 
;                 (bound->sbound (make-upper-bound rel
;                             (for*/list ([i (length atoms)]
;                                         [j (length atoms)]
;                                         #:when (< i j))
;                                     (list (list-ref atoms i) (list-ref atoms j)))))
;                 (set
;                     (@some/info (just-location-info loc) ([n sig]) 
;                         (@all ([m (@- sig n)]) 
;                             (@one (@join rel m))
;                         )
;                     )
;                 )))
;         (λ () (break bound (set
;             (@some/info (just-location-info loc) ([n sig]) (@&&
;                 (@no/info (just-location-info loc) (@join rel n))
;                 (@all ([m (@- sig n)]) 
;                     (@one (@join rel m))
;                 )
;             ))
;         )))
;     )
; ))
; (add-strategy 'acyclic (variadic 2 (hash-ref strategies 'acyclic)))
; (add-strategy 'tree (variadic 2 (hash-ref strategies 'tree)))
