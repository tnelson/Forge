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

(provide constrain-bounds (rename-out [break-rel break]) break-bound break-formulas)
(provide (rename-out [add-instance instance]) clear-breaker-state)
(provide make-exact-sbound)
(provide (struct-out sbound))

; The `rel` parameter needs to allow for join arguments, not just relations.
(define-type StrategyFunction 
  (->* (Integer node/expr bound (Listof Tuple) (Listof node/expr/relation)) 
       ((U srcloc #f))
       breaker))

;;;;;;;;;;;;;;
;;;; util ;;;;
;;;;;;;;;;;;;;

(define-syntax-rule (cons! xs x) (set! xs (cons x xs)))
(define-syntax-rule (add1! x)    (begin (set! x  (add1 x)) x))

;;;;;;;;;;;;;;;;
;;;; breaks ;;;;
;;;;;;;;;;;;;;;;

; An "sbound" is nearly identical to the "bound" struct defined in forge/lang/bounds,
; except that it contains sets rather than lists. #f is permitted to denote a lack of value.
; Also, it may represent a bound on a _join_, not just a relation. This is not the case for "bound".
(struct sbound 
   ([relation : node/expr]
    [lower : (Setof Tuple)]
    [upper : (Setof Tuple)]) #:transparent)

(: make-sbound (->* (node/expr/relation (Setof Tuple)) ((Setof Tuple)) sbound))
(define (make-sbound relation lower [upper : (Setof Tuple) (set)]) 
  (sbound relation lower upper))
(: make-exact-sbound (-> node/expr/relation (Setof Tuple) sbound))
(define (make-exact-sbound relation s) (sbound relation s s))

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

; ORIGINAL CODE
; (struct breaker (pri break-graph make-break make-default) #:transparent)
; BEGIN INSERTED TEMPORARY FIX FOR 'FUNC
(struct breaker (
    ; priority level of the breaker
    [pri : Integer]
    [break-graph : break-graph]
    [make-break : (-> break)] 
    [make-default : (-> break)] 
    [use-formula : Boolean ]) 
    #:transparent #:mutable)

(: formula-breaker (-> Integer break-graph (-> break) (-> break) breaker))
(define (formula-breaker pri break-graph make-break make-default)
    (define res (breaker pri break-graph make-break make-default #f))
    (set-breaker-use-formula! res #t)
    res)
; END INSERTED TEMPORARY FIX FOR 'FUNC

(: bound->sbound (-> bound sbound))
(define (bound->sbound bound) 
    (make-sbound (bound-relation bound)
                (list->set (bound-lower bound))
                (list->set (bound-upper bound))))

(: sbound->bound (-> sbound bound))
(define (sbound->bound sbound) 
    (if (node/expr/relation? (sbound-relation sbound))
        (make-bound (sbound-relation sbound)
                (set->list (sbound-lower sbound))
                (set->list (sbound-upper sbound)))
        (raise (format "Internal error: sbound->bound called on non-relation. sbound=~a" sbound))))

(: bound->break (-> bound break))                
(define (bound->break bound) (break (bound->sbound bound) (set)))
(: break-lower (-> break (Setof Tuple)))
(define break-lower    (compose sbound-lower    break-sbound))
(: break-upper (-> break (Setof Tuple)))
(define break-upper    (compose sbound-upper    break-sbound))
(: break-relation (-> break node/expr))
(define break-relation (compose sbound-relation break-sbound))
(: break-bound (-> break bound))
(define break-bound (compose sbound->bound   break-sbound))

; (: sbound+ (-> (Listof sbound) bound))
; (define (sbound+ sbounds)
;     ; TODO: assert all same relations
;     (make-bound (break-relation (first sbounds)) 
;                 (apply set-union     (map break-lower sbounds))
;                 (apply set-intersect (map break-lower sbounds))))

; (: break+ (-> (Listof break) break))
; (define (break+ . breaks)
;     (make-break (sbound+ breaks)
;                 (apply set-union (map break-formulas breaks))))

(: make-exact-break (-> node/expr (Setof Tuple) (Setof node/formula) break))
(define (make-exact-break relation contents [formulas : (Setof node/formula) (set)])
  (break (sbound relation contents contents) formulas))

(: make-upper-break (->* (node/expr/relation (Setof Tuple)) ((Setof node/formula)) break))
(define (make-upper-break relation contents [formulas : (Setof node/formula) (set)])
  (break (sbound relation (set) contents) formulas))

(: make-lower-break (-> node/expr/relation (Setof Tuple) (Listof Tuple) (Setof node/formula) break))
(define (make-lower-break relation contents atom-lists [formulas : (Setof node/formula) (set)])
  (break (sbound relation contents (list->set (apply cartesian-product atom-lists))) formulas))

;;;;;;;;;;;;;;
;;;; data ;;;;
;;;;;;;;;;;;;;

; symbol |-> (pri rel bound atom-lists rel-list) -> breaker
(: strategies (HashTable Symbol StrategyFunction))
(define strategies (make-hash))

; compos[{a₀,...,aᵢ}] = b => a₀+...+aᵢ = b
(: compos (HashTable (Setof Symbol) Symbol))
(define compos (make-hash))

; a ∈ upsets[b] => a > b
(: upsets (HashTable Symbol (Setof Symbol)))
(define upsets (make-hash))

; a ∈ downsets[b] => a < b
(: downsets (HashTable Symbol (Setof Symbol)))
(define downsets (make-hash))

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
(: pri_c Number)
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
(: hash-add-set! (All (K1 K2 V) (-> (HashTable K1 (HashTable K2 V)) K1 K2 V Void)))
(define (hash-add-set! h k1 k2 v)
    (unless (hash-has-key? h k1) (hash-set! h k1 (ann (make-hash) (HashTable K2 V))))
    (define h_k1 (hash-ref h k1))
    ;; CHANGED pri_c to v.
    ;(unless (hash-has-key? h_k1 k2) (hash-set! h_k1 k2 pri_c)))
    (unless (hash-has-key? h_k1 k2) (hash-set! h_k1 k2 v)))

; strategy :: () -> breaker
(: add-strategy (-> Symbol StrategyFunction Void))
(define (add-strategy a strategy)
    (hash-set! strategies a strategy)
    (hash-add! upsets a a)      ;; a > a
    (hash-add! downsets a a))   ;; a < a

(: equiv (-> Symbol Symbol * Any))
(define (equiv a . bs) 
    (hash-set! compos (apply set bs) a)
    (apply stricter a bs)
    ; TODO: if no fn defined for a, default to naively doing all bs
    #;(unless (hash-has-key? strategies a)
            (hash-set! strategies a (λ (rel atom-lists rel-list)
                (apply break+ (for ([b bs]) 
                    ((hash-ref strategies b) atom-lists) )) )))
)
(: dominate (-> Symbol Symbol Void))
(define (dominate a b)  
    (define upa (hash-ref upsets a))
    (define downb (hash-ref downsets b))
    (for ([x (in-set upa)])             ;; x > a
        (hash-add! upsets b x)          ;; x > b
        (hash-add! downsets x b)        ;; b < x
        (hash-set! compos (set b x) x)  ;; x = x + b
    )
    (for ([x (in-set downb)])           ;; x < b
        (hash-add! downsets a x)        ;; x < a
        (hash-add! upsets x a)          ;; a > x
        (hash-set! compos (set a x) a)  ;; a = a + x
    )
)
(: stricter (-> Symbol Symbol * Void))
(define (stricter a . bs) (for ([b bs]) (dominate a b)))
(: weaker (-> Symbol Symbol * Void))
(define (weaker a . bs) (for ([b bs]) (dominate b a)))

; TODO: allow syntax like (declare 'a 'b > 'c 'd > 'e 'f)
(define-syntax declare
  (syntax-rules (> < =)
    [(_ a > bs ...) (stricter a bs ...)]
    [(_ a < bs ...) (weaker a bs ...)]
    [(_ a = bs ...) (equiv a bs ...)]))

(: min-breaks! (-> (Setof node/breaking/break) (Mutable-HashTable node/breaking/break Integer) Void))
(define (min-breaks! breaks break-pris)
    (define changed (ann false Boolean))
    (hash-for-each compos (λ ([k : (Setof Symbol)] [v : Symbol])
        (when (subset? k breaks)
              (set-subtract! breaks k)
              (set-add! breaks v)
              ; new break should have priority of highest priority component
              (define pris (ann (set-map k (lambda ([s : Symbol]) 
                (hash-ref break-pris (node/breaking/break empty-nodeinfo s)))) (Listof Integer)))
              (define min-pri (apply min pris)) ; was max-pri (typo?)
              (hash-set! break-pris (node/breaking/break empty-nodeinfo v) min-pri)
              (set! changed true))
    ))
    (when changed (min-breaks! breaks break-pris))
)

; renamed-out to 'break for use in forge
(: break-rel (-> node/expr/relation (Listof (U Symbol node/breaking/break)) Void))
(define (break-rel rel . breaks)
    (for ([break breaks])
        (define-values (break-key break-node)
            (cond [(symbol? break) (values break (node/breaking/break empty-nodeinfo break))]
                  [(node/breaking/break? break) (values (node/breaking/break-break break) break)]
                  [else (raise (format "Not a value break or break name: ~a" break))]))
        (unless (hash-has-key? strategies break-key)
                (error (format "break not implemented among ~a" strategies) break-key))
        (hash-add! rel-breaks rel break)
        (hash-add-set! rel-break-pri rel break (add1! pri_c))))

(: add-instance (-> sbound Void))
(define (add-instance i) (cons! instances i)) 

(: constrain-bounds (-> (Listof bound) (Listof node/expr/relation) 
                        (HashTable node/expr/relation (List Symbol))
                        (HashTable node/expr/relation (Listof node/expr/relation)) 
                        (HashTable node/expr/relation (Listof node/expr/relation)) 
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
                        (cons! new-total-bounds (sbound->bound b))
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

        ; compose breaks
        (min-breaks! breaks break-pris)

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
              (define sym-list (ann (hash-ref bounds-store b (lambda () (list))) (Listof Symbol)))
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
                (cond [(or broken (breaker-use-formula bkr))
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
                    (define sub-upper (ann (sbound-upper sub-sbound) (Setof Tuple)))

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
                #f
            )
        ])
    )
)

; (define (co f)
;     (λ (pri rel bound atom-lists rel-list [loc #f])
;         (define sub-breaker (f pri (@~ rel) bound (reverse atom-lists) (reverse rel-list) loc))
;         (breaker pri
;             (breaker-break-graph sub-breaker)
;             (λ () 
;                 ; unpack results of sub-breaker
;                 (define sub-break ((breaker-make-break sub-breaker)))
;                 (define sub-formulas (break-formulas sub-break))
;                 (define sub-sbound (break-sbound sub-break))
;                 (define sub-lower (sbound-lower sub-sbound))
;                 (define sub-upper (sbound-upper sub-sbound))
;                 ; reverse all tuples in sbounds 
;                 (define lower (for/set ([l sub-lower]) (reverse l)))
;                 (define upper (for/set ([l sub-upper]) (reverse l)))
;                 (define bound (sbound rel lower upper))

;                 (break bound sub-formulas)
;             )
;             (λ ()
;                 ((breaker-make-default sub-breaker))
;             )
;         )
;     )
; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; define breaks and compositions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A->A Strategies ;;;
; (add-strategy 'irref (λ (pri rel bound atom-lists rel-list [loc #f]) 
;     (define atoms (first atom-lists))
;     (define sig (first rel-list))
;     (breaker pri
;         (break-graph (set) (set))
;         (λ () 
;             (make-upper-break rel
;                             (filter-not (lambda (x) (equal? (first x) (second x)))
;                                         (apply cartesian-product atom-lists))))
;         (λ () (break bound (set
;             (@no/info (just-location-info loc) (@& @iden rel))
;         )))
;     )
; ))
; (add-strategy 'ref (λ (pri rel bound atom-lists rel-list [loc #f]) 
;     (define atoms (first atom-lists))
;     (define sig (first rel-list))
;     (breaker pri
;         (break-graph (set) (set))
;         (λ () 
;             (make-lower-break rel
;                             (filter     (lambda (x) (equal? (first x) (second x)))
;                                         (apply cartesian-product atom-lists))
;                             atom-lists))
;         (λ () (break bound (set
;             (@all/info (just-location-info loc) ([x sig])
;                 (@in x (@join sig rel))
;             )
;         )))
;     )
; ))


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
        #f
    )
))

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
        #f
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
    
    ; OLD CODE
;     (if (equal? A B)
; ; ORIGINAL CODE
;         ; (breaker pri ; TODO: can improve, but need better symmetry-breaking predicates
;         ;     (break-graph (set A) (set))
;         ;     (λ () (break ;(bound->sbound bound) formulas))
;         ;         (sbound rel
;         ;             (set)
;         ;             ;(for*/set ([a (length As)]
;         ;             ;           [b (length Bs)] #:when (<= b (+ a 1)))
;         ;             ;    (list (list-ref As a) (list-ref Bs b))))
;         ;             (set-add (cartesian-product (cdr As) Bs) (list (car As) (car Bs))))
;         ;         formulas))
;         ;     (λ () (break bound formulas))
;         ; )
;         ; (breaker pri ; TODO: can improve, but need better symmetry-breaking predicates
;         ;     (break-graph (set B) (set (set A B)))   ; breaks B and {A,B}
;         ;     (λ () 
;         ;         ; assume wlog f(a) = b for some a in A, b in B
;         ;         (break 
;         ;             (sbound rel
;         ;                 (set (list (car As) (car Bs)))
;         ;                 (set-add (cartesian-product (cdr As) Bs) (list (car As) (car Bs))))
;         ;             formulas))
;         ;     (λ () (break bound formulas))
;         ; )
; ; BEGIN INSERTED TEMPORARY FIX FOR 'FUNC
;         (formula-breaker pri ; TODO: can improve, but need better symmetry-breaking predicates
;             (break-graph (set A) (set))
;             (λ () (break ;(bound->sbound bound) formulas))
;                 (sbound rel
;                     (set)
;                     ;(for*/set ([a (length As)]
;                     ;           [b (length Bs)] #:when (<= b (+ a 1)))
;                     ;    (list (list-ref As a) (list-ref Bs b))))
;                     (set-add (cartesian-product (cdr As) Bs) (list (car As) (car Bs))))
;                 formulas))
;             (λ () (break bound formulas))
;         )
;         (formula-breaker pri ; TODO: can improve, but need better symmetry-breaking predicates
;             (break-graph (set B) (set (set A B)))   ; breaks B and {A,B}
;             (λ () 
;                 ; assume wlog f(a) = b for some a in A, b in B
;                 (break 
;                     (sbound rel
;                         (set (list (car As) (car Bs)))
;                         (set-add (cartesian-product (cdr As) Bs) (list (car As) (car Bs))))
;                     formulas))
;             (λ () (break bound formulas))
;         )
; END INSERTED TEMPORARY FIX FOR 'FUNC
        (breaker pri 
            (break-graph (set) (set))
            (λ () (break (bound->sbound bound) formulas))
            ;; TYPES
            ;(λ () (break bound formulas))
            (λ () (break (bound->sbound bound) formulas))
            #f)))

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
                
    ; OLD CODE
    ; (if (equal? A B)
    ;     (formula-breaker pri ; TODO: can improve, but need better symmetry-breaking predicates
    ;         (break-graph (set A) (set))
    ;         (λ () (break ;(bound->sbound bound) formulas))
    ;             (sbound rel
    ;                 (set)
    ;                 ;(for*/set ([a (length As)]
    ;                 ;           [b (length Bs)] #:when (<= b (+ a 1)))
    ;                 ;    (list (list-ref As a) (list-ref Bs b))))
    ;                 (set-add (cartesian-product (cdr As) Bs) (list (car As) (car Bs))))
    ;             formulas))
    ;         (λ () (break bound formulas)))
    ;     (formula-breaker pri ; TODO: can improve, but need better symmetry-breaking predicates
    ;         (break-graph (set B) (set (set A B)))   ; breaks B and {A,B}
    ;         (λ () 
    ;             ; assume wlog f(a) = b for some a in A, b in B
    ;             (break 
    ;                 (sbound rel
    ;                     (set (list (car As) (car Bs)))
    ;                     (set-add (cartesian-product (cdr As) Bs) (list (car As) (car Bs))))
    ;                 formulas))
    ;         (λ () (break bound formulas))))
        (breaker pri
            (break-graph (set) (set))
            (λ () (break (bound->sbound bound) formulas))
            (λ () (break (bound->sbound bound) formulas))
            #f
        )
    ))

; ;(: surj-strategy StrategyFunction)
; (define surj-strategy (λ ([pri : Number] [rel : Any] [bound : bound] [atom-lists : (NonEmptyListOf (Listof Any))] [rel-list : (NonEmptyListOf Any)] [loc #f]) 
;     (define A (first rel-list))
;     (define B (second rel-list))
;     (define As (first atom-lists))
;     (define Bs (second atom-lists))  
;     (define formulas (set 
;         (@all/info (just-location-info loc) ([a A]) (@one/info (just-location-info loc)  (@join a rel)))    ; @one
;         (@all/info (just-location-info loc) ([b B]) (@some/info (just-location-info loc) (@join rel b)))    ; @some
;     ))
;     (if (equal? A B)
;         (breaker pri ; TODO: can improve, but need better symmetry-breaking predicates
;             (break-graph (set) (set))
;             (λ () (break (bound->sbound bound) formulas))
;             (λ () (break bound formulas))
;         )
;         (breaker pri ; TODO: can improve, but need better symmetry-breaking predicates
;             (break-graph (set) (set (set A B)))   ; breaks only {A,B}
;             (λ () 
;                 ; assume wlog f(a) = b for some a in A, b in B
;                 (break 
;                     (sbound rel
;                         (set (list (car As) (car Bs)))
;                         (set-add (cartesian-product (cdr As) Bs) (list (car As) (car Bs))))
;                     formulas))
;             (λ () (break bound formulas))
;         )
;     )
; ))
; (add-strategy 'surj surj-strategy)

; (add-strategy 'inj (λ (pri rel bound atom-lists rel-list [loc #f]) 
;     (define A (first rel-list))
;     (define B (second rel-list))
;     (define As (first atom-lists))
;     (define Bs (second atom-lists))  
;     (define formulas (set 
;         (@all/info (just-location-info loc) ([a A]) (@one/info (just-location-info loc)  (@join a rel)))    ; @one
;         (@all/info (just-location-info loc) ([b B]) (@lone/info (just-location-info loc) (@join rel b)))    ; @lone
;     ))
;     (if (equal? A B)
;         (breaker pri ; TODO: can improve, but need better symmetry-breaking predicates
;             (break-graph (set) (set))
;             (λ () (break (bound->sbound bound) formulas))
;             (λ () (break bound formulas))
;         )
;         (breaker pri ; TODO: can improve, but need better symmetry-breaking predicates
;             (break-graph (set B) (set (set A B)))   ; breaks B and {A,B}
;             (λ () 
;                 ; assume wlog f(a) = b for some a in A, b in B
;                 (break 
;                     (sbound rel
;                         (set (list (car As) (car Bs)))
;                         (set-add (cartesian-product (cdr As) (cdr Bs)) (list (car As) (car Bs))))
;                     formulas))
;             (λ () (break bound formulas))
;         )
;     )
; ))
; (add-strategy 'bij (λ (pri rel bound atom-lists rel-list [loc #f]) 
;     (define A (first rel-list))
;     (define B (second rel-list))
;     (define As (first atom-lists))
;     (define Bs (second atom-lists))  
;     (define formulas (set 
;         (@all/info (just-location-info loc) ([a A]) (@one/info (just-location-info loc)  (@join a rel)))    ; @one
;         (@all/info (just-location-info loc) ([b B]) (@one/info (just-location-info loc)  (@join rel b)))    ; @one
;     ))
;     (if (equal? A B)
;         (breaker pri ; TODO: can improve, but need better symmetry-breaking predicates
;             (break-graph (set) (set))
;             (λ () (break (bound->sbound bound) formulas))
;             (λ () (break bound formulas))
;         )
;         (breaker pri ; TODO: can improve, but need better symmetry-breaking predicates
;             (break-graph (set) (set (set A B)))   ; breaks only {A,B}
;             (λ () (make-exact-break rel (map list As Bs)))
;             (λ () (break bound formulas))
;         )
;     )
; ))
; (add-strategy 'pbij (λ (pri rel bound atom-lists rel-list [loc #f]) 
;     (define A (first rel-list))
;     (define B (second rel-list))
;     (define As (first atom-lists))
;     (define Bs (second atom-lists))  
;     (define LA (length As))
;     (define LB (length Bs))
;     (define broken (cond [(> LA LB) (set A)]
;                          [(< LA LB) (set B)]
;                          [else (set)]))
;     ;(printf "broken : ~v~n" broken)
;     (define formulas (set 
;         (@all/info (just-location-info loc) ([a A]) (@one/info (just-location-info loc) (@join a rel)))    ; @one
;         (@all/info (just-location-info loc) ([b B]) (@one/info (just-location-info loc) (@join rel b)))    ; @one
;     ))
;     (if (equal? A B)
;         (breaker pri ; TODO: can improve, but need better symmetry-breaking predicates
;             (break-graph (set) (set))
;             (λ () (break (bound->sbound bound) formulas))
;             (λ () (break bound formulas))
;         )
;         (breaker pri ; TODO: can improve, but need better symmetry-breaking predicates
;             (break-graph broken (set (set A B)))   ; breaks only {A,B}
;             (λ () (make-upper-break rel (for/list ([a As][b Bs]) (list a b)) formulas))
;             (λ () (break bound formulas))
;         )
;     )
; ))

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
      #f)
    (raise (format "Internal error in breaks.defaultStrategy. Given: ~a; from:~a~n" rel loc)))))

(add-strategy 'default defaultStrategy)

; (add-strategy 'cotree (variadic 2 (co (hash-ref strategies 'tree))))
; (add-strategy 'cofunc (variadic 2 (co (hash-ref strategies 'func))))
; (add-strategy 'cosurj (variadic 2 (co (hash-ref strategies 'surj))))
; (add-strategy 'coinj (variadic 2 (co (hash-ref strategies 'inj))))

; (add-strategy 'irref (variadic 2 (hash-ref strategies 'irref)))
; (add-strategy 'ref (variadic 2 (hash-ref strategies 'ref)))

; this one cannot afford to go to purely formula break
(add-strategy 'linear (variadic 2 (hash-ref strategies 'linear)))
(add-strategy 'plinear (variadic 2 (hash-ref strategies 'plinear)))



; (add-strategy 'acyclic (variadic 2 (hash-ref strategies 'acyclic)))
; (add-strategy 'tree (variadic 2 (hash-ref strategies 'tree)))
(add-strategy 'func (hash-ref strategies 'func))
(add-strategy 'pfunc (hash-ref strategies 'pfunc))
; (add-strategy 'surj (variadic 2 (hash-ref strategies 'surj)))
; (add-strategy 'inj (variadic 2 (hash-ref strategies 'inj)))
; (add-strategy 'bij (variadic 2 (hash-ref strategies 'bij)))
; (add-strategy 'pbij (variadic 2 (hash-ref strategies 'pbij)))


;;; Domination Order ;;;
; (declare 'linear > 'tree)
; (declare 'tree > 'acyclic)
; (declare 'acyclic > 'irref)
; (declare 'func < 'surj 'inj 'pfunc)
(declare 'func < 'pfunc)
; (declare 'bij = 'surj 'inj)
; (declare 'linear = 'tree 'cotree)
; (declare 'bij = 'func 'cofunc)
; (declare 'cofunc < 'cosurj 'coinj)
; (declare 'bij = 'cosurj 'coinj)

(provide get-co)
(define co-map (make-hash))
(hash-set! co-map 'tree 'cotree)
(hash-set! co-map 'func 'cofunc)
(hash-set! co-map 'surj 'cosurj)
(hash-set! co-map 'inj 'coinj)
(hash-set! co-map 'tree 'cotree)
(for ([(k v) (in-hash co-map)]) (hash-set! co-map v k))
(for ([sym '('bij 'pbij 'linear 'plinear 'ref 'irref 'acyclic)]) (hash-set! co-map sym sym))
(define (get-co sym) (hash-ref co-map sym))



#|
ADDING BREAKS
- add breaks here with using add-strategy and the declare forms:
    - (declare a > bs ...)
    - (declare a < bs ...)
    - (declare a = bs ...)
- note that your break can likely compose with either 'ref or 'irref because they don't break syms
    - so don't forget to declare that
- declarations will be inferred automatically when possible:
    - a > b        |- a = a + b
    - a > b, b > c |- a > c
- note, however:
    - a = a + b   !|- a > b
- add a call to make-breaker to the bottom of ast.rkt for your new breaker

TODO:
- prove correctness
- add extra formulas to further break symmetries because kodkod can't once we've broken bounds
    - improve all functional strategies (see func A->A case for commented working example)
- allow strategies to be passed multiple values, return values, split sigs
- strategy combinators
    - naive equiv strategies
        - can be used to combine many strats with ref/irref, even variadic ones
        - use in equiv if sum isn't defined
- more strats
    - lasso
    - loop
    - loops
    - unique init/term
    - unique init/term + acyclic
    - has init/term
    - more partial breaks
|#


; (require (except-in xml attribute))

; (define (xml->breakers xml name-to-rel)
;     (set! xml (xml->xexpr (document-element (read-xml (open-input-string xml)))))
;     (define (read-label info)
;         (define label #f)
;         (define builtin #f)
;         (for/list ([i info]) (match i
;             [(list 'label l) (set! label l)]
;             [(list 'builtin "yes") (set! builtin #t)]
;             [else #f]
;         ))
;         (if builtin #f (hash-ref name-to-rel label))
;     )
;     (define (read-atoms atoms) 
;         (filter identity (for/list ([a atoms]) (match a
;             [(list atom (list (list 'label l))) (string->symbol l)]
;             [else #f]
;         )))
;     )
;     (define (read-tuples tuples)
;         (list->set (filter identity (for/list ([t tuples]) (match t
;             [(list 'tuple atoms ...) (read-atoms atoms)]
;             [else #f]
;         ))))
;     )
;     (define (read-rel x) (match x
;         [(list 'sig info atoms ...) 
;             (define sig (read-label info))
;             (if sig (make-exact-sbound sig (map list (read-atoms atoms))) #f)]
;         [(list 'field info tuples ...) (make-exact-sbound (read-label info) (read-tuples tuples))]
;         [else #f]
;     ))

;     (when (equal? (first xml) 'alloy) (for ([x xml]) (match x
;         [(list 'instance _ ...) (set! xml x)]
;         [else #f]
;     )))
;     (match xml
;         [(list 'instance _ ...)  (filter identity (for/list ([x xml]) (read-rel x)))]
;         [else (list (read-rel xml))]
;     )
; )