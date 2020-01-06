#lang racket

(require "lang/bounds.rkt" (prefix-in @ "lang/ast.rkt"))
(require predicates)
(require data/union-find)

(provide constrain-bounds (rename-out [break-rel break]) break-bound break-formulas)

;;;;;;;;;;;;;;
;;;; util ;;;;
;;;;;;;;;;;;;;

(define-syntax-rule (cons! xs x) (set! xs (cons x xs)))
(define-syntax-rule (add1! x)    (set! x  (add1 x)))

;;;;;;;;;;;;;;;;
;;;; breaks ;;;;
;;;;;;;;;;;;;;;;

(struct sbound (relation lower upper) #:transparent)
(define (make-sbound relation lower [upper false]) (sbound relation lower upper))
(struct break (sbound formulas) #:transparent)
(define (make-break sbound [formulas (set)]) (break sbound formulas))

; sigs  :: set<sig>
; edges :: set<set<sig>> ; technically hyperedges
(struct break-graph (sigs edges) #:transparent)

; pri               :: Nat
; break-graph       :: break-graph
; make-break        :: () -> break
; make-default      :: () -> break
(struct breaker (pri break-graph make-break make-default) #:transparent)

(define (bound->sbound bound) 
    (make-sbound (bound-relation bound)
                (list->set (bound-lower bound))
                (list->set (bound-upper bound))))
(define (sbound->bound sbound) 
    (make-bound (sbound-relation sbound)
                (set->list (sbound-lower sbound))
                (set->list (sbound-upper sbound))))
(define (bound->break bound) (break (bound->sbound bound) (set)))
(define break-lower    (compose sbound-lower    break-sbound))
(define break-upper    (compose sbound-upper    break-sbound))
(define break-relation (compose sbound-relation break-sbound))
(define break-bound    (compose sbound->bound   break-sbound))

(define (sbound+ . sbounds)
    (make-bound (break-relation (first sbounds)) ; TODO: assert all same relations
                (apply set-union     (map break-lower sbounds))
                (apply set-intersect (map break-lower sbounds)))
)
(define (break+ . breaks)
    (make-break (apply sbound+ breaks)
                (apply set-union (map break-formulas breaks)))
)

(define (make-exact-break relation contents [formulas (set)])
  (break (sbound relation contents contents) formulas))
(define (make-upper-break relation contents [formulas (set)])
  (break (sbound relation (set) contents) formulas))
(define (make-lower-break relation contents atom-lists [formulas (set)])
  (break (sbound relation contents (apply cartesian-product atom-lists)) formulas))

;;;;;;;;;;;;;;
;;;; data ;;;;
;;;;;;;;;;;;;;

; symbol |-> (pri rel bound atom-lists rel-list) -> breaker
(define strategies (make-hash))
; compos[{a₀,...,aᵢ}] = b => a₀+...+aᵢ = b
(define compos (make-hash))
; a ∈ upsets[b] => a > b
(define upsets (make-hash))
; a ∈ downsets[b] => a < b
(define downsets (make-hash))

; a ∈ rel-breaks[r] => "user wants to break r with a"
(define rel-breaks (make-hash))
; rel-break-pri[r][a] = i => "breaking r with a has priority i"
(define rel-break-pri (make-hash))
; priority counter
(define pri_c 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; methods for defining breaks ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; h :: type(k) |-> set<type(v)>
(define (hash-add! h k v)
    (if (hash-has-key? h k)
        (set-add! (hash-ref h k) v)
        (hash-set! h k (mutable-set v))))

; h :: type(k1) |-> type(k2) |-> type(v)
(define (hash-add-set! h k1 k2 v)
    (unless (hash-has-key? h k1) (hash-set! h k1 (make-hash)))
    (define h_k1 (hash-ref h k1))
    (unless (hash-has-key? h_k1 k2) (hash-set! h_k1 k2 pri_c)))

; strategy :: () -> breaker
(define (add-strategy a strategy)
    (hash-set! strategies a strategy)
    (hash-add! upsets a a)      ;; a > a
    (hash-add! downsets a a))   ;; a < a
(define (equiv a . bs) 
    (hash-set! compos (apply set bs) a)
    (apply stricter a bs)
    ; TODO: if no fn defined for a, default to naively doing all bs
    #|(unless (hash-has-key? strategies a)
            (hash-set! strategies a (λ (rel atom-lists rel-list)
                (apply break+ (for ([b bs]) 
                    ((hash-ref strategies b) atom-lists)
                ))
            )))|#
)
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
(define (stricter a . bs) (for ([b bs]) (dominate a b)))
(define (weaker a . bs) (for ([b bs]) (dominate b a)))

; TODO: allow syntax like (declare 'a 'b > 'c 'd > 'e 'f)
(define-syntax declare
  (syntax-rules (> < =)
    [(_ a > bs ...) (stricter a bs ...)]
    [(_ a < bs ...) (weaker a bs ...)]
    [(_ a = bs ...) (equiv a bs ...)]))


(define (min-breaks! breaks break-pris)
    (define changed false)
    (hash-for-each compos (λ (k v)
        (when (subset? k breaks)
              (set-subtract! breaks k)
              (set-add! breaks v)
              ; new break should have priority of highest priority component
              (define max-pri (apply min 
                (set-map k (lambda (s) (hash-ref break-pris s)))))
              (hash-set! break-pris v max-pri)
              (set! changed true))
    ))
    (when changed (min-breaks! breaks break-pris))
)

(define (break-rel rel . breaks) ; renamed-out to 'break for use in forge
    (for ([break breaks]) 
        (unless (hash-has-key? strategies break) (error "break not implemented:" break))
        (hash-add! rel-breaks rel break)
        (set! pri_c (add1 pri_c))
        (hash-add-set! rel-break-pri rel break pri_c)))

(define (constrain-bounds total-bounds sigs bounds-store relations-store extensions-store) 
    ; returns (values new-total-bounds (set->list formulas))
    (define new-total-bounds (list))
    (define formulas (mutable-set))
    ; unextended sets
    (define unbroken-sigs (list->mutable-set sigs))
    (hash-for-each extensions-store (λ (k v) (set-remove! unbroken-sigs v)))    

    ; proposed breaks from each relation
    (define candidates (list))

    (for ([bound total-bounds])
        ; get declared breaks for the relation associated with this bound        
        (define rel (bound-relation bound))
        (define breaks (hash-ref rel-breaks rel (set)))
        (define break-pris (hash-ref rel-break-pri rel (make-hash)))
        ; compose breaks
        (min-breaks! breaks break-pris)

        (cond [(set-empty? breaks)
            (cons! new-total-bounds bound)]
        [else
            (define rel-list (hash-ref relations-store rel))
            (define atom-lists (map (λ (b) (hash-ref bounds-store b)) rel-list))

            ; make all breakers
            (define breakers (for/list ([sym (set->list breaks)]) 
                (define strategy (hash-ref strategies sym))
                (define pri (hash-ref break-pris sym))
                (strategy pri rel bound atom-lists rel-list)
            ))
            (set! breakers (sort breakers < #:key breaker-pri))

            ; propose highest pri breaker that breaks only leaf sigs
            ; break the rest the default way (with get-formulas)
            (define broken #f)
            (for ([breaker breakers])
                (cond [broken
                    (define default ((breaker-make-default breaker)))
                    (set-union! formulas (break-formulas default))
                ][else
                    (define break-graph (breaker-break-graph breaker))
                    (define broken-sigs (break-graph-sigs break-graph))
                    (cond [(subset? broken-sigs unbroken-sigs)
                        (cons! candidates breaker)
                        (set! broken #t)
                    ][else
                        (define default ((breaker-make-default breaker)))
                        (set-union! formulas (break-formulas default))
                    ])
                ])
            )
            (unless broken (cons! new-total-bounds bound))
        ])     
    )
    (set! candidates (sort candidates < #:key breaker-pri))

    #|
        Now we try to use candidate breakers, starting with highest priority.
        We maintain an equivalence relation on sigs, with some sigs being broken.
        If applying a breaker would create a loop or break a broken sig,
        the breaker isn't applied and the default formulas are used instead.
        Otherwise, we do the break and update the equivalence classes.

        The approach used to check if a breaker would create a loop is a
        generalization of the one used in Kruskal's MST algorithm. Instead of
        checking a single candidate edge in each step, we must check a set of
        candidate edges together. This requires the construction of an eqivalence
        relation over the equivalence classes of sigs.
        Consider: {(A,B),(C,D)} + {(A,C),(B,D)} creates a loop.
    |#

    ; equivalence relation on sigs
    (define sig2class (make-hash))
    (for ([sig sigs]) (hash-set! sig2class sig (uf-new sig)))

    (for ([breaker candidates])
        (define break-graph (breaker-break-graph breaker))
        (define broken-sigs (break-graph-sigs break-graph))
        (define broken-edges (break-graph-edges break-graph))

        ; equivalence relation on equivalence classes
        (define canon2clocl (make-hash))
        (hash-for-each sig2class (λ (sig class) (when (equal? sig (uf-find class))
            (hash-set! canon2clocl sig (uf-new sig)))))

        (define acceptable (subset? broken-sigs unbroken-sigs))

        (when acceptable
            (for ([edge broken-edges])
                (define clocl #f)
                (for ([sig edge])
                    (define class (hash-ref sig2class sig))
                    (define canon (uf-find class))
                    (define clocl2 (hash-ref canon2clocl canon))

                    (if clocl
                        (if (uf-same-set? clocl clocl2) ; breaks loop criteria
                            (set! acceptable #f)
                            (uf-union! clocl clocl2))
                        (set! clocl clocl2))
                    (hash-set! canon2clocl canon clocl)
                )
            )
        )

        (cond [acceptable
            (define break ((breaker-make-break breaker)))
            (cons! new-total-bounds (break-bound break))
            (set-union! formulas (break-formulas break))
            ; squash the clocls
            (hash-for-each canon2clocl (λ (canon clocl)
                (define canon2 (uf-find clocl))
                (define class  (hash-ref sig2class canon))
                (define class2 (hash-ref sig2class canon2))
                (uf-union! class class2)
            ))
            (set-subtract! unbroken-sigs broken-sigs)
        ][else
            (define default ((breaker-make-default breaker)))
            (cons! new-total-bounds (break-sbound default))
            (set-union! formulas (break-formulas default))
        ])
    )

    (values new-total-bounds (set->list formulas))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; define breaks and compositions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A->A Strategies ;;;
(add-strategy 'irref (λ (pri rel bound atom-lists rel-list) (breaker pri
    (break-graph (set) (set))
    (λ () 
        (make-upper-break rel
                        (filter-not (lambda (x) (equal? (first x) (second x)))
                                    (apply cartesian-product atom-lists))))
    (λ () (break bound (set)))
)))
(add-strategy 'ref (λ (pri rel bound atom-lists rel-list) (breaker pri
    (break-graph (set) (set))
    (λ () 
        (make-lower-break rel
                        (filter     (lambda (x) (equal? (first x) (second x)))
                                    (apply cartesian-product atom-lists))
                        atom-lists))
    (λ () (break bound (set)))
)))
(add-strategy 'linear (λ (pri rel bound atom-lists rel-list) 
    (define atoms (first atom-lists))
    (define sig (first rel-list))
    (breaker pri
        (break-graph (set sig) (set))
        (λ () (make-exact-break rel (map list (drop-right atoms 1) (cdr atoms))))
        (λ () (break bound (set)))
    )
))
(add-strategy 'acyclic (λ (pri rel bound atom-lists rel-list) (breaker pri
    (break-graph (set) (set))
    (λ ()
        (define atoms (first atom-lists))
        (make-upper-break rel
                        (for*/list ([i (length atoms)]
                                    [j (length atoms)]
                                    #:when (< i j))
                                (list (list-ref atoms i) (list-ref atoms j)))))
    (λ () (break bound (set)))
)))
(add-strategy 'tree (λ (pri rel bound atom-lists rel-list) (breaker pri
    (break-graph (set) (set))
    (λ ()
        (define atoms (first atom-lists))
        (define rel2 (first rel-list))
        (make-break 
            (bound->sbound (make-upper-bound rel
                        (for*/list ([i (length atoms)]
                                    [j (length atoms)]
                                    #:when (< i j))
                                (list (list-ref atoms i) (list-ref atoms j)))))
            (set
                (@some ([n rel2]) 
                    (@all ([m (@- rel2 n)]) 
                        (@one (@join rel m))
                    )
                )
            )))
    (λ () (break bound (set)))
)))

;;; A->B Strategies ;;;
(add-strategy 'func (λ (pri rel bound atom-lists rel-list) 
    (define A (first rel-list))
    (define B (second rel-list))
    (define As (first atom-lists))
    (define Bs (second atom-lists))  
    (define formulas (set 
        (@all ([a A]) (@one (@join a rel)))
    ))
    (breaker pri
        (break-graph (set B) (set (set A B)))   ; breaks B and {A,B}
        (λ () 
            ; assume wlog f(a) = b for some a in A, b in B
            (break 
                (sbound rel
                    (set (list (car As) (car Bs)))
                    (set-add (cartesian-product (cdr As) Bs) (list (car As) (car Bs))))
                formulas))
        (λ () (break bound formulas))
    )
))
(add-strategy 'surj (λ (pri rel bound atom-lists rel-list) 
    (define A (first rel-list))
    (define B (second rel-list))
    (define As (first atom-lists))
    (define Bs (second atom-lists))  
    (define formulas (set 
        (@all ([a A]) (@one  (@join a rel)))
        (@all ([b B]) (@some (@join rel b)))    ; @some
    ))
    (breaker pri
        (break-graph (set) (set (set A B)))   ; breaks only {A,B}
        (λ () 
            ; assume wlog f(a) = b for some a in A, b in B
            (break 
                (sbound rel
                    (set (list (car As) (car Bs)))
                    (set-add (cartesian-product (cdr As) Bs) (list (car As) (car Bs))))
                formulas))
        (λ () (break bound formulas))
    )
))
(add-strategy 'inj (λ (pri rel bound atom-lists rel-list) 
    (define A (first rel-list))
    (define B (second rel-list))
    (define As (first atom-lists))
    (define Bs (second atom-lists))  
    (define formulas (set 
        (@all ([a A]) (@one  (@join a rel)))
        (@all ([b B]) (@lone (@join rel b)))    ; @lone
    ))
    (breaker pri
        (break-graph (set B) (set (set A B)))   ; breaks B and {A,B}
        (λ () 
            ; assume wlog f(a) = b for some a in A, b in B
            (break 
                (sbound rel
                    (set (list (car As) (car Bs)))
                    (set-add (cartesian-product (cdr As) (cdr Bs)) (list (car As) (car Bs))))
                formulas))
        (λ () (break bound formulas))
    )
))
(add-strategy 'bij (λ (pri rel bound atom-lists rel-list) 
    (define A (first rel-list))
    (define B (second rel-list))
    (define As (first atom-lists))
    (define Bs (second atom-lists))  
    (define formulas (set 
        (@all ([a A]) (@one  (@join a rel)))
        (@all ([b B]) (@one  (@join rel b)))    ; @one
    ))
    (breaker pri
        (break-graph (set) (set (set A B)))   ; breaks only {A,B}
        (λ () (make-exact-break rel (map list As Bs)))
        (λ () (break bound formulas))
    )
))


; use to prevent breaks
(add-strategy 'default (λ (pri rel bound atom-lists rel-list) (breaker pri
    (break-graph (set) (set))
    (λ () 
        (make-upper-break rel (apply cartesian-product atom-lists)))
    (λ () (break bound (set)))
)))


;;; Domination Order ;;;
(declare 'linear > 'tree)
(declare 'tree > 'acyclic)
(declare 'acyclic > 'irref)
(declare 'func < 'surj 'inj)
(declare 'bij = 'surj 'inj)



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

TODO
- test sound strat composition with A->B strats
- generalize strats to higher arities: A->B ==> S->A->B
- co- strategies
- naive equiv strategies
- more strats
    - loop
    - loops
    - unique init/term
    - unique init/term + acyclic
    - has init/term
|#




