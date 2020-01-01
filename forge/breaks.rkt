#lang racket

(require "lang/bounds.rkt" (prefix-in @ "lang/ast.rkt"))
(require predicates)
(require data/union-find)

(provide constrain-bounds (rename-out [break-rel break]) break-bound break-formulas)

;;;;;;;;;;;;;;
;;;; util ;;;;
;;;;;;;;;;;;;;

; TODO: use this
(define-syntax-rule (set-cons! xs x) (set! xs (cons x xs)))

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

; elem2class    :: A |-> uf-set<A>
; broken        :: mut-set<A> ; set of broken canonical elements
(struct eq-rel (elem2class broken))
(define (make-eq-rel) (eq-rel (make-hash) (mutable-set)))
(define (in-broken? e-r x) 
    (define elem2class (eq-rel-elem2class e-r))
    (define broken (eq-rel-broken e-r))
    (define class (hash-ref elem2class x))
    (define canon (uf-find class))
    (set-member? broken canon)
)
; union two classes and mark as broken if either is broken
(define (e-r-union! e-r c1 c2)
    (define broken (eq-rel-broken e-r))
    (define canon1 (uf-find c1))
    (define canon2 (uf-find c2))
    (define is-broken (or (set-member? broken canon1)
                          (set-member? broken canon2)))
    (uf-union! c1 c2)
    (define canon3 (uf-find c1))
    (when is-broken (set-add! broken canon3))
)
; (define (compose ))

; pri               :: Nat
; break-graph       :: break-graph
; make-break        :: () -> break
; make-formulas     :: () -> set<formula>
(struct breaker (pri break-graph make-break make-formulas) #:transparent)

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

(define (make-exact-break relation contents)
  (make-break (make-sbound relation contents contents)))
(define (make-upper-break relation contents)
  (make-break (make-sbound relation (set) contents)))
(define (make-lower-break relation contents atom-lists)
  (make-break (make-sbound relation contents (apply cartesian-product atom-lists))))

;;;;;;;;;;;;;;
;;;; data ;;;;
;;;;;;;;;;;;;;

; symbol |-> (rel atom-lists rel-list) -> break
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
    ; if no fn defined for a, default to naively doing all bs
    (unless (hash-has-key? strategies a)
            (hash-set! strategies a (λ (rel atom-lists rel-list)
                (apply break+ (for ([b bs]) 
                    ((hash-ref strategies b) atom-lists)
                ))
            )))
)
(define (dominate a b)  
    (define upa (hash-ref upsets a))
    (define downb (hash-ref downsets b))
    (for ([x (in-set upa)])         ;; x > a
        (hash-add! upsets b x)      ;; x > b
        (hash-add! downsets x b)    ;; b < x
        (equiv x x b)               ;; x = x + b
    )
    (for ([x (in-set downb)])       ;; x < b
        (hash-add! downsets a x)    ;; x < a
        (hash-add! upsets x a)      ;; a > x
        (equiv a a x)               ;; a = a + x
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
                (map (lambda (s) (hash-ref break-pris s)) k)))
              (hash-set! break-pris v max-pri)
              (set! changed true))
    ))
    (when changed (min-breaks! breaks))
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
    (define leaf-sigs (list->mutable-set sigs))
    (hash-for-each extensions-store (λ (k v) (set-remove! leaf-sigs v)))    

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
            (set-cons! new-total-bounds bound)]
        [else
            (define rel-list (hash-ref relations-store rel))
            (define atom-lists (map (λ (b) (hash-ref bounds-store b)) rel-list))

            ; make all breakers
            (define breakers (for/list ([sym (set->list breaks)]) 
                (define strategy (hash-ref strategies sym))
                (define pri (hash-ref break-pris sym))
                (strategy pri rel atom-lists rel-list)
            ))
            (set! breakers (sort breakers < #:key breaker-pri))

            ; propose highest pri breaker that breaks only leaf sigs
            ; break the rest the default way (with get-formulas)
            (define broken #f)
            (for ([breaker breakers])
                (cond [broken
                    (set-union! formulas ((breaker-make-formulas breaker)))
                ][else
                    (define break-graph (breaker-break-graph breaker))
                    (define broken-sigs (break-graph-sigs break-graph))
                    ; (define broken-sigs ((breaker-break-graph breaker)))
                    (cond [(subset? broken-sigs leaf-sigs)
                        (set-cons! candidates breaker)
                        (set! broken #t)
                    ][else
                        (set-union! formulas ((breaker-make-formulas breaker)))
                    ])
                ])
            )
            (unless broken (set-cons! new-total-bounds bound))
        ])     
    )
    (set! candidates (sort candidates < #:key breaker-pri))

    #|
        Now we try to use candidate breakers, starting with highest priority.
        We maintain an equivalence class on sigs, with some classes being broken.
        If applying a breaker would merge 2 broken classes or break a sig in a broken class,
        the breaker isn't applied and the default formulas are used instead.
        Otherwise, we do the break and update the equivalence classes.
    |#

    (define e-r (make-eq-rel))
    (define sig2class (eq-rel-elem2class e-r))
    (define broken-classes (eq-rel-broken e-r))
    (for ([sig sigs]) (hash-set! sig2class sig (uf-new sig)))

    (for ([breaker candidates])
        (define break-graph (breaker-break-graph breaker))
        (define broken-sigs (break-graph-sigs break-graph))
        (define broken-edges (break-graph-edges break-graph))

        ; equivalence relation on equivalnce classes
        (define e-r-2 (make-eq-rel))
        (define canon2clocl (eq-rel-elem2class e-r-2))
        (define broken-clocls (eq-rel-broken e-r-2))
        (hash-for-each sig2class (λ (sig class) 
            (hash-set! canon2clocl (uf-find class) (uf-new class)))) 

        (for ([edge broken-edges])
            (define clocl #f)
            (define broken #f)
            (for ([sig edge])
                (when (set-member? broken-sigs sig) (set! broken #t))

                (define class (hash-ref sig2class sig))
                (define canon (uf-find class))
                (define clocl2 (hash-ref canon2clocl canon))
                (if clocl (e-r-union! clocl clocl2) (set! clocl clocl2))
                (hash-set canon2clocl canon clocl)
            )
            (when broken (set-add! broken-clocls (uf-find clocl)))
        )

        ; TODO: for each clocl, if >1 of {its classes} ∪ {it} are broken, abort.
        ;       otherwise, apply the break and squash the classes.
        ;       if exactly 1 of those is broken, set the squashed class as broken.

        (define acceptable #t)

        (define break ((breaker-make-break breaker)))
        (set-cons! new-total-bounds (break-bound break))
        (set-union! formulas (break-formulas break))
    )

    (values new-total-bounds (set->list formulas))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; define breaks and compositions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-strategy 'irref (λ (pri rel atom-lists rel-list) (breaker pri
    (break-graph (set) (set))
    (λ () 
        (make-upper-break rel
                        (filter-not (lambda (x) (equal? (first x) (second x)))
                                    (apply cartesian-product atom-lists))))
    (λ () (set))
)))
(add-strategy 'ref (λ (pri rel atom-lists rel-list) (breaker pri
    (break-graph (set) (set))
    (λ () 
        (make-lower-break rel
                        (filter     (lambda (x) (equal? (first x) (second x)))
                                    (apply cartesian-product atom-lists))
                        atom-lists))
    (λ () (set))
)))
(add-strategy 'linear (λ (pri rel atom-lists rel-list) (breaker pri
    (break-graph (set) (set))
    (λ ()
        (define atoms (first atom-lists))
        (make-exact-break rel
                        (map list (drop-right atoms 1) (cdr atoms))))
    (λ () (set))
)))
(add-strategy 'acyclic (λ (pri rel atom-lists rel-list) (breaker pri
    (break-graph (set) (set))
    (λ ()
        (define atoms (first atom-lists))
        (make-upper-break rel
                        (for*/list ([i (length atoms)]
                                    [j (length atoms)]
                                    #:when (< i j))
                                (list (list-ref atoms i) (list-ref atoms j)))))
    (λ () (set))
)))
(add-strategy 'tree (λ (pri rel atom-lists rel-list) (breaker pri
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
    (λ () (set))
)))


#| TODO 
- loop
- loops
- unique init/term
- unique init/term + acyclic
- has init/term
- co-everything
|#


(declare 'linear > 'tree)
(declare 'tree > 'acyclic)
(declare 'acyclic > 'irref)



; use to prevent breaks
(add-strategy 'default (λ (pri rel atom-lists rel-list) (breaker pri
    (break-graph (set) (set))
    (λ () 
        (make-upper-break rel (apply cartesian-product atom-lists)))
    (λ () (set))
)))






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
|#




