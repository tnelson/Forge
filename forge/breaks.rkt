#lang racket

(require "lang/bounds.rkt" (prefix-in @ "lang/ast.rkt"))
(provide constrain-bounds (rename-out [break-rel break]) break-bound break-formulas)

;;;;;;;;;;;;;;;;
;;;; breaks ;;;;
;;;;;;;;;;;;;;;;

(struct sbound (relation lower upper) #:transparent)
(define (make-sbound relation lower [upper false]) (sbound relation lower upper))
(struct break (sbound formulas) #:transparent)
(define (make-break sbound [formulas (set)]) (break sbound formulas))

; get-broken-sigs   : rel |-> set<sig>
; make-break        : (rel atom-lists rel-list) |-> break
; make-formulas     : (rel atom-lists rel-list) |-> set<formula>
(struct breaker (get-broken-sigs make-break make-formulas) #:transparent)

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
(define breakers (make-hash))
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

; h : type(k) |-> set<type(v)>
(define (hash-add! h k v)
    (if (hash-has-key? h k)
        (set-add! (hash-ref h k) v)
        (hash-set! h k (mutable-set v))))

; h : type(k1) |-> type(k2) |-> type(v)
(define (hash-add-set! h k1 k2 v)
    (unless (hash-has-key? h k1) (hash-set! h k1 (make-hash)))
    (define h_k1 (hash-ref h k1))
    (unless (hash-has-key? h_k1 k2) (hash-set! h_k1 k2 pri_c)))

(define (add-breaker a get-broken-sigs make-break make-formulas)
    (hash-set! breakers a (breaker get-broken-sigs make-break make-formulas))
    (hash-add! upsets a a)      ;; a > a
    (hash-add! downsets a a))   ;; a < a
(define (equiv a . bs) 
    (hash-set! compos (apply set bs) a)
    ; if no fn defined for a, default to naively doing all bs
    (unless (hash-has-key? breakers a)
            (hash-set! breakers a (λ (rel atom-lists rel-list)
                (apply break+ (for ([b bs]) 
                    ((hash-ref breakers b) atom-lists)
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
              (define min-pri (apply min 
                (map (lambda (s) (hash-ref break-pris s)) k)))
              (hash-set! break-pris v min-pri)
              (set! changed true))
    ))
    (when changed (min-breaks! breaks))
)

(define (break-rel rel . breaks) ; renamed-out to 'break for use in forge
    (for ([break breaks]) 
        (unless (hash-has-key? breakers break) (error "break not implemented:" break))
        (hash-add! rel-breaks rel break)
        (set! pri_c (add1 pri_c))
        (hash-add-set! rel-break-pri rel break pri_c)))

; map from atom to set of consts it belongs to
;(define atom2consts (make-hash))
; inverse of the above
;(define consts2atoms (make-hash))

(define (constrain-bound bound sigs bounds-store relations-store extensions-store)
    (define rel (bound-relation bound))
    (define breaks (hash-ref rel-breaks rel (set)))
    (define break-pris (hash-ref rel-break-pri rel (make-hash)))
    (min-breaks! breaks break-pris)
    (define c (set-count breaks))

    (case (set-count breaks) 
        [(0) (bound->break bound)]
        [(1) (define breaker (hash-ref breakers (set-first breaks)))
             (define rel-list (hash-ref relations-store rel))
             (define atom-lists (map (λ (b) (hash-ref bounds-store b)) rel-list))
             ((breaker-make-break breaker) rel atom-lists rel-list)
        ]
        [else (error "can't compose these breaks; either unsat or unimplemented:" (set->list breaks))]
    )
)
(define (constrain-bounds total-bounds sigs bounds-store relations-store extensions-store) 
    (define new-total-bounds (list))
    (define formulas (list))

    (for ([bound total-bounds])
        (define break (constrain-bound bound sigs bounds-store relations-store extensions-store))
        (set! new-total-bounds (cons (break-bound break) new-total-bounds))
    )
    (values new-total-bounds formulas)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; define breaks and compositions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-breaker 'irref 
    (λ (rel) (set))
    (λ (rel atom-lists rel-list) 
        (make-upper-break rel
                        (filter-not (lambda (x) (equal? (first x) (second x)))
                                    (apply cartesian-product atom-lists))))
    (λ (rel atom-lists rel-list) (set)) )
(add-breaker 'ref 
    (λ (rel) (set))
    (λ (rel atom-lists rel-list) 
        (make-lower-break rel
                        (filter     (lambda (x) (equal? (first x) (second x)))
                                    (apply cartesian-product atom-lists))
                        atom-lists))
    (λ (rel atom-lists rel-list) (set)) )
(add-breaker 'linear 
    (λ (rel) (set))
    (λ (rel atom-lists rel-list)
        (define atoms (first atom-lists))
        (make-exact-break rel
                        (map list (drop-right atoms 1) (cdr atoms))))
    (λ (rel atom-lists rel-list) (set)) )
(add-breaker 'acyclic 
    (λ (rel) (set))
    (λ (rel atom-lists rel-list)
        (define atoms (first atom-lists))
        (make-upper-break rel
                        (for*/list ([i (length atoms)]
                                    [j (length atoms)]
                                    #:when (< i j))
                                (list (list-ref atoms i) (list-ref atoms j)))))
    (λ (rel atom-lists rel-list) (set)) )
(add-breaker 'tree 
    (λ (rel) (set))
    (λ (rel atom-lists rel-list)
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
    (λ (rel atom-lists rel-list) (set)) )


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
(add-breaker 'default 
    (λ (rel) (set))
    (λ (rel atom-lists rel-list) 
        (make-upper-break rel (apply cartesian-product atom-lists)))
    (λ (rel atom-lists rel-list) (set)) )






#|
ADDING BREAKS
- add breaks here with using add-breaker and the declare forms:
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




