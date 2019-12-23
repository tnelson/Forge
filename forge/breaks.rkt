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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; methods for defining breaks ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (hash-add! h k v)
    (if (hash-has-key? h k)
        (set-add! (hash-ref h k) v)
        (hash-set! h k (mutable-set v))))

(define breakers (make-hash))
(define compos (make-hash))
(define upsets (make-hash))
(define downsets (make-hash))

(define (add-breaker a f) 
    (hash-set! breakers a f)
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


(define (min-breaks! breaks)
    (define changed false)
    (hash-for-each compos (λ (k v)
        (when (subset? k breaks)
              (set-subtract! breaks k)
              (set-add! breaks v)
              (set! changed true))
    ))
    ;(println breaks)
    (when changed (min-breaks! breaks))
)

(define rel-breaks (make-hash))
(define (break-rel rel . breaks) ; renamed-out to 'break for use in forge
    (for ([break breaks]) 
        (unless (hash-has-key? breakers break) (error "break not implemented:" break))
        (hash-add! rel-breaks rel break)))

(define (constrain-bound bound bounds-store relations-store)
    (define rel (bound-relation bound))
    (define breaks (hash-ref rel-breaks rel (set)))
    (min-breaks! breaks)
    (define c (set-count breaks))

    (case (set-count breaks) 
        [(0) (bound->break bound)]
        [(1) (define breaker (hash-ref breakers (set-first breaks)))
             (define rel-list (hash-ref relations-store rel))
             (define atom-lists (map (λ (b) (hash-ref bounds-store b)) rel-list))
             ;(define the-break (breaker rel atom-lists rel-list))
             ;(println the-break)
             ;(break-bound the-break)
             (breaker rel atom-lists rel-list)
        ]
        [else (error "can't compose these breaks; either unsat or unimplemented:" (set->list breaks))]
    )
)
(define (constrain-bounds total-bounds bounds-store relations-store) 
    (map (lambda (bound) 
        (constrain-bound bound bounds-store relations-store)
    ) total-bounds)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; define breaks and compositions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-breaker 'irref (λ (rel atom-lists rel-list) 
    (make-upper-break rel
                      (filter-not (lambda (x) (equal? (first x) (second x)))
                                  (apply cartesian-product atom-lists)))))
(add-breaker 'ref (λ (rel atom-lists rel-list) 
    (make-lower-break rel
                      (filter     (lambda (x) (equal? (first x) (second x)))
                                  (apply cartesian-product atom-lists))
                      atom-lists)))
(add-breaker 'linear (λ (rel atom-lists rel-list)
    (define atoms (first atom-lists))
    (make-exact-break rel
                      (map list (drop-right atoms 1) (cdr atoms)))))
(add-breaker 'acyclic (λ (rel atom-lists rel-list)
    (define atoms (first atom-lists))
    (make-upper-break rel
                      (for*/list ([i (length atoms)]
                                  [j (length atoms)]
                                  #:when (< i j))
                            (list (list-ref atoms i) (list-ref atoms j))))))
(add-breaker 'tree (λ (rel atom-lists rel-list)
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
        )
        #| ;can't identify specific atoms in kodkod formulas
        (for/set ([atom (cdr atoms)]) 
            (@one (@join rel (@node/expr/constant 1 atom)))
        )|#
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
(add-breaker 'default (λ (rel atom-lists rel-list) 
    (make-upper-break rel (apply cartesian-product atom-lists))))






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




