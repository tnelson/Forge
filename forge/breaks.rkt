#lang racket

(require "lang/bounds.rkt")
(provide constrain-bounds! (rename-out [break-rel break]))

;;;;

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

;;;;

(define (hash-add! h k v)
    (if (hash-has-key? h k)
        (set-add! (hash-ref h k) v)
        (hash-set! h k (mutable-set v))))

;; structures/methods for defining breaks
(define break-fns (make-hash))
(define compos (make-hash))

(define (add-break a f) (hash-set! break-fns a f))
(define (equiv a . bs) 
    (hash-set! compos (apply set bs) a)
    ; if no fn defined for a, default to naively doing all bs
    (unless (hash-has-key? break-fns a)
            (hash-set! break-fns a (λ (atom-lists)
                (apply break+ (for ([b bs]) 
                    ((hash-ref break-fns b) atom-lists)
                ))
                #|(for ([b bs])
                    ((hash-ref break-fns b) bound atom-list atom-lists)
                )|#
            )))
)
(define (stricter a . bs) (for ([b bs]) (equiv a a b)))
(define (weaker a . bs) (for ([b bs]) (equiv b b a)))


(define (min-breaks! breaks)
    (define changed false)
    (hash-for-each compos (λ (k v)
        (when (subset? k breaks)
              (set-subtract! breaks k)
              (set-add! breaks v)
              (set! changed true))
    ))
    (when changed (min-breaks! breaks))
)


(define rel-breaks (make-hash))
(define (break-rel rel . breaks) 
    (for ([break breaks]) (hash-add! rel-breaks rel break)))

;; define allowed breaks and compositions
(add-break 'irref (λ (atom-lists) 
    (make-upper-break (filter-not (lambda (x) (equal? (first x) (second x)))
                                  (apply cartesian-product atom-lists)))))
(add-break 'ref (λ (atom-lists) 
    (make-lower-break (filter     (lambda (x) (equal? (first x) (second x)))
                                  (apply cartesian-product atom-list))
                      atom-lists)))
(add-break 'linear (λ (atom-lists)
    (define atoms (first atom-list))
    (make-exact-bound (map list (drop-right atoms 1) (cdr atoms)))))
(stricter 'linear 'irref)

#|(add-break 'irref (λ (atom-list) 
    (make-upper-bound (filter-not (lambda (x) (equal? (first x) (second x)))
                                  (apply cartesian-product atom-list)))))
(add-break 'ref (λ (atom-list) 
    (make-upper-bound (filter     (lambda (x) (equal? (first x) (second x)))
                                  (apply cartesian-product atom-list)))))
(add-break 'linear (λ (atom-list)
    (define atoms (first atom-list))
    (make-exact-bound (map list (drop-right atoms 1) (cdr atoms)))))
(stricter 'linear 'irref)|#



;; constrain bounds for symmetry-breaking optimizations and custom bounds
#|(define (constrain-bound bound bounds-store relations-store) 
    (println rel-breaks)
    (define rel (bound-relation bound))
    (when (member rel irref-rels) 
        (define ubound (filter-not (lambda (x) (equal? (first x) (second x)))
                                    (bound-upper bound)))
        (set! bound (make-bound rel (bound-lower bound) ubound))
    )
    (when (member rel linear-rels)
        ; TODO: support higher arity relations like Solution->State->State
        ; TODO: choose a hamiltonian path through upper bound containing lower bound      
        (define atoms (hash-ref bounds-store (first (hash-ref relations-store rel))))
        (define order (map list (drop-right atoms 1) (cdr atoms)))  ;; map list = zip
        (set! bound (make-bound rel order order))
    )
    bound
)|#




(define (constrain-bound! bound bounds-store relations-store)
    (define rel (bound-relation bound))
    (define breaks (hash-ref rel-breaks rel (set)))
    (min-breaks! breaks)
    (define c (set-count breaks))


    (case (set-count breaks) 
        [(0) bound]
        [(1) (define break-fn (hash-ref break-fns (set-first breaks)))
             (define atom-lists
                (for ([bound (hash-ref relations-store rel)]) (hash-ref bounds-store bound)))
             ;(print "break-fn: " break-fn)
             ;(print "atom-lists: " atom-lists)
             (break-fn atom-lists)
        ]
        [else (raise "can't compose breaks")] ; TODO: improve err msg
    )

    bound
)



(define (constrain-bounds! total-bounds bounds-store relations-store) 
    ;(set! bounds-store bounds-stor)
    ;(set! relations-store relations-stor)
    ;(map (lambda (bound) (constrain-bound bound)) total-bounds)
    (for ([bound total-bounds]) (constrain-bound! bound bounds-store relations-store))
)






