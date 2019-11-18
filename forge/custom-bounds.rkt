#lang racket

(require "lang/bounds.rkt")
(provide linear irref constrain-bounds break)



(define (hash-add! h k v)
    (if (hash-has-key? h k)
        (set-add! (hash-ref h k) v)
        (hash-set! h k (mutable-set v))))


;(provide linear irref)
;List of relations that are linear orders
(define linear-rels '())
(define (linear rel) (set! linear-rels (cons rel linear-rels)))
;List of relations that are irreflexive
(define irref-rels '())
(define (irref rel) (set! irref-rels (cons rel irref-rels)))



(define breaks (make-hash))
(define upset (make-hash))
(define downset (make-hash))
(define equivs (make-hash))

(define (add-break a f) (hash-set! breaks a f))
(define (stricter a b) (hash-add! upset b a) (hash-add! downset a b))
(define (equiv a b c) (hash-set! equivs (list a b) c))

(define rel-breaks (make-hash))
(define (break rel . breaks) 
    (for ([break breaks]) (hash-add! rel-breaks rel break)))


(add-break 'irref (λ (atom-list) 
    (make-upper-bound (filter-not (lambda (x) (equal? (first x) (second x)))
                                  (apply cartesian-product atom-list)))))
(add-break 'linear (λ (atom-list)
    (define atoms (first atom-list))
    (make-exact-bound (map list (drop-right atoms 1) (cdr atoms)))))
(stricter 'linear 'irref)



;; constrain bounds for symmetry-breaking optimizations and custom bounds
(define (constrain-bound bound bounds-store relations-store) 
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
)



(define (constrain-bounds total-bounds bounds-store relations-store) 
    (map (lambda (bound) (constrain-bound bound bounds-store relations-store)) total-bounds)
    )






