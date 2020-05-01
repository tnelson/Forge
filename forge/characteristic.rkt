#lang racket

(require "lang/ast.rkt" (prefix-in @ racket))

(provide inst-to-formula)

;; converts an instance I into a characteristic function ϕ s.t.
;; 1. I |= ϕ
;; 2. J |= ϕ <=> I ≅ J
;; this function assumes inst assigns a value to every sig/relation
(define (inst-to-formula inst [relations-store #f])

    (printf "inst : ~v~n" inst)

    (define name-to-rel (hash-map))
    (when relations-store
        (for ([rel relations-store])
            (hash-set! name-to-rel (string->symbol (relation-name rel)) rel)))
    (define (to-rel x) (if (symbol? x) (name-to-rel x) x))

    (printf "name-to-rel : ~v~n" name-to-rel)

    (define (is-rel rel)
        (define name (relation-name rel))
        (and (@not (equal? name "Int"))                         ; not Int
             (@not (equal? (first (string->list name)) #\$))))  ; not Skolem

    ;; rels :: relation |-> list<tuple<sym>>
    (define rels (for/hash ([(rel tups) (in-hash inst)] #:when (is-rel rel)) 
        (values rel tups)))
    ;(printf "rels : ~v~n" rels)

    ;; sigs :: sig |-> list<sym>
    (define sigs (for/hash ([(rel tups) (in-hash rels)] #:when (equal? (relation-arity rel) 1)) 
        (values rel (map car tups))))
    ;(printf "sigs : ~s~n" sigs)

    ;; atoms :: symbol |-> (qvar . sig)
    ;; sorting sigs by size makes sure atoms get assigned most specific sig when there are subsigs
    (define atoms (for*/hash ([sig-syms (sort (hash->list sigs) @> #:key length)]
                              [sym (cdr sig-syms)]) 
        (values sym `[,(node/expr/quantifier-var 1 sym) . ,(car sig-syms)])))
    ;(printf "atoms : ~s~n" atoms)
    (define (sym-to-qv sym) (car (hash-ref atoms sym)))

    (define ineqs (for*/list ([(sig syms) (in-hash sigs)]
                [sym1 syms]
                [sym2 syms] #:when (@not (equal? sym1 sym2)))
        `(! (= ,(sym-to-qv sym1) ,(sym-to-qv sym2)))))

    (define defs (for/list ([(rel tups) (in-hash rels)])
        `(= ,rel (+ ,@(for/list ([tup tups])
            (if (@= 1 (length tup)) 
                (sym-to-qv (car tup))
                `(-> ,@(map sym-to-qv tup)))
        )))
    ))

    (define fm (foldl 
        (lambda (kv acc) `(some (,kv) ,acc))
        `(and ,@ineqs ,@defs)
        (hash-values atoms) 
    ))

    ;(define fm `(some ,(hash-values atoms) 
    ;    (and ,@ineqs ,@defs)
    ;))

    (printf "fm : ~s~n" fm)
)