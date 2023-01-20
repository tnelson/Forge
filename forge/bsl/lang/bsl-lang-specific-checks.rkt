#lang racket

(require forge/lang/ast)
(require forge/sigs-structs)
(require (for-syntax racket/syntax syntax/srcloc)
         syntax/srcloc)

(define (raise-bsl-error message node loc)
  (raise-user-error 'forge/bsl (format "~a in ~a at loc: ~a" message (deparse node) (srcloc->string loc))))

(define (raise-bsl-error-deparsed-str message deparsed loc)
  (raise-user-error 'forge/bsl (format "~a in ~a at loc: ~a" message deparsed (srcloc->string loc))))

(define (raise-bsl-relational-error rel-str node loc [optional-str #f])  
  (raise-bsl-error 
          (format "froglet didn't recognize the ~a operator ~a"
            rel-str
            (if optional-str (format "; ~a" optional-str) "")) 
          node loc))

(define (raise-bsl-relational-error-expr-args rel-str args loc [optional-str #f])  
  (raise-bsl-error-deparsed-str 
          (format "froglet didn't recognize the ~a operator~a"
            rel-str
            (if optional-str (format "; ~a" optional-str) "")) 
          (cond 
          [(equal? (length args) 1)
              (format "(~a~a)" rel-str (deparse (first args)))]
          [(equal? (length args) 2)
              (format "(~a ~a ~a)" (deparse (first args)) rel-str (deparse (second args)))]
          [else (string-join (map deparse args) " ")])
          loc))

(define (srcloc->string loc)
  (format "line ~a, col ~a, span: ~a" (source-location-line loc) (source-location-column loc) (source-location-span loc)))

;; ---

(define (check-node-formula-constant formula-node)
  (void))

(define (check-node-formula-op formula-node)
  (void))

(define (check-node-formula-multiplicity formula-node)
  (void))

(define (check-node-formula-quantified formula-node)
  (void))

(define (check-node-formula-op-always formula-node)
  (void))

(define (check-node-formula-op-eventually formula-node)
  (void))

(define (check-node-formula-op-until formula-node)
  (void))

(define (check-node-formula-op-releases formula-node)
  (void))

(define (check-node-formula-op-next_state formula-node)
  (void))

(define (check-node-formula-op-historically formula-node)
  (void))

(define (check-node-formula-op-once formula-node)
  (void))

(define (check-node-formula-op-prev_state formula-node)
  (void))

(define (check-node-formula-op-since formula-node)
  (void))

(define (check-node-formula-op-triggered formula-node)
  (void))

(define (check-node-formula-op-&& formula-node)
  (void))

(define (check-node-formula-op-|| formula-node)
  (void))

(define (check-node-formula-op-=> formula-node)
  (void))

(define (check-node-formula-op-in formula-node)
  (when (eq? (nodeinfo-lang (node-info formula-node)) 'bsl)
    (define loc (nodeinfo-loc (node-info formula-node)))
    (raise-bsl-relational-error "\"in\"" formula-node loc)))

(define (check-node-formula-op-= formula-node)
  (void))

(define (check-node-formula-op-! formula-node)
  (void))

(define (check-node-formula-op-int> formula-node)
  (void))

(define (check-node-formula-op-int< formula-node)
  (void))

(define (check-node-formula-op-int= formula-node)
  (void))

(define (check-node-expr-relation expr-node)
  (void))

(define (check-node-expr-atom expr-node)
  (void))

(define (check-node-expr-ite expr-node)
  (void))

(define (check-node-expr-constant expr-node)
  (void))

(define (check-node-expr-op expr-node)
  (void))

(define (check-node-expr-quantifier-var expr-node)
  (void))

#;(define (check-node-expr-comprehension expr-node)
    (when (eq? (nodeinfo-lang (node-info expr-node)) 'bsl)
      (define loc (nodeinfo-loc (node-info expr-node)))
      (define locstr (format "line ~a, col ~a, span: ~a" (source-location-line loc) (source-location-column loc) (source-location-span loc)))
      (printf "Set Comprehension at ~a at loc: ~a~n" (deparse expr-node) locstr)))
      ;(raise-bsl-error "Set Comprehension" expr-node loc)

(define (check-node-expr-op-prime expr-node)
  (void))

(define (check-node-expr-op-+ expr-node)
  (when (eq? (nodeinfo-lang (node-info expr-node)) 'bsl)
    (define loc (nodeinfo-loc (node-info expr-node)))
    (raise-bsl-relational-error "+" expr-node loc "You may have meant to use the `add` predicate instead.")))

(define (check-node-expr-op-- expr-node)
  (when (eq? (nodeinfo-lang (node-info expr-node)) 'bsl)
    (define loc (nodeinfo-loc (node-info expr-node)))
    (raise-bsl-relational-error "-" expr-node loc "You may have meant to use the `subtract` predicate instead.")))

(define (check-node-expr-op-& expr-node)
  (when (eq? (nodeinfo-lang (node-info expr-node)) 'bsl)
    (define loc (nodeinfo-loc (node-info expr-node)))
    (raise-bsl-relational-error "&" expr-node loc)))

(define (check-node-expr-op--> expr-node)
  (when (eq? (nodeinfo-lang (node-info expr-node)) 'bsl)
    (define loc (nodeinfo-loc (node-info expr-node)))
    (raise-bsl-error "Direct use of -> is not allowed in froglet" expr-node loc)))

(define (check-node-expr-op-join expr-node)
  (void))
  ; ;(printf "checking join: ~a~n" expr-node)
  ; (define left-hand-side (first (node/expr/op-children expr-node)))
  ; (define loc (nodeinfo-loc (node-info left-hand-side)))
  ; (define locstr (format "line ~a, col ~a, span: ~a" (source-location-line loc) (source-location-column loc) (source-location-span loc)))
  ; (unless (or (node/expr/quantifier-var? left-hand-side)
  ;         (and (node/expr/relation? left-hand-side) (equal? 1 (node/expr-arity left-hand-side)) (Sig-one left-hand-side)))
  ;   (raise-bsl-error "Left hand side to field access must be a sig" expr-node loc))

(define (check-node-expr-op-^ expr-node)
  (when (eq? (nodeinfo-lang (node-info expr-node)) 'bsl)
    (define loc (nodeinfo-loc (node-info expr-node)))
    (raise-bsl-relational-error "^" expr-node loc)))

(define (check-node-expr-op-* expr-node)
  (when (eq? (nodeinfo-lang (node-info expr-node)) 'bsl)
    (define loc (nodeinfo-loc (node-info expr-node)))
    (raise-bsl-relational-error "*" expr-node loc)))

(define (check-node-expr-op-~ expr-node)
  (when (eq? (nodeinfo-lang (node-info expr-node)) 'bsl)
    (define loc (nodeinfo-loc (node-info expr-node)))
    (raise-bsl-relational-error "~~" expr-node loc)))

(define (check-node-expr-op-sing expr-node)
  (void))

(define (err-empty-join expr-node)
  (define loc (nodeinfo-loc (node-info expr-node)))
  (raise-bsl-error "Sig does not have such a field" expr-node loc))

(define (err-relation-join expr-node args)
  (define loc (nodeinfo-loc (node-info expr-node)))
  (raise-bsl-error (format "\"~a\" was not an object, so could not access its fields." (deparse (first args))) expr-node loc))

(define bsl-checker-hash (make-hash))
(hash-set! bsl-checker-hash 'empty-join err-empty-join)
(hash-set! bsl-checker-hash 'relation-join err-relation-join)
(hash-set! bsl-checker-hash node/formula/constant check-node-formula-constant)
(hash-set! bsl-checker-hash node/formula/op check-node-formula-op)
(hash-set! bsl-checker-hash node/formula/multiplicity check-node-formula-multiplicity)
(hash-set! bsl-checker-hash node/formula/quantified check-node-formula-quantified)
(hash-set! bsl-checker-hash node/formula/op/always check-node-formula-op-always)
(hash-set! bsl-checker-hash node/formula/op/eventually check-node-formula-op-eventually)
(hash-set! bsl-checker-hash node/formula/op/until check-node-formula-op-until)
(hash-set! bsl-checker-hash node/formula/op/releases check-node-formula-op-releases)
(hash-set! bsl-checker-hash node/formula/op/next_state check-node-formula-op-next_state)
(hash-set! bsl-checker-hash node/formula/op/historically check-node-formula-op-historically)
(hash-set! bsl-checker-hash node/formula/op/once check-node-formula-op-once)
(hash-set! bsl-checker-hash node/formula/op/prev_state check-node-formula-op-prev_state)
(hash-set! bsl-checker-hash node/formula/op/since check-node-formula-op-since)
(hash-set! bsl-checker-hash node/formula/op/triggered check-node-formula-op-triggered)
(hash-set! bsl-checker-hash node/formula/op/&& check-node-formula-op-&&)
(hash-set! bsl-checker-hash node/formula/op/|| check-node-formula-op-||)
(hash-set! bsl-checker-hash node/formula/op/=> check-node-formula-op-=>)
(hash-set! bsl-checker-hash node/formula/op/in check-node-formula-op-in)
(hash-set! bsl-checker-hash node/formula/op/= check-node-formula-op-=)
(hash-set! bsl-checker-hash node/formula/op/! check-node-formula-op-!)
(hash-set! bsl-checker-hash node/formula/op/int> check-node-formula-op-int>)
(hash-set! bsl-checker-hash node/formula/op/int< check-node-formula-op-int<)
(hash-set! bsl-checker-hash node/formula/op/int= check-node-formula-op-int=)
(hash-set! bsl-checker-hash node/expr/relation check-node-expr-relation)
(hash-set! bsl-checker-hash node/expr/atom check-node-expr-atom)
(hash-set! bsl-checker-hash node/expr/ite check-node-expr-ite)
(hash-set! bsl-checker-hash node/expr/constant check-node-expr-constant)
(hash-set! bsl-checker-hash node/expr/op check-node-expr-op)
(hash-set! bsl-checker-hash node/expr/quantifier-var check-node-expr-quantifier-var)
;(hash-set! bsl-checker-hash node/expr/comprehension check-node-expr-comprehension)
(hash-set! bsl-checker-hash node/expr/op/prime check-node-expr-op-prime)
(hash-set! bsl-checker-hash node/expr/op/+ check-node-expr-op-+)
(hash-set! bsl-checker-hash node/expr/op/- check-node-expr-op--)
(hash-set! bsl-checker-hash node/expr/op/& check-node-expr-op-&)
(hash-set! bsl-checker-hash node/expr/op/-> check-node-expr-op-->)
(hash-set! bsl-checker-hash node/expr/op/join check-node-expr-op-join)
(hash-set! bsl-checker-hash node/expr/op/^ check-node-expr-op-^)
(hash-set! bsl-checker-hash node/expr/op/* check-node-expr-op-*)
(hash-set! bsl-checker-hash node/expr/op/~ check-node-expr-op-~)
(hash-set! bsl-checker-hash node/expr/op/sing check-node-expr-op-sing)


(define (check-expr-mult expr-node sing parent-expr)
  (when (and (not sing) (eq? (nodeinfo-lang (node-info parent-expr)) 'bsl))
    (define loc (nodeinfo-loc (node-info expr-node)))
    (raise-user-error (format "froglet: ~a not a singleton in ~a at loc: ~a"  (deparse expr-node) (deparse parent-expr) (srcloc->string loc)))))

(hash-set! bsl-checker-hash 'expr-mult check-expr-mult)
(provide bsl-checker-hash)


(define (bsl-ast-arg-checks args)
  (void))

; motivation: if student use join on relations and get arity error, that could be confusing
#;(define (check-node-expr-op-join-args expr-args)
  ;(printf "checking join: ~a~n" expr-args)
  (define left-hand-side (first expr-args))
  (define loc (nodeinfo-loc (node-info left-hand-side)))
  (define locstr (format "line ~a, col ~a, span: ~a" (source-location-line loc) (source-location-column loc) (source-location-span loc)))
  (unless (or (node/expr/quantifier-var? left-hand-side)
          (and (node/expr/relation? left-hand-side) (equal? 1 (node/expr-arity left-hand-side)) (Sig-one left-hand-side)))
    (raise-user-error (format "Left hand side to field access at ~a must be an object at loc: ~a" expr-args locstr))))

  ;NOTE: make better error message 

(define (check-args-node-expr-op--> expr-args info)
    (when (eq? (nodeinfo-lang info) 'bsl)
      (define loc (nodeinfo-loc info))
      (raise-bsl-error-deparsed-str "Direct use of -> is not allowed in froglet" (format "(~a->~a)" (deparse (first expr-args)) (deparse (second expr-args))) loc)))

(define (check-args-node-expr-op-+ expr-args info)
  (when (eq? (nodeinfo-lang info) 'bsl)
      (define loc (nodeinfo-loc info))
      (raise-bsl-relational-error-expr-args "+" expr-args loc "You may have meant to use the `add` predicate instead.")))


(define (check-args-node-expr-op-- expr-args info)
  (when (eq? (nodeinfo-lang info) 'bsl)
      (define loc (nodeinfo-loc info))
      (raise-bsl-relational-error-expr-args "-" expr-args loc "You may have meant to use the `subtract` predicate instead.")))

(define (check-args-node-expr-op-& expr-args info)
  (when (eq? (nodeinfo-lang info) 'bsl)
      (define loc (nodeinfo-loc info))
      (raise-bsl-relational-error-expr-args "&" expr-args loc)))

(define (check-args-node-expr-op-^ expr-args info)
  (when (eq? (nodeinfo-lang info) 'bsl)
      (define loc (nodeinfo-loc info))
      (raise-bsl-relational-error-expr-args "^" expr-args loc)))

(define (check-args-node-expr-op-* expr-args info)
  (when (eq? (nodeinfo-lang info) 'bsl)
      (define loc (nodeinfo-loc info))
      (raise-bsl-relational-error-expr-args "*" expr-args loc)))

(define (check-args-node-expr-op-~ expr-args info)
  (when (eq? (nodeinfo-lang info) 'bsl)
      (define loc (nodeinfo-loc info))
      (raise-bsl-relational-error-expr-args "~~" expr-args loc)))

; TODO: add a global field-decl check outside bsl
(define (bsl-field-decl-func true-breaker)
  (unless (or (equal? 'func (node/breaking/break-break true-breaker)) (equal? 'pfunc (node/breaking/break-break true-breaker))) 
  (raise-user-error (format "froglet: Field declaration must be one, lone, func, or pfunc"))))



(define bsl-ast-checker-hash (make-hash))
(hash-set! bsl-ast-checker-hash "check-args" bsl-ast-arg-checks)
(hash-set! bsl-ast-checker-hash 'field-decl bsl-field-decl-func)
(hash-set! bsl-ast-checker-hash node/expr/op/-> check-args-node-expr-op-->)
(hash-set! bsl-ast-checker-hash node/expr/op/+ check-args-node-expr-op-+)
(hash-set! bsl-ast-checker-hash node/expr/op/- check-args-node-expr-op--)
(hash-set! bsl-ast-checker-hash node/expr/op/& check-args-node-expr-op-&)
(hash-set! bsl-ast-checker-hash node/expr/op/^ check-args-node-expr-op-^)
(hash-set! bsl-ast-checker-hash node/expr/op/* check-args-node-expr-op-*)
(hash-set! bsl-ast-checker-hash node/expr/op/~ check-args-node-expr-op-~)
;(hash-set! bsl-ast-checker-hash node/expr/op/join check-node-expr-op-join-args)


; BE CAREFUL WHEN ADDING AST-CHECKS!!
; could break inst checking; we don't want all bsl rules to 
; apply to inst. For example, even in BSL we could say 
; next = `Node1->`Node2
(provide bsl-ast-checker-hash)


(define bsl-inst-checker-hash (make-hash))

(define (inst-check-node-formula-op-in formula-node)
  (when (eq? (nodeinfo-lang (node-info formula-node)) 'bsl)
    (define loc (nodeinfo-loc (node-info formula-node)))
    (raise-bsl-relational-error "\"in\"" formula-node loc)))

(hash-set! bsl-inst-checker-hash node/formula/op/in inst-check-node-formula-op-in)
(provide bsl-inst-checker-hash)
