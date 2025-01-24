#lang racket/base

(require forge/lang/ast)
(require forge/sigs-structs)
(require (for-syntax racket/syntax syntax/srcloc)
         syntax/srcloc)
(require (only-in racket first second string-join))

(provide bsl-checker-hash)
(provide bsl-ast-checker-hash)

(define LANG_ID 'bsl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper functions for errors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (raise-bsl-error message node [ignore-loc #f])
  (raise-forge-error #:msg (format "~a in ~a" message (deparse node))
                     #:context node))

(define (raise-bsl-error-deparsed-str message deparsed loc)
  (raise-forge-error #:msg (format "~a in ~a" message deparsed)
                     #:context loc))

(define (raise-bsl-relational-error rel-str node loc [optional-str #f])  
  (raise-bsl-error 
          (format "Froglet: invalid use of the ~a operator ~a"
            rel-str
            (if optional-str (format "; ~a" optional-str) "")) 
          node loc))

(define (raise-bsl-relational-error-expr-args rel-str args loc [optional-str #f])  
  (raise-bsl-error-deparsed-str 
          (format "Froglet: invalid use of the ~a operator~a"
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

; TODO: Special case: take only the node and child-types
; Should be unified with other registered functions
(define (err-empty-join expr-node child-types #:run-or-state [run-or-state #f])
  (define loc (nodeinfo-loc (node-info expr-node)))
  ; Deprimification lets us print, e.g., "Animal" rather than "(Dog Cat Human ...)"
  (define lhs-type (if run-or-state 
                       (deprimify run-or-state (expression-type-type (first child-types)))
                       (expression-type-type (first child-types))))
  (if (and (equal? 1 (length lhs-type))
           (symbol? (first lhs-type)))
      (raise-bsl-error (format "Sig ~a does not have such a field" (first lhs-type)) expr-node loc)
      (raise-bsl-error (format "No such field in possible types of left-hand-side (~a)" lhs-type) expr-node loc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Checker functions and checker-hash definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 1: checker-hash. These are invoked in the last-checker recursive descent, before
; the problem is passed to the solver. At this point, all AST nodes have been generated.
;
; The key that identifies a given checker function will either be:
;     - an AST node type (i.e., the constructor function defined in ast.rkt), denoting
;       that a node of this type is being visited
;     - a symbol, denoting a specific error type that has been recognized, giving the
;       checker an opportunity to override that error with something language-specific.
;
; For the FIRST CASE:
; The convention is that, when defining a checker function, it accepts three arguments:
;    - the AST node to be checked
;    - the inferred type of this node, if applicable (expressions only; #f otherwise)
;    - the inferred type of its children, if applicable (child expressions only; #f otherwise)
;
; For the SECOND CASE: only the node itself is passed.
;
; Each checker may use, or not use, each argument as needed. This is merely a standard 
; polymorphic interface that the checker module uses to pass data.
;
; Checker functions' return results will be ignored. They should invoke raise-forge-error
; to trigger (or queue, depending on settings) an error message.
;
; All checker functions are responsible for confirming the language tag on each AST node,
; because some may come from "blessed" contexts such as the reachable built-in.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The "in" operator is not allowed in Froglet, except when the LHS is a singleton and the 
; RHS is a sig. I.e., it is allowed only when it is used as a type predicate.

(define (check-node-formula-op-in formula-node node-type child-types)  
  (when (eq? (nodeinfo-lang (node-info formula-node)) LANG_ID)
    (define lhs-multiplicity (expression-type-multiplicity (first child-types)))
    (unless (and (equal? lhs-multiplicity 'one)
                 (Sig? (second (node/formula/op-children formula-node))))
      (define loc (nodeinfo-loc (node-info formula-node)))
      (raise-bsl-relational-error "\"in\"" formula-node loc))))

(define (check-top-expression formula-parent expr-node t #:allow-sigs [allow-sigs #f])
  (when (and (member t '(func pfunc))
             (eq? (nodeinfo-lang (node-info formula-parent)) LANG_ID))
    (raise-forge-error #:msg (format "Chain of field applications did not result in a singleton value: ~a" (deparse expr-node))
                       #:context expr-node))
  (when (and (member t '(set))
             (eq? (nodeinfo-lang (node-info formula-parent)) LANG_ID)
             (or (not allow-sigs) (not (Sig? expr-node))))
    (raise-forge-error #:msg (format "Expression was not a singleton value: ~a" (deparse expr-node))
                       #:context expr-node)))

(define (check-node-formula-op-= formula-node node-type child-types)
  (when (eq? (nodeinfo-lang (node-info formula-node)) LANG_ID)
    (define t1 (expression-type-multiplicity (first child-types)))
    (define t2 (expression-type-multiplicity (second child-types)))
    (define node1 (first (node/formula/op-children formula-node)))
    (define node2 (second (node/formula/op-children formula-node)))
    (check-top-expression formula-node node1 t1)
    (check-top-expression formula-node node2 t2)))

(define (check-formula-mult formula-node node-type child-types)
  (when (eq? (nodeinfo-lang (node-info formula-node)) LANG_ID)
    (define t (expression-type-multiplicity (first child-types)))
    (define expr-node (node/formula/multiplicity-expr formula-node))
    (check-top-expression formula-node expr-node t #:allow-sigs #t)))


#;(define (check-node-expr-comprehension expr-node node-type child-types)
    (when (eq? (nodeinfo-lang (node-info expr-node)) LANG_ID)
      (define loc (nodeinfo-loc (node-info expr-node)))
      (define locstr (format "line ~a, col ~a, span: ~a" (source-location-line loc) (source-location-column loc) (source-location-span loc)))
      (printf "Set Comprehension at ~a at loc: ~a~n" (deparse expr-node) locstr)))
      ;(raise-bsl-error "Set Comprehension" expr-node loc)

(define (check-node-expr-op-+ expr-node node-type child-types)
  (when (eq? (nodeinfo-lang (node-info expr-node)) LANG_ID)
    (define loc (nodeinfo-loc (node-info expr-node)))
    (raise-bsl-relational-error "+" expr-node loc "You may have meant to use the `add` predicate instead.")))

(define (check-node-expr-op-- expr-node node-type child-types)
  (when (eq? (nodeinfo-lang (node-info expr-node)) LANG_ID)
    (define loc (nodeinfo-loc (node-info expr-node)))
    (raise-bsl-relational-error "-" expr-node loc "You may have meant to use the `subtract` predicate instead.")))

(define (check-node-expr-op-& expr-node node-type child-types)
  (when (eq? (nodeinfo-lang (node-info expr-node)) LANG_ID)
    (define loc (nodeinfo-loc (node-info expr-node)))
    (raise-bsl-relational-error "&" expr-node loc)))

(define (check-node-expr-op--> expr-node node-type child-types)  
  (when (eq? (nodeinfo-lang (node-info expr-node)) LANG_ID)
    (define loc (nodeinfo-loc (node-info expr-node)))
    (raise-bsl-error "Use of -> in expressions is not allowed in forge/bsl" expr-node loc)))

(define (check-node-expr-op-join expr-node node-type child-types)
  (when (eq? (nodeinfo-lang (node-info expr-node)) LANG_ID)
    (define lhs-type (first child-types))
    (define rhs-type (second child-types))
    (define args (node/expr/op-children expr-node))
    (unless (member (expression-type-multiplicity lhs-type) '(one lone))
      (raise-bsl-error (format "\"~a\" was not an object, so could not access its fields." (deparse (first args))) expr-node))
    (unless (member (expression-type-multiplicity rhs-type) '(pfunc func))
      (raise-bsl-error (format "\"~a\" was not usable as a field." (deparse (second args))) expr-node))))

(define (check-node-expr-op-^ expr-node node-type child-types)
  (when (eq? (nodeinfo-lang (node-info expr-node)) LANG_ID)
    (define loc (nodeinfo-loc (node-info expr-node)))
    (raise-bsl-relational-error "^" expr-node loc)))

(define (check-node-expr-op-* expr-node node-type child-types)
  (when (eq? (nodeinfo-lang (node-info expr-node)) LANG_ID)
    (define loc (nodeinfo-loc (node-info expr-node)))
    (raise-bsl-relational-error "*" expr-node loc)))

(define (check-node-expr-op-~ expr-node node-type child-types)
  (when (eq? (nodeinfo-lang (node-info expr-node)) LANG_ID)
    (define loc (nodeinfo-loc (node-info expr-node)))
    (raise-bsl-relational-error "~~" expr-node loc)))

(define bsl-checker-hash (make-hash))
(hash-set! bsl-checker-hash node/formula/multiplicity check-formula-mult)
;(hash-set! bsl-checker-hash node/formula/multiplicity check-node-formula-multiplicity)
(hash-set! bsl-checker-hash 'empty-join err-empty-join)
;(hash-set! bsl-checker-hash 'relation-join err-relation-join)
(hash-set! bsl-checker-hash node/formula/op/in check-node-formula-op-in)
(hash-set! bsl-checker-hash node/formula/op/= check-node-formula-op-=)
(hash-set! bsl-checker-hash node/expr/op/+ check-node-expr-op-+)
(hash-set! bsl-checker-hash node/expr/op/- check-node-expr-op--)
(hash-set! bsl-checker-hash node/expr/op/& check-node-expr-op-&)
(hash-set! bsl-checker-hash node/expr/op/-> check-node-expr-op-->)
(hash-set! bsl-checker-hash node/expr/op/join check-node-expr-op-join)
(hash-set! bsl-checker-hash node/expr/op/^ check-node-expr-op-^)
(hash-set! bsl-checker-hash node/expr/op/* check-node-expr-op-*)
(hash-set! bsl-checker-hash node/expr/op/~ check-node-expr-op-~)

;(hash-set! bsl-checker-hash node/fmla/pred-spacer check-node-fmla-pred-spacer)
;(hash-set! bsl-checker-hash node/expr/fun-spacer check-node-expr-fun-spacer)
;(hash-set! bsl-checker-hash node/formula/constant check-node-formula-constant)
;(hash-set! bsl-checker-hash node/formula/op check-node-formula-op)
;(hash-set! bsl-checker-hash node/formula/quantified check-node-formula-quantified)
;(hash-set! bsl-checker-hash node/formula/op/always check-node-formula-op-always)
;(hash-set! bsl-checker-hash node/formula/op/eventually check-node-formula-op-eventually)
;(hash-set! bsl-checker-hash node/formula/op/until check-node-formula-op-until)
;(hash-set! bsl-checker-hash node/formula/op/releases check-node-formula-op-releases)
;(hash-set! bsl-checker-hash node/formula/op/next_state check-node-formula-op-next_state)
;(hash-set! bsl-checker-hash node/formula/op/historically check-node-formula-op-historically)
;(hash-set! bsl-checker-hash node/formula/op/once check-node-formula-op-once)
;(hash-set! bsl-checker-hash node/formula/op/prev_state check-node-formula-op-prev_state)
;(hash-set! bsl-checker-hash node/formula/op/since check-node-formula-op-since)
;(hash-set! bsl-checker-hash node/formula/op/triggered check-node-formula-op-triggered)
;(hash-set! bsl-checker-hash node/formula/op/&& check-node-formula-op-&&)
;(hash-set! bsl-checker-hash node/formula/op/|| check-node-formula-op-||)
;(hash-set! bsl-checker-hash node/formula/op/=> check-node-formula-op-=>)
;(hash-set! bsl-checker-hash node/formula/op/! check-node-formula-op-!)
;(hash-set! bsl-checker-hash node/formula/op/int> check-node-formula-op-int>)
;(hash-set! bsl-checker-hash node/formula/op/int< check-node-formula-op-int<)
;(hash-set! bsl-checker-hash node/formula/op/int= check-node-formula-op-int=)
;(hash-set! bsl-checker-hash node/expr/relation check-node-expr-relation)
;(hash-set! bsl-checker-hash node/expr/atom check-node-expr-atom)
;(hash-set! bsl-checker-hash node/expr/ite check-node-expr-ite)
;(hash-set! bsl-checker-hash node/expr/constant check-node-expr-constant)
;(hash-set! bsl-checker-hash node/expr/op check-node-expr-op)
;(hash-set! bsl-checker-hash node/expr/quantifier-var check-node-expr-quantifier-var)
;(hash-set! bsl-checker-hash node/expr/comprehension check-node-expr-comprehension)
;(hash-set! bsl-checker-hash node/expr/op/prime check-node-expr-op-prime)
;(hash-set! bsl-checker-hash node/expr/op/sing check-node-expr-op-sing)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 2: ast-checker-hash. These are invoked when an AST node is being created, in order
; to allow the sub-language to provide its own errors at that stage. This is important
; because some attempts to create AST nodes will result in an error of last resort. For
; example, making the union of two expressions whose arities differ.
;
; At this point, however, the types of each given expression have not yet been inferred.
; Thus, each checker accepts two arguments:
;   - the children of this (prospective) AST node; and
;   - the node-info that this AST node will receive, if it is created successfully (which
;     is useful for assigning syntax locations in errors given). 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (when (eq? (nodeinfo-lang info) LANG_ID)
    (define loc (nodeinfo-loc info))
    (raise-bsl-error-deparsed-str "Direct use of -> in a formula is not allowed in Froglet" (format "(~a->~a)" (deparse (first expr-args)) (deparse (second expr-args))) loc)))

(define (check-args-node-expr-op-+ expr-args info)
  (when (eq? (nodeinfo-lang info) LANG_ID)
    (define loc (nodeinfo-loc info))
    (raise-bsl-relational-error-expr-args "+" expr-args loc "You may have meant to use the `add` predicate instead.")))


(define (check-args-node-expr-op-- expr-args info)
  (when (eq? (nodeinfo-lang info) LANG_ID)
    (define loc (nodeinfo-loc info))
    (raise-bsl-relational-error-expr-args "-" expr-args loc "You may have meant to use the `subtract` predicate instead.")))

(define (check-args-node-expr-op-& expr-args info)
  (when (eq? (nodeinfo-lang info) LANG_ID)
    (define loc (nodeinfo-loc info))
    (raise-bsl-relational-error-expr-args "&" expr-args loc)))

(define (check-args-node-expr-op-^ expr-args info)
  (when (eq? (nodeinfo-lang info) LANG_ID)
    (define loc (nodeinfo-loc info))
    (raise-bsl-relational-error-expr-args "^" expr-args loc)))

(define (check-args-node-expr-op-* expr-args info)
  (when (eq? (nodeinfo-lang info) LANG_ID)
    (define loc (nodeinfo-loc info))
    (raise-bsl-relational-error-expr-args "*" expr-args loc)))

(define (check-args-node-expr-op-~ expr-args info)
  (when (eq? (nodeinfo-lang info) LANG_ID)
    (define loc (nodeinfo-loc info))
    (raise-bsl-relational-error-expr-args "~" expr-args loc)))

; TODO: add a global field-decl check outside bsl
(define (bsl-field-decl-func true-breaker)
  (unless (or (equal? 'func (node/breaking/break-break true-breaker)) (equal? 'pfunc (node/breaking/break-break true-breaker))) 
    (raise-forge-error #:msg (format "Froglet: Field declaration must be one, lone, func, or pfunc")
                       #:context true-breaker)))



(define bsl-ast-checker-hash (make-hash))
(hash-set! bsl-ast-checker-hash 'field-decl bsl-field-decl-func)
(hash-set! bsl-ast-checker-hash node/expr/op/-> check-args-node-expr-op-->)
(hash-set! bsl-ast-checker-hash node/expr/op/+ check-args-node-expr-op-+)
(hash-set! bsl-ast-checker-hash node/expr/op/- check-args-node-expr-op--)
(hash-set! bsl-ast-checker-hash node/expr/op/& check-args-node-expr-op-&)
(hash-set! bsl-ast-checker-hash node/expr/op/^ check-args-node-expr-op-^)
(hash-set! bsl-ast-checker-hash node/expr/op/* check-args-node-expr-op-*)
(hash-set! bsl-ast-checker-hash node/expr/op/~ check-args-node-expr-op-~)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Partial instances and example instance blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; BE CAREFUL WHEN ADDING AST-CHECKS!!
; could break inst checking; we don't want all bsl rules to 
; apply to inst. For example, even in BSL we could say 
; next in `Node1->`Node2

(define bsl-inst-checker-hash (make-hash))

;(define (inst-check-node-formula-op-in formula-node)  
;  (when (eq? (nodeinfo-lang (node-info formula-node)) LANG_ID)
;    (define loc (nodeinfo-loc (node-info formula-node)))
;    (raise-bsl-relational-error "\"in\"" formula-node loc)))

;(hash-set! bsl-inst-checker-hash node/formula/op/in inst-check-node-formula-op-in)
(provide bsl-inst-checker-hash)
