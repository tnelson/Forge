#lang typed/racket/base/optional

; Check whether a higher-order quantification can be Skolemized (by Pardinus). That is:
;   - the quantifier must be effectively existential (`some`, or `all` under negation);
;   - every enclosing context must not be a positive `or`/`=>`, a negated `and`, 
;      a temporal operator, or be within a relational expression; 
;   - there are at most `skolem_depth` enclosing non-skolemized quantified variables.

(require forge/types/ast-adapter
         forge/utils/collector
         (only-in typed/racket findf))
(provide check-higher-order-quantifiers)

(: set-var? (-> node/expr/quantifier-var Boolean))
(define (set-var? v)
  (equal? 'set (node/expr/quantifier-var-mult v)))

; Returns the first `set` quantifier variable within n
(: find-set-var (-> node (U node/expr/quantifier-var False)))
(define (find-set-var n)
  (define found
    (collect n
             (lambda ([sub : node] [ctxt : Any])
               (and (node/formula/quantified? sub)
                    (findf set-var? (map (lambda ([d : Decl]) (car d)) (node/formula/quantified-decls sub)))))
             #:order 'pre-order))
  (if (null? found)
      #f
      (let ([v (car found)])
        (if (node/expr/quantifier-var? v) v #f))))

(: raise-ho-error (-> node/expr/quantifier-var String Nothing))
(define (raise-ho-error v reason)
  (raise-forge-error
   #:msg (format "Unsupported use of `set` in the declaration of ~a: ~a." (node/expr/quantifier-var-name v) reason)
   #:context v))

(define REASON-EXPR "a `set` quantifier cannot appear inside an expression (e.g., a quantifier domain, set comprehension, or if-then-else expression)")
(define REASON-NOT-EXISTENTIAL "a `set` quantifier must be effectively existential (`some`, or negated `all`)")

; Check every set-quantified formula. 
(: check-higher-order-quantifiers (-> (Listof node/formula) Integer Symbol Void))
(define (check-higher-order-quantifiers fmlas skolem-depth backend)
  (unless (equal? backend 'pardinus)
    (for ([f (in-list fmlas)])
      (define v (find-set-var f))
      (when v
        (raise-ho-error v (format "`set` quantifiers require the Pardinus backend (current backend: ~a)" backend)))))

  (: check-expr (-> node Void))
  (define (check-expr e)
    (define v (find-set-var e))
    (when v (raise-ho-error v REASON-EXPR)))

  ; neg?: under an odd number of negations
  ; blocked: #f, or the reason Pardinus will not skolemize at this position
  ; univ: number of enclosing non-skolemized quantified variables
  (: walk (-> node/formula Boolean (U False String) Integer Void))
  (define (walk f neg? blocked univ)
    (cond
      [(node/fmla/pred-spacer? f)
       (walk (node/fmla/pred-spacer-expanded f) neg? blocked univ)]
      [(node/formula/sealed? f)
       ; A sealed formula carries the actual formula in its info field
       (define inner : Any (node-info f))
       (when (node/formula? inner) (walk inner neg? blocked univ))]
      [(node/formula/quantified? f)
       (define q (node/formula/quantified-quantifier f))
       (define decls (node/formula/quantified-decls f))
       (define inner-neg? (if (equal? q 'no) (not neg?) neg?))
       (define existential? (if (equal? q 'all) inner-neg? (not inner-neg?)))
       (for ([d (in-list decls)]) (check-expr (cdr d)))
       (cond
         [(and existential? (not blocked))
          (walk (node/formula/quantified-formula f) inner-neg? blocked univ)]
         [else
          (for ([d (in-list decls)])
            (when (set-var? (car d))
              (raise-ho-error (car d) (or blocked REASON-NOT-EXISTENTIAL))))
          (define new-univ (+ univ (length decls)))
          (define new-blocked
            (or blocked
                (and (> new-univ skolem-depth)
                     (format "a `set` quantifier can be nested under at most skolem_depth (currently ~a) universally-quantified variables; see `option skolem_depth`"
                             skolem-depth))))
          (walk (node/formula/quantified-formula f) inner-neg? new-blocked new-univ)])]
      [(node/formula/op-on-formulas/!? f)
       (for ([c (in-list (node/formula/op-children f))])
         (when (node/formula? c) (walk c (not neg?) blocked univ)))]
      [(node/formula/op-on-formulas/&&? f)
       (define b (or blocked (and neg? "a `set` quantifier cannot appear under a negated `and`")))
       (for ([c (in-list (node/formula/op-children f))])
         (when (node/formula? c) (walk c neg? b univ)))]
      [(node/formula/op-on-formulas/||? f)
       (define b (or blocked (and (not neg?) "a `set` quantifier cannot appear under `or`")))
       (for ([c (in-list (node/formula/op-children f))])
         (when (node/formula? c) (walk c neg? b univ)))]
      [(node/formula/op-on-formulas/=>? f)
       (define children (node/formula/op-children f))
       (define left (car children))
       (define right (cadr children))
       (cond
         [neg?
          ; !(a => b) = a && !b
          (when (node/formula? left) (walk left #f blocked univ))
          (when (node/formula? right) (walk right #t blocked univ))]
         [else
          (define b (or blocked "a `set` quantifier cannot appear under `=>`, `iff`, or a formula if-then-else"))
          (when (node/formula? left) (walk left neg? b univ))
          (when (node/formula? right) (walk right neg? b univ))])]
      [(node/formula/op-on-formulas? f)
       ; Just temporal operators left 
       (define b (or blocked "a `set` quantifier cannot appear under a temporal operator"))
       (for ([c (in-list (node/formula/op-children f))])
         (when (node/formula? c) (walk c neg? b univ)))]
      [else
       ; Atomic formulas
       (check-expr f)]))

  (define initial-blocked
    (and (< skolem-depth 0)
         (format "a `set` quantifier requires skolemization, which is disabled because skolem_depth is ~a" skolem-depth)))
  (when (equal? backend 'pardinus)
    (for ([f (in-list fmlas)])
      (walk f #f initial-blocked 0))))
