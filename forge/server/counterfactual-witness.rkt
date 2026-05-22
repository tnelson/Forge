#lang racket/base

; Witness attribution for counterfactual UNSAT relaxation.
;
; When the solver returns a RelaxedSat (a SAT instance produced by dropping
; one or more core formulas), the dropped formulas are by definition false
; in the resulting instance. To help the visualizer point at *what made
; them false*, we walk each dropped formula's AST, collect the sigs/fields
; it references, and tag every tuple of those relations in the instance
; with a `violates_cN`="true" XML attribute. The N indexes into the
; RelaxedSat-dropped list, so the visualizer can correlate a flagged tuple
; with the specific dropped constraint.
;
; This is intentionally coarse: it doesn't say "this specific tuple is the
; one that violates phi" (that would be full Amalgam-style provenance,
; which needs an evaluator pass per dropped formula). It says "these
; relations are involved in the violation; look here." For most pedagogical
; specs this is enough signal to direct attention.
;
; Anything Pardinus couldn't map back to an AST node (entries that come
; through as strings in Unsat-core / RelaxedSat-dropped) gets no flagging
; — we have no AST to walk.

(require forge/lang/ast
         (only-in racket/list second remove-duplicates))

(provide collect-referenced-relations
         build-witness-tuple-annotations)

; Walk the AST rooted at `root`, gathering every node/expr/relation that
; appears anywhere in the formula or its sub-expressions. Quantifier
; variables, atoms, and constants are leaves with no relations. Decl
; domains are visited (so `all n: Node | ...` correctly attributes `Node`).
(define (collect-referenced-relations root)
  (define result '())
  (define (push! r) (set! result (cons r result)))

  (define (visit-decl-domain decl)
    ; A decl is `(var domain)` or `(var . domain)` depending on shape;
    ; mirror the same robustness as sigs-structs.rkt's `second/safe`.
    (cond [(list? decl) (when (>= (length decl) 2) (visit (second decl)))]
          [(pair? decl) (visit (cdr decl))]))

  (define (visit n)
    (cond
      ; Hit: a relation (sig or field).
      [(node/expr/relation? n) (push! n)]

      ; Expressions with explicit child lists.
      [(node/expr/op-on-exprs? n)
       (for-each visit (node/expr/op-on-exprs-children n))]
      [(node/expr/op-on-ints? n)
       (for-each visit (node/expr/op-on-ints-children n))]

      ; If-then-else expression.
      [(node/expr/ite? n)
       (visit (node/expr/ite-condition n))
       (visit (node/expr/ite-thene n))
       (visit (node/expr/ite-elsee n))]

      ; Comprehension `{ x: D | phi }`.
      [(node/expr/comprehension? n)
       (for-each visit-decl-domain (node/expr/comprehension-decls n))
       (visit (node/expr/comprehension-formula n))]

      ; Function call expanded form.
      [(node/expr/fun-spacer? n)
       (visit (node/expr/fun-spacer-expanded n))]

      ; Integer expressions with children.
      [(node/int/op-on-ints? n)
       (for-each visit (node/int/op-on-ints-children n))]
      [(node/int/op-on-exprs? n)
       (for-each visit (node/int/op-on-exprs-children n))]
      [(node/int/sum-quant? n)
       (for-each visit-decl-domain (node/int/sum-quant-decls n))
       (visit (node/int/sum-quant-int-expr n))]

      ; Formula operators.
      [(node/formula/op-on-formulas? n)
       (for-each visit (node/formula/op-on-formulas-children n))]
      [(node/formula/op-on-exprs? n)
       (for-each visit (node/formula/op-on-exprs-children n))]
      [(node/formula/op-on-ints? n)
       (for-each visit (node/formula/op-on-ints-children n))]

      ; Quantified formulas `all x: D | phi`, `some x: D | phi`, etc.
      [(node/formula/quantified? n)
       (for-each visit-decl-domain (node/formula/quantified-decls n))
       (visit (node/formula/quantified-formula n))]

      ; Multiplicity formulas (`some e`, `no e`, `lone e`, ...).
      [(node/formula/multiplicity? n)
       (visit (node/formula/multiplicity-expr n))]

      ; Predicate call expanded form.
      [(node/fmla/pred-spacer? n)
       (visit (node/fmla/pred-spacer-expanded n))]

      ; Everything else (quantifier vars, atoms, constants, sealed
      ; formulas we can't introspect) contributes no relations.
      [else (void)]))

  (visit root)
  (remove-duplicates result))

; Build a tuple-annotations hash suitable for modelToXML's
; `#:tuple-annotations` keyword arg.
;
;   dropped-formulas: List<(U node String)> — typically (RelaxedSat-dropped soln)
;   instance-by-symbol: HashTable<Symbol, List<Tuple>> — typically
;     (first (Sat-instances soln)), keyed by the engine's symbol names
;     (e.g., 'edges, 'Node).
;
; Returns: HashTable<Relation, HashTable<Tuple, List<(Pair Symbol Symbol)>>>
;   where each tuple of a relation referenced by dropped-formula i gets
;   the annotation `violates_cI` = `true`.
;
; A tuple in a relation that appears in multiple dropped formulas gets
; multiple violates_cN annotations — they accumulate via cons.
(define (build-witness-tuple-annotations dropped-formulas instance-by-symbol)
  (define result (make-hash))

  (for ([dropped (in-list dropped-formulas)]
        [idx (in-naturals)])
    (define annotation-pair
      (cons (string->symbol (format "violates_c~a" idx)) 'true))
    (define relations
      (cond [(node? dropped) (collect-referenced-relations dropped)]
            [else '()]))  ; opaque string entries: skip

    (for ([rel (in-list relations)])
      (define rel-sym (string->symbol (relation-name rel)))
      (when (hash-has-key? instance-by-symbol rel-sym)
        (define rel-tuples (hash-ref instance-by-symbol rel-sym))
        (unless (hash-has-key? result rel)
          (hash-set! result rel (make-hash)))
        (define per-tuple (hash-ref result rel))
        (for ([tup (in-list rel-tuples)])
          (define existing (hash-ref per-tuple tup '()))
          (hash-set! per-tuple tup (cons annotation-pair existing))))))

  result)
