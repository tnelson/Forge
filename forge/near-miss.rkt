#lang racket/base

(require racket/lazy-require racket/list racket/match
         (only-in forge/sigs-structs Run-run-spec Run-command Run-result Run-atoms Run-spec-preds Run-spec-state
                  Unsat? Unsat-kind Sat? Relation? Relation-name get-option
                  get-all-rels is-solver-process-alive? close-run)
         forge/evaluator
         (prefix-in ast: forge/lang/ast)
         (only-in forge/utils/substitutor substitute-formula)
         (prefix-in tree: forge/utils/lazy-tree))

;; sigs-functional also uses the Sterling server. Load the solver entry point
;; on the first diagnostic request, after the model has finished loading.
(lazy-require [forge/sigs-functional (run-from-state)])
(provide near-miss-available? near-miss-rules find-near-miss (struct-out relaxed-formula))

(struct relaxed-formula (formula cases) #:transparent)

(define (near-miss-available? run result)
  (and (Unsat? result) (memq (Unsat-kind result) '(unsat no-counterexample))
       (eq? 'pardinus (get-option run 'backend))
       (eq? 'default (get-option run 'problem_type))
       ;; Reproduce only runs whose source command records its inputs.
       ;; Direct functional callers can supply an opaque or synthetic command.
       (command-option run '#:preds)
       (Unsat? (tree:get-value (Run-result run)))
       (is-solver-process-alive? run)))

(define (unfold f)
  (if (ast:node/fmla/pred-spacer? f) (unfold (ast:node/fmla/pred-spacer-expanded f)) f))

;; Split only conjunctions in positive position. In a check of A => P, the
;; command's negation is equivalent to A and !P. Keep !P as the fixed goal.
(define (conjuncts f)
  (define body (unfold f))
  (if (ast:node/formula/op-on-formulas/&&? body)
      (append-map conjuncts (ast:node/formula/op-children body))
      (list f)))

(define (command-option run keyword)
  (define args (syntax->list (Run-command run)))
  (and (pair? args)
       (for/first ([key args] [value (cdr args)] #:when (eq? keyword (syntax-e key))) value)))

;; Forge expands a check into a run with (! (&& ...)) as its goal.
;; This syntax survives compilation, including when expansion-origin metadata
;; has been stripped from a cached model.
(define (check-command? run)
  (define preds (command-option run '#:preds))
  (and preds (match (syntax->datum preds)
               [(list (list '! (list '&& _ ...))) #t]
               [_ #f])))

;; Fixed formulas and relaxable formulas, in the order used by pins and previous corrections.
(define (near-miss-rules run)
  (define preds (Run-spec-preds (Run-run-spec run)))
  (match preds
    [(list (ast:node/formula/op-on-formulas/! _ (list property)))
     #:when (check-command? run)
     (match (unfold property)
       [(ast:node/formula/op-on-formulas/=> _ (list assumptions goal))
        (values (list (ast:!/func goal)) (conjuncts assumptions))]
       [_ (values preds '())])]
    [_ (values '() (append-map conjuncts preds))]))

;; Record the value of an operand and, where it reads a field directly, the
;; underlying field tuples in declaration order. Derived values (e.g. closure)
;; need not be members of any field. Keep that distinction for all arities.
(define (observation run expr [mult #f])
  (define value (evaluate run '() expr))
  (define entry (hash 'expression expr 'value value))
  (define result (if mult (hash-set entry 'multiplicity mult) entry))
  (define-values (field rows)
    (match expr
      [(? Relation? r) (values r value)]
      [(ast:node/expr/op-on-exprs/join _ _ (list (ast:node/expr/atom _ _ a) (? Relation? r)))
       (values r (for/list ([row value]) (cons a row)))]
      [(ast:node/expr/op-on-exprs/join _ _ (list (? Relation? r) (ast:node/expr/atom _ _ a)))
       (values r (for/list ([row value]) (append row (list a))))]
      [_ (values #f '())]))
  (if field
      (hash-set (hash-set result 'relation (Relation-name field)) 'tuples rows)
      result))

(define (explain run formula)
  (define (walk f bindings)
    (define (result observations) (list (hash 'bindings bindings 'evaluations observations)))
    ;; Some evaluators cannot inspect every subexpression. The source formula
    ;; and any bindings already established still belong to the relaxation.
    (with-handlers ([exn:fail? (lambda (_) (result '()))])
      (match (unfold f)
        [(ast:node/formula/quantified _ q decls body)
         (define query (ast:set/func decls (if (eq? q 'all) (ast:!/func body) body)))
         (define matches (evaluate run '() query))
         (if (null? matches)
             (result (list (hash 'expression query 'value '())))
             (append*
              (for/list ([row matches])
                (define assignment
                  (for/list ([d decls] [a row])
                    (hash 'variable (car d) 'atom a)))
                (define grounded
                  (for/fold ([acc body]) ([d decls] [a row])
                    (substitute-formula run acc (get-all-rels run) (Run-atoms run) '()
                                        (car d) (if (number? a) (ast:sing/func (ast:int/func a))
                                                    (ast:atom/func a)))))
                (walk grounded (append bindings assignment)))))]
        [(ast:node/formula/multiplicity _ mult expr) (result (list (observation run expr mult)))]
        [(ast:node/formula/op-on-formulas/! _ (list child)) (walk child bindings)]
        [(? ast:node/formula/op? op)
         (result (for/list ([child (ast:node/formula/op-children op)]) (observation run child)))]
        [_ (result '())])))
  (relaxed-formula formula (walk formula '())))

(define (original-bounds run model-ns)
  ;; Use the lexical context of the loaded model. Inst objects reapply their
  ;; bounds and "is" breakers every time run-from-state constructs a run.
  (define bounds (command-option run '#:bounds))
  (if bounds
      (for/list ([expr (or (syntax->list bounds) (list bounds))]) (eval-syntax expr model-ns))
      '()))

(define (original-scope run model-ns)
  ;; run-from-state expects the numeric scopes from *before* an inst block
  ;; narrowed them. Forge supplies the defaults for omitted scopes and Int.
  (define scope (command-option run '#:scope))
  (for/list ([clause (if scope (syntax->list scope) '())])
    (match-define (cons sig numbers) (syntax->list clause))
    (cons (eval-syntax sig model-ns) (map syntax-e numbers))))

;; Return the Sat witness, relaxed rule indices, and their evidence on this witness.
;; If no correction remains, return #f and two empty lists.
;; Grow a satisfiable subset of the source conjuncts with fixed structure,
;; bounds and pinned rules. Each rejected addition is UNSAT, and stays UNSAT
;; as the retained set grows. Its complement is an inclusion-minimal correction.
(define (find-near-miss original result pins model-ns [previous '()])
  (unless (near-miss-available? original result) (error 'near-miss "This result cannot be diagnosed."))
  (define-values (fixed rules) (near-miss-rules original))
  (define (rule i) (list-ref rules i))
  (define (rule-indices? ids)
    (and (list? ids) (andmap (lambda (i) (and (exact-nonnegative-integer? i) (< i (length rules)))) ids)))
  (unless (and (rule-indices? pins) (list? previous)
               (andmap (lambda (ids) (and (pair? ids) (rule-indices? ids))) previous))
    (ast:raise-forge-error #:msg "Unknown near-miss rule selection." #:context (Run-command original)))
  ;; Each previous correction set must now have at least one satisfied member.
  ;; Grow the retained set under these disjunctions. At the end it contains a
  ;; member of every disjunction, so the correction is minimal even without
  ;; the blocking clauses. Rule indices distinguish equal-looking source sites.
  (define blocks (for/list ([dropped previous]) (apply ast:||/func (map rule dropped))))
  (define kept (append blocks fixed (map rule pins)))
  (define eligible (for/list ([i (in-range (length rules))] #:unless (member i pins)) i))
  (define spec (Run-run-spec original))
  (define scope (original-scope original model-ns))
  (define bounds (original-bounds original model-ns))
  (define (trial preds proc)
    (define run
      (run-from-state (Run-spec-state spec) #:name (gensym 'nearMiss)
                      #:scope scope #:bounds bounds #:preds preds
                      #:command (Run-command original)))
    (dynamic-wind void (lambda () (proc run (tree:get-value (Run-result run)))) (lambda () (close-run run))))
  (define (sat-result? run result)
    (cond [(Sat? result) #t] [(Unsat? result) #f] [else (error 'near-miss "Solver returned unknown.")]))
  (cond [(or (null? eligible) (not (trial kept sat-result?)))
         (values #f '() '())]
        [else
         (define dropped
           (for/fold ([dropped '()]) ([i eligible])
             (define candidate (cons (rule i) kept))
             (if (trial candidate sat-result?)
                 (begin (set! kept candidate) dropped)
                 (cons i dropped))))
         (when (null? dropped) (error 'near-miss "The original conflict could not be reproduced."))
         (define ids (sort dropped <))
         (trial kept (lambda (run sat)
                       (unless (Sat? sat) (error 'near-miss "Unable to reproduce the witness."))
                       (values sat ids (for/list ([i ids]) (explain run (rule i))))))]))
