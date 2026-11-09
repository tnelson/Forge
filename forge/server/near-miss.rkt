#lang racket/base

(require racket/lazy-require racket/list racket/match racket/string racket/format racket/path
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
(provide near-miss-available? near-miss-options find-near-miss)

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

;; Stock Forge expands a check into a run with (! (&& ...)) as its goal.
;; This syntax survives compilation, including when expansion-origin metadata
;; has been stripped from a cached model.
(define (check-command? run)
  (define preds (command-option run '#:preds))
  (and preds (match (syntax->datum preds)
               [(list (list '! (list '&& _ ...))) #t]
               [_ #f])))

(define (command-parts run)
  (define preds (Run-spec-preds (Run-run-spec run)))
  (match preds
    [(list (ast:node/formula/op-on-formulas/! _ (list property)))
     #:when (check-command? run)
     (match (unfold property)
       [(ast:node/formula/op-on-formulas/=> _ (list assumptions goal))
        (values (list (ast:!/func goal)) (conjuncts assumptions))]
       [_ (values preds '())])]
    [_ (values '() (append-map conjuncts preds))]))

(define (rule-label f)
  (define loc (ast:nodeinfo-loc (ast:node-info (unfold f))))
  (define label
    (if (ast:node/fmla/pred-spacer? f)
        (format "~a: ~a" (ast:node/fmla/pred-spacer-name f) (ast:deparse f))
        (ast:deparse f)))
  (if (and (srcloc? loc) (path? (srcloc-source loc)) (srcloc-line loc))
      (format "~a:~a · ~a" (file-name-from-path (srcloc-source loc)) (srcloc-line loc) label)
      label))

(define (near-miss-options run)
  (define-values (fixed rules) (command-parts run))
  (hash 'rules (map rule-label rules) 'fixed (map ast:deparse fixed)))

;; Evidence is evaluated on the same SAT solution used for the diagram. A
;; comprehension finds a counterbinding; substitution lets Forge evaluate the
;; actual relation at those atoms without reparsing source strings.
(define (explain run formula)
  (define lines '())
  (define atoms '())
  (define edges '())
  (define (say s) (set! lines (cons s lines)))
  (define (ev n) (evaluate run '() n))
  (define (remember tuples)
    (set! atoms (append (map ~a (append* tuples)) atoms)))
  (define (values-of expr)
    (define val (ev expr))
    (cond
      [(list? val)
       (define shown (take val (min 8 (length val))))
       (remember shown)
       (say (format "~a = {~a}~a" (ast:deparse expr)
                    (string-join (for/list ([t shown]) (string-join (map ~a t) " → ")) ", ")
                    (if (> (length val) 8) (format " (first 8 of ~a tuples)" (length val)) "")))
       ;; Only annotate real binary field tuples. A join/closure result may
       ;; be derived, and an absent tuple must never be drawn as an actual edge.
       (define-values (rel tuples)
         (match expr
           [(? Relation? r) (values r shown)]
           [(ast:node/expr/op-on-exprs/join _ _ (list (ast:node/expr/atom _ _ a) (? Relation? r)))
            (values r (map (lambda (t) (cons a t)) shown))]
           [_ (values #f '())]))
       (when (and rel (= 2 (ast:node/expr-arity rel)))
         (for ([t tuples] #:when (= 2 (length t)))
           (set! edges (cons (hash 'relation (~a (Relation-name rel))
                                   'source (~a (first t)) 'target (~a (second t))) edges))))]
      [else (say (format "~a = ~a" (ast:deparse expr) val))])
    val)
  (define (walk f)
    (match (unfold f)
      [(ast:node/formula/quantified _ q decls body)
       (define matches (ev (ast:set/func decls (if (eq? q 'all) (ast:!/func body) body))))
       (cond [(null? matches)
              (say (if (eq? q 'all) "Every binding satisfies the body."
                       "No binding satisfies the body."))]
             [else
              (define tuple (first matches))
              (remember (list tuple))
              (say (format "With ~a, the quantified body is ~a."
                           (string-join (for/list ([d decls] [v tuple])
                                          (format "~a = ~a" (ast:deparse (car d)) v)) ", ")
                           (if (eq? q 'all) "false" "true")))
              (define grounded
                (for/fold ([acc body]) ([d decls] [v tuple])
                  (substitute-formula run acc (get-all-rels run) (Run-atoms run) '()
                                      (car d) (if (number? v) (ast:sing/func (ast:int/func v))
                                                  (ast:atom/func v)))))
              (walk grounded)])]
      [(ast:node/formula/multiplicity _ mult expr)
       (define tuples (values-of expr))
       (define required (hash-ref (hash 'lone "at most one" 'one "exactly one"
                                        'no "zero" 'some "at least one") mult #f))
       (when (and required (list? tuples))
         (say (format "The expression contains ~a ~a. The source multiplicity ~a requires ~a."
                      (length tuples) (if (= 1 (length tuples)) "tuple" "tuples") mult required)))]
      [(ast:node/formula/op-on-exprs _ children) (for-each values-of children)]
      [(ast:node/formula/op-on-ints _ children) (for-each values-of children)]
      [(ast:node/formula/op-on-formulas/! _ (list child)) (walk child)]
      [(ast:node/formula/op-on-formulas _ children)
       (for ([child children]) (say (format "~a is ~a." (ast:deparse child) (if (ev child) "true" "false"))))]
      [_ (void)]))
  ;; A language-specific evaluator may not support every derived subexpression.
  ;; Minimality still certifies that the original source formula is false.
  (with-handlers ([exn:fail? (lambda (_) (void))]) (walk formula))
  (hash 'rule (rule-label formula) 'details (reverse lines)
        'atoms (remove-duplicates atoms) 'edges (remove-duplicates edges)))

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

;; Grow a satisfiable subset of the source conjuncts with fixed structure,
;; bounds and pinned rules. Each rejected addition is UNSAT, and stays UNSAT
;; as the retained set grows. Its complement is an inclusion-minimal correction.
(define (find-near-miss original result pins model-ns [previous '()])
  (unless (near-miss-available? original result) (error 'near-miss "This result cannot be diagnosed."))
  (define-values (fixed rules) (command-parts original))
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
