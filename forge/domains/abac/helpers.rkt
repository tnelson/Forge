#lang forge/core

(require (only-in racket remove-duplicates flatten))
(require forge/domains/abac/lexparse) 
(provide (all-defined-out))

; Build a formula that is true IFF this rule matches
(define (build-rule-matches r request-vars relations var->maybe-skolem)
  ; Embed "some" quantifier if needed
  (define vars-used (remove-duplicates
                           (flatten (map (lambda (a) (atomic-fmla-args a))
                                         (append (extract-atomic-formulas-rule r))))))
  (define to-quantify (filter (lambda (v) (equal? #f (member v request-vars))) vars-used))
  ;(printf "to quantify: ~a~n" to-quantify)
  (define basef (&& (map (lambda (rc) (build-condition rc relations var->maybe-skolem)) (rule-conditions r))))
  (apply-quantifiers to-quantify basef))

; Datatype to represent an atomic formula
(struct atomic-fmla (pred args) #:transparent)
(define (condition->atomic-fmla c)
  (atomic-fmla (condition-pred c) (condition-args c)))
(define (extract-atomic-formulas-rule r)
  (remove-duplicates (foldl (lambda (c acc) (cons (condition->atomic-fmla c) acc))
                              empty
                              (rule-conditions r))))
(define (extract-atomic-formulas-policy pol)
  (remove-duplicates (foldl (lambda (r acc) (append (extract-atomic-formulas-rule r) acc))
                              empty
                              (policy-rules pol))))

; Build a formula (Forge AST) for a specific rule condition given by the parser
(define (build-condition c relations var->maybe-skolem)
  (cond [(condition-sign c)
         (in (list->product (map var->maybe-skolem (condition-args c)))
             (rel->unary (hash-ref relations (string-titlecase (symbol->string (condition-pred c))))))]
        [else
         (! (build-condition (condition #t (condition-pred c) (condition-args c)) relations))]))

(define (apply-quantifiers todo f)
  (if (empty? todo)
      f
      (some-quant/func '([(first todo) univ]) 
                       (apply-quantifiers (rest todo) f))))

; Convert a list of identifiers to a product
(define (list->product l)
  (foldl (lambda (i acc) (-> acc i)) (first l) (rest l)))

; Mechanism for building subsets of a sig (which might overlap) in forge/core
(sig True #:one)

; Replace a 2+-ary relation with (join R True)
(define (rel->unary r)
  (if (> (node/expr-arity r) 1)
      (join r True)
      r))
