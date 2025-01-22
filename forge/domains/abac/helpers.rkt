#lang forge/core

(require (only-in racket remove-duplicates flatten last))
(require forge/domains/abac/lexparse) 

; Import the domain model. This contains sig and field definitions.
(require "abac.frg")

(provide build-rule-matches
         build-condition
         apply-quantifiers
         condition->atomic-fmla
         rel->unary 
         list->product
         (struct-out atomic-fmla)
         (all-from-out "abac.frg"))

; Build a formula that is true IFF this rule matches
(define (build-rule-matches r request-vars relations var->maybe-skolem)
  ; Embed "some" quantifier if needed
  (define vars-used (remove-duplicates
                           (flatten (map (lambda (a) (atomic-fmla-args a))
                                         (append (extract-atomic-formulas-rule r))))))
  (define to-quantify (filter (lambda (v) (equal? #f (member v request-vars))) vars-used))
  ;(printf "to quantify: ~a~n" to-quantify)
  (define basef (&& (map (lambda (rc) (build-condition relations var->maybe-skolem rc)) (rule-conditions r))))
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
(define (build-condition relations var->maybe-skolem c)
  (define args (condition-args c))
  (cond [(condition-sign c)
         ; Build positive Forge literal formula 
         (in (list->product (map var->maybe-skolem args))
             (rel->unary (hash-ref relations (string-titlecase (symbol->string (condition-pred c))))))]
        [else
         ; Build negative Forge literal formula
         (! (build-condition relations var->maybe-skolem (condition #t (condition-pred c) args)))]))

(define (apply-quantifiers todo f)
  (if (empty? todo)
      f
      (some-quant/func '([(first todo) univ]) 
                       (apply-quantifiers (rest todo) f))))

; Convert a list of identifiers to a product
(define (list->product l)
  (foldl (lambda (i acc) (-> acc i)) (first l) (rest l)))

; Replace a 2+-ary relation that has a "fake" column on True with (join R True)
(define (rel->unary r)
  (define types ((relation-typelist-thunk r)))
  (cond [(and (node/expr/relation? r) 
              (> (node/expr-arity r) 1)
              (equal? "True" (last types)))
         (join r True)]
        [else r]))
  