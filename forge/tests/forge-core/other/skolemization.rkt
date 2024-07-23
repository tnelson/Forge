#lang forge/core

; Check that Skolemization is happening as expected. In the Kodkod/Pardinus backend,
; Skolemization is done automatically by the engine, but may be influenced by both
; the skolem_depth option and the exact form of the constraints passed. 

(require (only-in rackunit check-eq? check-not-eq? check-true check-false)
         (only-in racket empty?))
(require racket/syntax syntax/parse)
(require (for-syntax racket/base racket/syntax syntax/parse))

(set-option! 'verbose 0)
; This module is _not_ meant to test the skolem_depth option. Set it high enough that all Skolemization
; possible should be performed, and then confirm it was.
(set-option! 'skolem_depth 3)

(sig A)

; A skolem-result struct contains an expectation:
;   "variable v will result in a skolem relation with this arity."
; If no Skolem relation should be produced, give #f as the arity to check that.

(struct skolem-result (v arity) #:transparent)
(define-syntax (check-skolemized stx)
  (syntax-parse stx
      [(_ run-name fmla result-list)
       (quasisyntax/loc stx
         (begin
           (run run-name #:preds [fmla])
           (define sat-gen (forge:make-model-generator (forge:get-result run-name) 'next))
           (define sat-soln (sat-gen))
           (check-true (forge:Sat? sat-soln))
           ;(printf "~a: ~a~n    ~a~n" 'run-name fmla (first (Sat-instances sat-soln)))
           (for ([r result-list])
             (define present? (hash-has-key? (first (Sat-instances sat-soln)) (skolem-result-v r)))
             (cond [(skolem-result-arity r)
                    (check-true present? (format "~a: expected ~a relation present but was not" 'run-name (skolem-result-v r)))
                    (when present?                      
                      (define val (hash-ref (first (Sat-instances sat-soln)) (skolem-result-v r)))
                      (check-true (equal? (skolem-result-arity r) (length (first val)))
                                  (format "~a: expected ~a relation to have arity ~a, but was not" 'run-name (skolem-result-v r) (skolem-result-arity r))))]
                   [else
                    (check-false (hash-has-key? (first (Sat-instances sat-soln)) (skolem-result-v r)))]))))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests for Skolem *Constants*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-skolemized single-0
                  (some ([x A]) (some x))
                  (list (skolem-result '$x 1)))
(check-skolemized notsingle-0
                  (all ([x A]) (some x))
                  (list (skolem-result '$x #f)))
(check-skolemized double-0
                  (some ([x A] [y A]) (and (some x) (some y)))
                  (list (skolem-result '$x 1) (skolem-result '$y 1)))
(check-skolemized mixed-0
                  (some ([x A]) (all ([y A]) (and (some x) (some y))))
                  (list (skolem-result '$x 1) (skolem-result '$y #f)))
(check-skolemized single-0-binary-conjunction
                  (&& (some ([x A]) (some x))
                      (some ([y A]) (some y)))
                  (list (skolem-result '$x 1) (skolem-result '$y 1)))

;;;;;;;;;;;;;;;;;;;;;;;;

(check-skolemized negation-single-0
                  (! (all ([x A]) (no x)))
                  (list (skolem-result '$x 1)))

; after NNF, same as single-0-binary-conjunction
(check-skolemized negation-single-0-binary-disjunction
                  (! (|| (all ([x A]) (no x))
                         (all ([y A]) (no y))))
                  (list (skolem-result '$x 1) (skolem-result '$y 1)))

(check-skolemized negation-single-0-trivial-disjunction
                  (! (|| (all ([x A]) (no x))))
                  (list (skolem-result '$x 1)))

; Kodkod/Pardinus will not Skolemize across a _syntactic_ disjunction (as of July 2024).
; However, this unnecessary AST node ought not to be kept. (It must be removed _before_
; the core-mapping is done, or the path-to-core response from the solver might be unsound.
; hence, we remove it in AST-node creation, just like Kodkod does.)
(check-skolemized negation-single-0-trivial-conjunction
                  (! (&& (all ([x A]) (no x))))
                  (list (skolem-result '$x 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests for Skolem *Functions* (input arity > 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; When testing Skolemization within a formula with an outer "all", avoid vacuous truth by adding (some Domain)
; Remember that negation and conjunction are "!" and "&&" respectively, _not_ "not" and "and".
(check-skolemized single-1
                  (&& (some A)
                       (all ([x A]) (some ([y A]) (and (some x) (some y)))))
                  (list (skolem-result '$x #f) (skolem-result '$y 2)))
             