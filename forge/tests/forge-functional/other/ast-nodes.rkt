#lang forge/core

(set-option! 'verbose 0)

(require (prefix-in @ rackunit))

; Tests for AST node equality, etc.
; Equality should apply irrespective of source location information.

; Note there is some test-duplication between here and the forge-core version.
;  DO NOT DELETE ONE IN FAVOR OF THE OTHER (especially without confirming that they are 
;  in fact identical!)

(@check-equal? univ univ)
(@check-equal? true true)
(@check-equal? (join iden iden) (join iden iden))
(@check-equal? (in (join iden iden) (join iden iden))
               (in (join iden iden) (join iden iden)))

; Quantifier variables (node/expr/quantifier-var) have a "sym" field that should be
; distinct, even if the variable name is the same. This helps to detect shadowing, etc.
; But as a result, it is not safe to consider "x" = "x" here.
(@check-not-equal? (some ([x univ]) (in x x))
                   (some ([x univ]) (in x x)))
(@check-equal? (some univ)
               (some univ))

(@check-not-equal? univ iden)
(@check-not-equal? (join iden iden iden) (join iden iden))
(@check-not-equal? (join univ iden) (join iden iden))
(@check-not-equal? true false)
(@check-not-equal? (in (join iden iden) iden)
                   (in (join iden iden) (join iden iden)))
(@check-not-equal? (some ([x univ]) (in x x))
                   (all ([x univ]) (in x x)))
(@check-not-equal? (some ([x univ]) (in x x))
                   (some ([y univ]) (in y y)))
(@check-not-equal? (some ([x univ]) (in x x))
                   (some ([x none]) (in x x)))
(@check-not-equal? (some univ)
                   (lone univ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Safe; truth should be implied by fact equal values should have equal hashes
(@check-equal? (equal-hash-code univ)
               (equal-hash-code univ))
(@check-equal? (equal-hash-code true)
               (equal-hash-code true))
(@check-equal? (equal-hash-code (join iden iden))
               (equal-hash-code (join iden iden)))
(@check-equal? (equal-hash-code (in (join iden iden) (join iden iden)))
               (equal-hash-code (in (join iden iden) (join iden iden))))

; Unsafe: small chance will fail since non-equal values may hash the same
(@check-not-equal? (equal-hash-code univ)
                   (equal-hash-code iden))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Test equality for Sig and Relation nodes across aliasing
(sig A)
(sig B)
(@check-not-equal? A B)
(@check-true (node/expr? A))

(pred (isA x) (= A x))
(define isA_B (isA B))
(@check-true (node/formula? isA_B))
(@check-true (node/fmla/pred-spacer? isA_B))

;; remove the spacer and enclosing &&, etc.
(define isA_B_inner_eq (first (node/formula/op-children (node/fmla/pred-spacer-expanded isA_B))))
(define use_A (first (node/formula/op-children isA_B_inner_eq)))
; definition node and use node are considered equal
(@check-equal? A use_A)
; but they carry different location information
(@check-not-equal? (srcloc-line (nodeinfo-loc (node-info A))) 
                   (srcloc-line (nodeinfo-loc (node-info use_A))))




