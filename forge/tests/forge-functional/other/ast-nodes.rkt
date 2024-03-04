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
; Not testing equality for sig/relation nodes across aliasing here,
; since that's only done in the Forge expander at the moment.
; In forge/core, syntax location must be modified manually.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;