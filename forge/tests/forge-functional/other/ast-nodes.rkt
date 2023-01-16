#lang forge/core

option run_sterling off
(set-option! 'verbose 0)

(require (prefix-in @ rackunit))

; Tests for AST node equality, etc.

(@check-equal? univ univ)
(@check-equal? true true)
(@check-equal? (join iden iden) (join iden iden))
(@check-equal? (in (join iden iden) (join iden iden))
               (in (join iden iden) (join iden iden)))
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



