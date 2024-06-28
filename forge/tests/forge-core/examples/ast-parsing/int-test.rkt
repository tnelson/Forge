#lang forge/core

(require (prefix-in @ (only-in forge/utils/integer-converter interpret-formula)))
(require forge/utils/to-skolem)
(require "int-test.frg")

(set-verbosity 0)

(run run-statement #:preds [])

(define int-reconciled-fmla (@interpret-formula run-statement is_adult '() '() '()))
(printf "Int-reconciled formula: ~a~n" int-reconciled-fmla)
(define-values (skolemized-fmla bounds) (interpret-formula run-statement int-reconciled-fmla '() '() '() '()))
(printf "Skolemized formula: ~a~n" skolemized-fmla)

