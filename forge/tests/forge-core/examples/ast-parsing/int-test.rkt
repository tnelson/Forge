#lang forge/core

; DEPRECATED

;(require (prefix-in @int- (only-in forge/utils/integer-converter interpret-formula)))
;(require forge/utils/to-skolem)
;(require forge/utils/to-smtlib-tor)
;(require "int-test.frg")

;(set-verbosity 10)

;(run run-statement #:preds [])

;(define int-reconciled-fmla (@int-interpret-formula run-statement is_adult '() '() '()))
;(printf "Int-reconciled formula: ~a~n" int-reconciled-fmla)
;; (interpret-formula run-or-state formula relations atom-names quantvars)  
;(define-values (skolemized-fmla bounds) (@int-interpret-formula run-statement int-reconciled-fmla '() '() '()))
;(printf "Skolemized formula: ~a~n" skolemized-fmla)
;(define smtlib-string (convert-formula run-statement skolemized-fmla '() '() '()))
;(printf "SMT-LIB string: ~a~n" smtlib-string)
