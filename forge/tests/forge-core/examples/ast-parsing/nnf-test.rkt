#lang forge/core

(require forge/utils/to-nnf)
(require "test.frg")

(set-verbosity 0)


(run run-statement #:preds [])
(format "Not And: ~a" (interpret-formula run-statement not_and '() '() '()))
(format "Double Negation: ~a" (interpret-formula run-statement double_negation '() '() '()))
(format "For all Negation: ~a" (interpret-formula run-statement quant_negation '() '() '()))
(format "Exists Negation: ~a" (interpret-formula run-statement exists_negation '() '() '()))