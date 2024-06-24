#lang forge/core

(require forge/utils/identity)
(require "test.frg")

(set-verbosity 0)


(run run-statement #:preds [])

(format "~a" (interpret-formula run-statement age_limits '() '() '()))