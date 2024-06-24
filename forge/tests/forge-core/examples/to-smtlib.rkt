#lang forge/core

(require forge/utils/to-smtlib-uflia)
(require forge/sigs-structs)
(require "test2.frg")

(set-verbosity 0)


(run run-statement
     #:preds [])


(printf (interpret-formula run-statement familyFact '() '() '()))