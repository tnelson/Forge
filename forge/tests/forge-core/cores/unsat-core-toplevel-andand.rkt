#lang forge/core

(require (only-in "helpers/unsat-core-toplevel-andand.frg" inlineRun)
         "helpers/check-core.rkt")

(check-core inlineRun)
