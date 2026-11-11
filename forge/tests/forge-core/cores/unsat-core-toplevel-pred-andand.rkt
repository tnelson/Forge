#lang forge/core

(require (only-in "helpers/unsat-core-toplevel-pred-andand.frg" predRun)
         "helpers/check-core.rkt")

(check-core predRun)
