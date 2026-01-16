#lang forge/core
;; Test that runs ARE inherited on import
;; helpers/import-runs-B.frg defines a run called myRun
;; We verify that myRun exists in curr-state after importing

(require "helpers/import-runs-B.frg")
(require (only-in rackunit check-true))

(set-option! 'verbose 0)
(set-option! 'run_sterling 'off)

(check-true (hash-has-key? (forge:State-runmap forge:curr-state) 'myRun)
            "myRun should be inherited from imported module")
