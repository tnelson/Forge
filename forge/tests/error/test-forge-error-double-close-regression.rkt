#lang racket/base

; Regression test: when multiple `is forge_error` tests fail (because no
; error is produced), followed by another test, Pardinus should not crash.
;
; Previously, close-run was called twice on the same run:
; 1. Once in the test-body macro after the run completed
; 2. Once in report-test-failure when adding the next failure
; This caused Pardinus to receive duplicate (clear) commands, corrupting
; its state and crashing when the next test tried to run.
;
; The fix made close-run idempotent by checking is-run-closed? first.

(require rackunit
         racket/system
         racket/port
         racket/runtime-path)

(define-runtime-path here ".")

;; Helper: run a Forge file as a subprocess and capture all output
(define (run-forge-file-capture-output filename)
  (define filepath (path->string (build-path here filename)))
  (define output (open-output-string))

  (parameterize ([current-output-port output]
                 [current-error-port output])
    (system (format "racket ~a" filepath)))

  (get-output-string output))

;; The test
(define (test-no-pardinus-crash)
  (define combined-output
    (run-forge-file-capture-output "forge-error-double-close-helper.frg"))

  ;; Pardinus should NOT crash
  (check-false
   (regexp-match? #rx"shut down unexpectedly" combined-output)
   (format "Pardinus crashed (double-close bug). Full output:\n~a" combined-output))

  ;; t3 should pass despite t1/t2 failing
  (check-true
   (regexp-match? #rx"Test passed: t3" combined-output)
   (format "t3 should pass after failed forge_error tests. Full output:\n~a" combined-output)))

;; Run the test
(module+ test
  (test-no-pardinus-crash))

(module+ main
  (test-no-pardinus-crash)
  (displayln "Regression test passed!"))
