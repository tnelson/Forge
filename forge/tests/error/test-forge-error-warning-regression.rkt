#lang racket/base

; Regression test: the test_keep option controls whether Forge stops
; after the first test failure or continues to run until all tests 
; have finished. 
;
; This test covers an issue where, when `test_keep` was set to `last`
; (i.e., all tests would run), and a test failed, a warning would say 
; that the test was successful but state hadn't been cleaned up. This 
; made results confusing to read. 
; 
; This warning should only appear when there is an actual issue cleaning
; up the run state, because

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
(define (test-no-spurious-warning)
  (define combined-output
    (run-forge-file-capture-output "forge-error-warning-test-helper.frg"))

  ;; The test in the helper file should FAIL (no forge error produced)
  ;; Check that we get a failure message, not the misleading warning

  ;; This warning should NOT appear for a FAILED test
  (check-false
   (regexp-match? #rx"successful.*forge_error.*test run left in state" combined-output)
   (format "Spurious warning appeared in output. Full output:\n~a" combined-output))

  ;; The test should fail with "No Forge error was produced"
  (check-true
   (regexp-match? #rx"No Forge error was produced" combined-output)
   (format "Expected 'No Forge error was produced' message. Full output:\n~a" combined-output)))

;; Run the test
(module+ test
  (test-no-spurious-warning))

(module+ main
  (test-no-spurious-warning)
  (displayln "Regression test passed!"))
