#lang racket/base

; Shared helpers for tests that check for warnings. Warnings don't affect the 
; result like `is forge_error` does, so they need special handling for the moment.

(require rackunit
         racket/system
         racket/port
         racket/runtime-path)

(provide check-silent)

(define-runtime-path here ".")

(define (run-forge-file-capture-output filename)
  (define filepath (path->string (build-path here filename)))
  (define output (open-output-string))
  (parameterize ([current-output-port output]
                 [current-error-port output])
    (system (format "racket ~a" filepath)))
  (get-output-string output))

; Runs `filename` and then checks:
;  (1) no output matches `rx`;
;  (2) and no test failed.
(define (check-silent filename rx)
  (define combined-output (run-forge-file-capture-output filename))
  (check-false (regexp-match? rx combined-output)
               (format "Unexpected warning matching ~a. Full output:\n~a" rx combined-output))
  (check-false (regexp-match? #rx"Failed test" combined-output)
               (format "Helper test failed. Full output:\n~a" combined-output)))
