#lang racket/base

; In some situations, Forge will issue a missing-scope warning. 

(require rackunit
         racket/system
         racket/port
         racket/runtime-path)

(define-runtime-path here ".")

(define (run-forge-file-capture-output filename)
  (define filepath (path->string (build-path here filename)))
  (define output (open-output-string))
  (parameterize ([current-output-port output]
                 [current-error-port output])
    (system (format "racket ~a" filepath)))
  (get-output-string output))

(module+ test
  (define combined-output (run-forge-file-capture-output "missing-scope-warning-none-helper.frg"))
  (check-false (regexp-match? #rx"No scope was given" combined-output)
               (format "Unexpected missing-scope warning. Full output:\n~a" combined-output))
  (check-false (regexp-match? #rx"Failed test" combined-output)
               (format "Helper test failed. Full output:\n~a" combined-output)))
