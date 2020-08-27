#lang racket/base

(require racket/port)
(require racket/match)

(define (read-syntax path port)
  (define assignment (read port))
  (unless (string? assignment)
    (raise "Argument error: expected string after #lang forge/check-ex-spec; received ~a.~n" assignment))

  (define parse-tree (port->list read port))

  (define module-datum `(module forge-core/check-ex-spec-mod racket
                          (require forge/sigs)

                          ; Auto-provide all defined values
                          (provide (except-out (all-defined-out)
                                               forge:n))

                          ; Used for evaluator
                          (define-namespace-anchor forge:n)
                          (forge:nsa forge:n)

                          ; Enable check-ex-spec commands and load TA solution
                          (require forge/check-ex-spec/library)
                          (check-ex-spec:load-assignment ,assignment)
                          
                          ,@parse-tree))
  (datum->syntax #f module-datum))

(provide read-syntax)