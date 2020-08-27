#lang racket/base

(require forge/lang/alloy-syntax/parser)
(require forge/lang/alloy-syntax/tokenizer)
(require (only-in forge/lang/reader
                  coerce-ints-to-atoms))

(define (read-syntax path port)
  (define assignment (read port))
  (unless (string? assignment)
    (raise "Argument error: expected string after #lang forge/check-ex-spec; received ~a.~n" assignment))

  (define parse-tree (parse path (make-tokenizer port)))
  (define ints-coerced (coerce-ints-to-atoms parse-tree))

  (define module-datum `(module forge-mod forge/check-ex-spec/lang/expander
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

                          ,ints-coerced))
  (datum->syntax #f module-datum))
(provide read-syntax)