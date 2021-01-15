#lang racket/base

(require racket/port)
(require forge/check-ex-spec/library)
(require (only-in racket/function curry))

(define (filter-commands parse-tree keep)
  (filter (lambda (line) (member (car line) keep)) parse-tree))

(define (read-syntax path port)
  (define test-file (read port))
  (unless (string? test-file)
    (raise "Argument error: expected string after #lang forge/testme; received ~a.~n" test-file))

  (define parse-tree (port->list read port))

  (define just-tests (filter-commands parse-tree '(example test inst fun)))

  (define module-datum `(module forge/check-ex-spec/core-mod racket
                          (require forge/testme/library)
                          (require json)

                          ; Auto-provide all defined values
                          (provide (except-out (all-defined-out)
                                               forge:n))

                          ; Used for evaluator
                          (define-namespace-anchor forge:n)
                          (forge:nsa forge:n)

                          (require ,test-file)

                          (define test-results
                            (list ,@(for/list ([test just-tests])
                                      (match-define (test-report name passed?) test)
                                      (hash 'name name 
                                            'passed? passed?))))


                          (println (jsexpr->string test-results))))
  (datum->syntax #f module-datum))

(provide read-syntax)