#lang racket/base

(require forge/lang/alloy-syntax/parser)
(require forge/lang/alloy-syntax/tokenizer)
(require (only-in forge/lang/reader
                  coerce-ints-to-atoms))
(require forge/check-ex-spec/library)
(require (only-in racket/function curry))
(require racket/match)
; (require racket/list)

(define (filter-commands stx keep)
  (match-define (cons alloy-module commands) (syntax->list stx))
  (define filtered (filter (lambda (line) (member (car (syntax->datum line)) keep))
                           commands))
  (datum->syntax stx (cons alloy-module filtered) stx stx stx))

(define (read-syntax path port)
  (define test-file (read port))
  (unless (string? test-file)
    (raise (format "Argument error: expected string after #lang forge/check-ex-spec; received ~a.~n" test-file)))

  (define parse-tree (parse path (make-tokenizer port)))
  (define ints-coerced (coerce-ints-to-atoms parse-tree))

  (define non-tests (cdr (syntax->list (filter-commands ints-coerced '(InstDecl OptionDecl)))))
  (define just-tests (cdr (syntax->list (filter-commands ints-coerced '(ExampleDecl TestExpectDecl)))))

  (define module-datum `(module forge/testme-mod forge/testme/lang/expander
                          (require forge/testme/library)
                          (require (prefix-in @ racket))
                          (require json)

                          ,@non-tests

                          (set-verbosity 0)

                          ; Auto-provide all defined values
                          (provide (except-out (all-defined-out)
                                               forge:n))

                          ; Used for evaluator
                          (define-namespace-anchor forge:n)
                          (forge:nsa forge:n)

                          (require ,test-file)

                          (@define test-results
                            (@map (@lambda (test-result)
                                    (@match-define (test-report name passed?) test-result)
                                    (@hash 'name (@symbol->string name)
                                                   'passed? passed?))
                                  (@apply @append
                                          (for/list ([test-list (list ,@just-tests)])
                                            (for/list ([test test-list])
                                              test)))))


                          (@displayln (jsexpr->string test-results))))
  (datum->syntax #f module-datum))
(provide read-syntax)