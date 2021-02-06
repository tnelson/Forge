#lang racket/base

(require forge/lang/alloy-syntax/parser)
(require forge/lang/alloy-syntax/tokenizer)
(require (only-in forge/lang/reader
                  coerce-ints-to-atoms))
(require forge/check-ex-spec/library)
(require (only-in racket/function curry))
(require racket/match)
(require (prefix-in logging: forge/logging/logging)
         (prefix-in logging:check-ex-spec: forge/logging/check-ex-spec/main))
; (require racket/list)

(define (filter-commands stx keep)
  (match-define (cons alloy-module commands) (syntax->list stx))
  (define filtered (filter (lambda (line) (member (car (syntax->datum line)) keep))
                           commands))
  (datum->syntax stx (cons alloy-module filtered) stx stx stx))

(define (filter-names commands provided)
  (filter (lambda (command)
            (let ([command (syntax->datum command)])
              (not (and (member (car command) '(PredDecl FunDecl))
                        (if (symbol? (cadr command))
                            (member (cadr command) provided)
                            (member (cadadr command) provided))))))
          commands))

(define (read-syntax path port)
  (define-values (logging-on? assignment-name user) (logging:log-execution 'forge/check-ex-spec port path))
  (unless (string? assignment-name)
    (raise (format "Argument error: expected string after #lang forge/check-ex-spec; received ~a.~n" assignment-name)))

  (define assignment-info (check-ex-spec:get-info assignment-name))
  (define wheats (map (curry format "~a.rkt" ) (hash-ref assignment-info 'wheats)))
  (define chaffs (map (curry format "~a.rkt" ) (hash-ref assignment-info 'chaffs)))
  (define provided (map string->symbol (hash-ref assignment-info 'provides)))

  (define parse-tree (parse path (make-tokenizer port)))
  (define ints-coerced (coerce-ints-to-atoms parse-tree))

  (define not-tests (cdr (syntax->list (filter-commands ints-coerced '(InstDecl OptionDecl PredDecl FunDecl)))))
  (define safe-not-tests (filter-names not-tests provided))
  (define just-tests (cdr (syntax->list (filter-commands ints-coerced '(ExampleDecl TestExpectDecl)))))

  (define compile-time (current-seconds))

  (when logging-on?
    (logging:check-ex-spec:register-run compile-time assignment-name user path))

  (define module-datum `(module forge/check-ex-spec-mod forge/check-ex-spec/lang/expander
                          (require forge/check-ex-spec/library)
                          (require (prefix-in @ racket))

                          ; Auto-provide all defined values
                          (provide (except-out (all-defined-out)
                                               forge:n))
                          
                          ; Used for evaluator
                          (define-namespace-anchor forge:n)
                          (forge:nsa forge:n)

                          (require (prefix-in logging: forge/logging/logging)
                                   (prefix-in logging:check-ex-spec: forge/logging/check-ex-spec/main))

                          (logging:log-errors
                            (define wheat-results 
                              (list ,@(map (lambda (wheat) 
                                      `(with (,@provided #:from ,wheat)
                                         ,@safe-not-tests
                                         (@append ,@(for/list ([test just-tests])
                                                  test))))
                                     wheats)))

                            (define chaff-results 
                              (list ,@(map (lambda (chaff) 
                                      `(with (,@provided #:from ,chaff)
                                         ,@safe-not-tests
                                         (@append ,@(for/list ([test just-tests])
                                                  test))))
                                     chaffs)))

                            (logging:log-check-ex-spec wheat-results chaff-results)

                            (for ([wheat-result wheat-results]
                                  [num (in-naturals)])
                              (for ([test wheat-result])
                                (unless (test-report-passed? test)
                                  (raise (format "Failed wheat ~a with test '~a'." 
                                                 num (test-report-name test))))))

                            (for ([chaff-result chaff-results]
                                  [num (in-naturals)])
                              (define test-results
                                (for/list ([test chaff-result])
                                  (if (test-report-passed? test)
                                      #f
                                      (test-report-name test))))
                              (define catchers (filter (lambda (name) name) test-results))
                              (if (@> (length catchers) 0)
                                  (displayln (format "Caught chaff ~a with tests ~a."
                                                     num catchers))
                                  (displayln (format "Missed chaff ~a." 
                                                     num)))))

                          (logging:check-ex-spec:flush-logs ',compile-time wheat-results chaff-results)
                          (logging:flush-logs)

                          #;,ints-coerced))
  (datum->syntax #f module-datum))
(provide read-syntax)
