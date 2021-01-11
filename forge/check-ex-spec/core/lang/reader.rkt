#lang racket/base

(require racket/port)
(require forge/check-ex-spec/library)
(require (only-in racket/function curry))

(define (filter-commands parse-tree keep)
  (filter (lambda (line) (member (car line) keep)) parse-tree))

(define (read-syntax path port)
  (define assignment-name (read port))
  (unless (string? assignment-name)
    (raise "Argument error: expected string after #lang forge/check-ex-spec; received ~a.~n" assignment-name))

  (define assignment-info (check-ex-spec:get-info assignment-name))
  (define wheats (map (curry format "~a.rkt" ) (hash-ref assignment-info 'wheats)))
  (define chaffs (map (curry format "~a.rkt" ) (hash-ref assignment-info 'chaffs)))
  (define provided (map string->symbol (hash-ref assignment-info 'provides)))

  (define parse-tree (port->list read port))

  (define just-tests (filter-commands parse-tree '(example test inst fun)))

  (define module-datum `(module forge/check-ex-spec/core-mod racket
                          (require forge/check-ex-spec/library)
                          (require (prefix-in @ racket))

                          ; Auto-provide all defined values
                          (provide (except-out (all-defined-out)
                                               forge:n))

                          ; Used for evaluator
                          (define-namespace-anchor forge:n)
                          (forge:nsa forge:n)

                          (define wheat-results 
                            (list ,@(map (lambda (wheat) 
                                    `(with (,@provided #:from ,wheat)
                                       (list ,@(for/list ([test just-tests])
                                                test))))
                                   wheats)))

                          (define chaff-results 
                            (list ,@(map (lambda (chaff) 
                                    `(with (,@provided #:from ,chaff)
                                       (list ,@(for/list ([test just-tests])
                                                test))))
                                   chaffs)))

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
                                                   num))))

                          #;,@parse-tree))
  (datum->syntax #f module-datum))

(provide read-syntax)