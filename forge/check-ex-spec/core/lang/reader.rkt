#lang racket/base

(require racket/port)
(require forge/check-ex-spec/library)
(require racket/function)

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
                          (require forge/sigs)
                          (require forge/check-ex-spec/library)

                          ; Auto-provide all defined values
                          (provide (except-out (all-defined-out)
                                               forge:n))

                          ; Used for evaluator
                          (define-namespace-anchor forge:n)
                          (forge:nsa forge:n)

                          ; Enable check-ex-spec commands and load TA solution
                          (define wheat-results
                            (list
                              ,@(map (lambda (wheat) 
                                      `(with (,@provided #:from ,wheat)
                                         (list ,@(for/list ([test just-tests])
                                            `(with-handlers ([(lambda (exn) #t) (lambda (exn) #f)])
                                              ,test
                                             #t))))) wheats)))

                          (define chaff-results
                            (list
                              ,@(map (lambda (chaff) 
                                      `(with (,@provided #:from ,chaff)
                                         (list ,@(for/list ([test just-tests])
                                            `(with-handlers ([(lambda (exn) #t) (lambda (exn) #f)])
                                              ,test
                                             #t))))) chaffs)))

                          (printf "Wheat results: ~a~n" wheat-results)
                          (printf "Chaff results: ~a~n" chaff-results)

                          #;,@parse-tree))
  (datum->syntax #f module-datum))

(provide read-syntax)