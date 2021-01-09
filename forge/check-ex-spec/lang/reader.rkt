#lang racket/base

(require forge/lang/alloy-syntax/parser)
(require forge/lang/alloy-syntax/tokenizer)
(require (only-in forge/lang/reader
                  coerce-ints-to-atoms))
(require forge/check-ex-spec/library)
(require racket/function)
(require racket/match)
; (require racket/list)

(define (filter-commands stx keep)
  (match-define (cons alloy-module commands) (syntax->list stx))
  (define filtered (filter (lambda (line) (member (car (syntax->datum line)) keep))
                           commands))
  (datum->syntax stx (cons alloy-module filtered) stx stx stx))

(define (read-syntax path port)
  (define assignment-name (read port))
  (unless (string? assignment-name)
    (raise (format "Argument error: expected string after #lang forge/check-ex-spec; received ~a.~n" assignment-name)))

  (define assignment-info (check-ex-spec:get-info assignment-name))
  (define wheats (map (curry format "~a.rkt" ) (hash-ref assignment-info 'wheats)))
  (define chaffs (map (curry format "~a.rkt" ) (hash-ref assignment-info 'chaffs)))
  (define provided (map string->symbol (hash-ref assignment-info 'provides)))

  (define parse-tree (parse path (make-tokenizer port)))
  (define ints-coerced (coerce-ints-to-atoms parse-tree))

  (define just-tests (cdr (syntax->list (filter-commands ints-coerced '(ExampleDecl InstDecl TestExpectDecl OptionDecl)))))

  (define module-datum `(module forge/check-ex-spec-mod forge/check-ex-spec/lang/expander
                          (require forge/sigs)

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

                          #;,ints-coerced))
  (datum->syntax #f module-datum))
(provide read-syntax)