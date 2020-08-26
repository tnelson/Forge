#lang racket/base

(require forge/lang/alloy-syntax/parser)
(require forge/lang/alloy-syntax/tokenizer)
(require (only-in forge/lang/reader
                  coerce-ints-to-atoms))

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (define ints-coerced (coerce-ints-to-atoms parse-tree))

  (define final `((provide (except-out (all-defined-out) ; So other programs can require it
                                       forge:n))

                  (define-namespace-anchor forge:n) ; Used for evaluator
                  (forge:nsa forge:n)

                  ,ints-coerced))

  (define module-datum `(module forge-mod forge/lang/expander
                          ,@final))
  (datum->syntax #f module-datum))
(provide read-syntax)

