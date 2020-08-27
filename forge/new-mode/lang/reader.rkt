#lang racket/base

(require forge/lang/alloy-syntax/parser)
(require forge/lang/alloy-syntax/tokenizer)
(require (only-in forge/lang/reader
                  coerce-ints-to-atoms))

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (define ints-coerced (coerce-ints-to-atoms parse-tree))

  (define module-datum `(module forge/new-mode-mod forge/new-mode/lang/expander
                          (require forge/sigs)

                          ; Auto-provide all defined values
                          (provide (except-out (all-defined-out)
                                               forge:n))

                          ; Used for evaluator
                          (define-namespace-anchor forge:n)
                          (forge:nsa forge:n)

                          ; Enable new-mode commands
                          ; Only necessary if you run custom stuff here
                          ; (require forge/new-mode/library)

                          ,ints-coerced))
  (datum->syntax #f module-datum))
(provide read-syntax)