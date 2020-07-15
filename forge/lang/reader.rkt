#lang br/quicklang
(require "alloy-syntax/parser.rkt")
(require "alloy-syntax/tokenizer.rkt")

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))

  (define final `(,parse-tree))

  (define module-datum `(module forge-mod forge/lang/expander
                          (provide (all-defined-out))
                          (define-namespace-anchor n)
                          (nsa n)
                          ,@final))
  (datum->syntax #f module-datum))
(provide read-syntax)