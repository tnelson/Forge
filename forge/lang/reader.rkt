#lang br/quicklang
(require "alloy-syntax/parser.rkt")
(require "alloy-syntax/tokenizer.rkt")

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  ; (println parse-tree)

  (define final `(,parse-tree))

  (define module-datum `(module forge-mod forge/lang/expander
                          (provide (all-defined-out))
                          ,@final))
  (datum->syntax #f module-datum))
(provide read-syntax)