#lang br/quicklang

(require "tokenizer.rkt" "parser.rkt")

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  ; parse-tree
  (define module-datum `(module forge2-module forge2/expander ,parse-tree))
  ; (println module-datum)
  (datum->syntax #f module-datum)
  )
(provide read-syntax)
