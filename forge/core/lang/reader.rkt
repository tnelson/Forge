#lang br/quicklang

(require racket/runtime-path)
(define-runtime-path forge-path "../forge-core.rkt")

(define (read-syntax path port)
  (define parse-tree (port->list read port))
  (define module-datum `(module bf-mod ,forge-path
                          (provide (all-defined-out))
                          ,@parse-tree))
  (datum->syntax #f module-datum))

(provide read-syntax)