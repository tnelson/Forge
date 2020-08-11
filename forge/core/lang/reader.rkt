#lang racket/base

(require racket/port)

(define (read-syntax path port)
  (define parse-tree (port->list read port))
  (define module-datum `(module forge-core-mod racket
                          (require forge/sigs)
                          (provide (except-out (all-defined-out)
                                               forge:n))
                          (define-namespace-anchor forge:n)
                          (forge:nsa forge:n)
                          ,@parse-tree))
  (datum->syntax #f module-datum))

(provide read-syntax)