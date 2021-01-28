#lang racket/base

(require racket/port)
(require (prefix-in @ (only-in racket/base read-syntax)))

(define (read-syntax path port)
  ; Using "read" will not bring in syntax location info
  (define parse-tree (port->list (lambda (x) (@read-syntax path x)) port))
  (define module-datum `(module forge-core-mod racket
                          (require forge/sigs)
                          (provide (except-out (all-defined-out)
                                               forge:n))
                          (define-namespace-anchor forge:n)
                          (forge:nsa forge:n)
                          ,@parse-tree))
  (datum->syntax #f module-datum))

(provide read-syntax)