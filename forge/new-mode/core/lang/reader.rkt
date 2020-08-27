#lang racket/base

(require racket/port)

(define (read-syntax path port)
  (define parse-tree (port->list read port))

  (define module-datum `(module forge-core/new-mode/core-mod racket
                          (require forge/sigs)

                          ; Auto-provide all defined values
                          (provide (except-out (all-defined-out)
                                               forge:n))

                          ; Used for evaluator
                          (define-namespace-anchor forge:n)
                          (forge:nsa forge:n)

                          ; Enable new-mode commands
                          (require forge/new-mode/library)
                          
                          ,@parse-tree))
  (datum->syntax #f module-datum))

(provide read-syntax)