#lang racket/base

(require racket/port)
(require (prefix-in logging: forge/logging/logging))

(define (read-syntax path port)
  (logging:flush-logs)
  (define parse-tree (logging:log-execution 'forge/core (port->list read port)))
  (define module-datum `(module forge-core-mod racket
                          (require forge/sigs)
                          (provide (except-out (all-defined-out)
                                               forge:n))

                          ; For evaluating
                          (define-namespace-anchor forge:n)
                          (forge:nsa forge:n)

                          ; For logging
                          (require (only-in forge/logging/logging 
                                            [flush-logs logging:flush-logs]
                                            [log-errors logging:log-errors]))

                          (logging:log-errors
                            (begin ,@parse-tree))
                          (logging:flush-logs)))
  (datum->syntax #f module-datum))

(provide read-syntax)