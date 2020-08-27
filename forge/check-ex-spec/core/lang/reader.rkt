#lang racket/base

(require racket/port)
(require racket/match)

(define (read-syntax path port)
  (define parse-tree (port->list read port))
  (match-define (cons assignment contents) parse-tree)
  (unless (string? assignment)
    (raise "Argument error: expected string after #lang forge/check-ex-spec; received ~a.~n" assignment))


  (define module-datum `(module forge-core/check-ex-spec-mod racket
                          (require forge/sigs)
                          (provide (except-out (all-defined-out)
                                               forge:n)
                          (define-namespace-anchor forge:n)
                          (forge:nsa forge:n)

                          (require (prefix-in check-ex-spec: forge/check-ex-spec/library))
                          (check-ex-spec:load-assignment ,assignment)
                          
                          ,@contents))
  (datum->syntax #f module-datum))

(provide read-syntax)