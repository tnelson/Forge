#lang racket/base

(require (only-in racket/port port->list))
(require (prefix-in racket: (only-in racket/base read-syntax)))

(define (read-syntax path port)
  ; Using "read" will not bring in syntax location info
  (define parse-tree (port->list (lambda (x) (racket:read-syntax path x)) port))

  ; The module-path here is racket/base so that we get 
  ; various top-level bindings like #%module-begin
  (define module-datum `(module forge-domains-crypto-module                                 
                                racket/base

                                (require forge/choose-lang-specific)
                                (require forge/lang/lang-specific-checks)                      
                                (set-checker-hash! forge-checker-hash)
                                (set-ast-checker-hash! forge-ast-checker-hash) 

                                (require forge/sigs
                                         forge/domains/crypto/expander)
                                ; We must provide all-defined-out here, because otherwise sig, pred, etc.
                                ; definitions expanded below wouldn't be available to the importing module.
                                (provide (except-out (all-defined-out) forge:n)
                                         (all-from-out forge/domains/crypto/expander))
                                
                                ; For evaluating
                                (define-namespace-anchor forge:n)
                                (forge:nsa forge:n)

                                ,@parse-tree
                                
                                ; Include any tests, run commands, etc.
                                (module+ execs)
                                (module+ main
                                (require (submod ".." execs)))))
  (datum->syntax #f module-datum))

(provide read-syntax)


