#lang racket/base

(require racket/port)
(require (prefix-in @ (only-in racket/base read-syntax)))
(require forge/shared)
(do-time "forge/core/lang/reader")

(define (read-syntax path port)
  (define this-lang 'forge/core)
  (define compile-time (current-seconds))
  
  ; Using "read" will not bring in syntax location info
  (define parse-tree (port->list (lambda (x) (@read-syntax path x)) port))

  ; The module-path here is racket/base rather than forge/sigs so that we get various top-level bindings like #%module-begin
  (define module-datum `(module forge-core-mod racket/base
                          (require forge/shared)
                          (do-time "forge-core-mod toplevel")
                          (require forge/choose-lang-specific)
                          (require forge/lang/lang-specific-checks)                                                     
                          (set-checker-hash! forge-checker-hash)
                          (set-ast-checker-hash! forge-ast-checker-hash)                          

                          (require (only-in racket first rest empty? empty)) ; these are used heavily
                          (require forge/sigs)
                          (do-time "forge-core-mod require")

                          (provide (except-out (all-defined-out)
                                               forge:n))

                          ; For evaluating
                          (define-namespace-anchor forge:n)
                          (forge:nsa forge:n)

                          ;; Override default exception handler
                          ,@parse-tree
                          (do-time "forge-core-mod parse-tree")

                          (module+ execs)
                          (module+ main
                            (require (submod ".." execs)))))

  (datum->syntax #f module-datum))

(provide read-syntax)


