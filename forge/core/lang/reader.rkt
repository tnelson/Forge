#lang racket/base

(require racket/port)
(require (prefix-in log: forge/logging/2023/main))
(require (prefix-in @ (only-in racket/base read-syntax)))

(define (read-syntax path port)
  (define this-lang 'forge/core)
  (define-values (logging-on? project email) (log:setup this-lang port path))
  (define compile-time (current-seconds))
  (when logging-on?
    (uncaught-exception-handler (log:error-handler logging-on? compile-time (uncaught-exception-handler)))
    (log:register-run compile-time project this-lang email path))

  ; Using "read" will not bring in syntax location info
  (define parse-tree (port->list (lambda (x) (@read-syntax path x)) port))

  ; The module-path here is racket/base rather than forge/sigs so that we get various top-level bindings like #%module-begin
  (define module-datum `(module forge-core-mod racket/base
                          (require forge/choose-lang-specific)
                          (require forge/lang/lang-specific-checks)                                                     
                          (set-checker-hash! forge-checker-hash)
                          (set-ast-checker-hash! forge-ast-checker-hash)                          

                          (require (prefix-in log: forge/logging/2023/main))
                          (require (only-in racket first rest empty? empty)) ; these are used heavily
                          (require forge/sigs)

                          (provide (except-out (all-defined-out)
                                               forge:n))

                          ; For evaluating
                          (define-namespace-anchor forge:n)
                          (forge:nsa forge:n)

                          (uncaught-exception-handler (log:error-handler ',logging-on? ',compile-time (uncaught-exception-handler)))
                          ;; Override default exception handler
                          ,@parse-tree

                          (module+ execs)
                          (module+ main
                            (require (submod ".." execs)))
                          (log:flush-logs ',compile-time "no-error")))

  (datum->syntax #f module-datum))

(provide read-syntax)


