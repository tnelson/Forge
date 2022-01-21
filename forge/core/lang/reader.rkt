#lang racket/base

(require racket/port)
(require (prefix-in log: forge/logging/2022/main))
(require (prefix-in @ (only-in racket/base read-syntax)))

(define (read-syntax path port)
  (define this-lang 'forge/core)
  (define-values (logging-on? project email) (log:setup this-lang port path))

  ; Using "read" will not bring in syntax location info
  (define parse-tree (port->list (lambda (x) (@read-syntax path x)) port))
  (define compile-time (current-seconds))
  (when logging-on?
    (log:register-run compile-time project this-lang email path))
  (define module-datum `(module forge-core-mod racket
                          (require forge/choose-lang-specific)
                          (require forge/lang/lang-specific-checks) ; TODO: can this be relative?
                          ; ANSWER: maybe using dynamic-require
                          ;(printf "ast-ch = ~a~n" (get-ast-checker-hash))
                          (set-checker-hash! forge-checker-hash)
                          (set-ast-checker-hash! forge-ast-checker-hash)
                          ;(printf "ast-ch = ~a~n" (get-ast-checker-hash))

                          (require (prefix-in log: forge/logging/2022/main))
                          (require forge/sigs-functional)

                          (provide (except-out (all-defined-out)
                                               forge:n))

                          ; For evaluating
                          (define-namespace-anchor forge:n)
                          (forge:nsa forge:n)

                          (set-option! 'eval-language 'core)
                          (parameterize ([uncaught-exception-handler (log:error-handler ',logging-on? ',compile-time (uncaught-exception-handler))])
                            ,@parse-tree)
                          
                          (module+ execs)
                          (module+ main
                            (require (submod ".." execs))
                            (log:flush-logs ',compile-time "no-error"))))

  (datum->syntax #f module-datum))

(provide read-syntax)


