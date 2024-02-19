#lang racket/base

(require syntax/parse)
(require (only-in forge/lang/reader coerce-ints-to-atoms))
(require forge/lang/alloy-syntax/parser)
(require forge/lang/alloy-syntax/tokenizer)
(require (prefix-in log: forge/logging/2023/main))
(require forge/shared)
(do-time "forge/bsl/lang/reader")

(define (read-syntax path port)
  (define this-lang 'forge/bsl)
  (define-values (logging-on? project email) (log:setup this-lang port path))
  (define compile-time (current-seconds))
  (when logging-on?
    (uncaught-exception-handler (log:error-handler logging-on? compile-time (uncaught-exception-handler)))
    (log:register-run compile-time project this-lang email path))
  (define parse-tree (parse path (make-tokenizer port)))
  (define ints-coerced (coerce-ints-to-atoms parse-tree))
  (define final `((provide (except-out (all-defined-out) ; So other programs can require it
                                       forge:n))

                  ;; Used for the evaluator
                  (define-namespace-anchor forge:n) 
                  (forge:nsa forge:n)

                  (require (prefix-in log: forge/logging/2023/main))
                  (require (only-in racket printf uncaught-exception-handler))

                  ;; Set up language-specific error messages
                  (require forge/choose-lang-specific)
                  (require forge/bsl/lang/bsl-lang-specific-checks)                   
                  (set-checker-hash! bsl-checker-hash)
                  (set-ast-checker-hash! bsl-ast-checker-hash)
                  (set-inst-checker-hash! bsl-inst-checker-hash)
                  (set-check-lang! 'bsl)

                  ;; Override default exception handler
                  (uncaught-exception-handler (log:error-handler ',logging-on? ',compile-time (uncaught-exception-handler)))
                  
                  ;; Expanded model, etc.
                  ,ints-coerced
                  
                  ;; Declare submodule "execs". Macros like "test" or "run" etc. will add to
                  ;; this submodule.
                  (module+ execs)
                  ;; Declare submodule "main"
                  (module+ main
                    ; Invoke the execs submodule
                    (require (submod ".." execs))
                    ; After execution of execs, print test failures (if any)
                    (output-all-test-failures))
                  (log:flush-logs ',compile-time "no-error")))

  (define module-datum `(module forge-mod forge/lang/expander
                          ,@final))
  ; (printf "Ints-coerced: ~a~n" ints-coerced)
  ; (raise "STOP")
  (define result (datum->syntax #f module-datum))
  ;(printf "debug result of expansion: ~a~n" result)
  result)
(provide read-syntax)
