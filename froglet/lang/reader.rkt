#lang racket/base

(provide read-syntax)

(require
  (only-in froglet/util lang-name)
  (only-in froglet/typecheck/main typecheck)
  (only-in forge/lang/reader coerce-ints-to-atoms)
  (only-in forge/lang/alloy-syntax/main parse make-tokenizer)
  (prefix-in log: forge/logging/2022/main))

(define (read-syntax path port)
  (define-values (logging-on? project email) (log:setup lang-name port path))
  (define compile-time (current-seconds))
  (when logging-on?
    (uncaught-exception-handler (log:error-handler logging-on? compile-time (uncaught-exception-handler)))
    (log:register-run compile-time project lang-name email path))
  (define alloymod
    (let* ((mod (parse path (make-tokenizer port)))
           (mod (coerce-ints-to-atoms mod))
           (mod (typecheck mod)))
      mod))
  (define module-datum
    `(module forge-mod forge/lang/expander
       (provide (except-out (all-defined-out) ; So other programs can require it
                            forge:n))

       (define-namespace-anchor forge:n) ; Used for evaluator
       (forge:nsa forge:n)

       (require (prefix-in log: forge/logging/2022/main))
       (require (only-in racket printf uncaught-exception-handler))

       (require forge/choose-lang-specific)
       ;; 2023-01-18: TODO eventually stop using lang-specific-checks, make them all type errors
       (require forge/bsl/lang/bsl-lang-specific-checks)
       (set-checker-hash! bsl-checker-hash)
       (set-ast-checker-hash! bsl-ast-checker-hash)
       (set-inst-checker-hash! bsl-inst-checker-hash)
       (set-check-lang! ',lang-name)

       (uncaught-exception-handler (log:error-handler ',logging-on? ',compile-time (uncaught-exception-handler)))
       ;; Override default exception handler

       ,alloymod

       (module+ execs)
       (module+ main
         (require (submod ".." execs))
         (log:flush-logs ',compile-time "no-error"))))
  (define result (datum->syntax #f module-datum))
  result)

