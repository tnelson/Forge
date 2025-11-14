#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TEMPORAL FORGE READER
;   This is quite similar to the normal Forge reader, except it
;   injects a problem-type option annotation automatically.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require syntax/parse)
(require (only-in forge/lang/reader generic-forge-reader))
;(require forge/lang/alloy-syntax/parser)
;(require forge/lang/alloy-syntax/tokenizer)
(require forge/shared)
(do-time "forge/temporal/lang/reader")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-syntax path port)
  (generic-forge-reader
   path
   port
   'forge/temporal
   temporal-checker-hash
   temporal-ast-checker-hash
   temporal-inst-checker-hash
   '(forge/temporal/lang/temporal-lang-specific-checks)
   '((set-option! 'problem_type 'temporal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define (read-syntax path port)
;;   (define this-lang 'forge/temporal)
;;   (define-values (logging-on? project email) (log:setup this-lang port path))
;;   (define compile-time (current-seconds))
;;   (when logging-on?
;;     (uncaught-exception-handler (log:error-handler logging-on? compile-time (uncaught-exception-handler)))
;;     (log:register-run compile-time project this-lang email path))
;;   (define parse-tree (parse path (make-tokenizer port)))
;;   (define ints-coerced (coerce-ints-to-atoms parse-tree))
;;   (define final `((provide (except-out (all-defined-out) ; So other programs can require it
;;                                        forge:n))
;; 
;;                   (define-namespace-anchor forge:n) ; Used for evaluator
;;                   (forge:nsa forge:n)
;; 
;;                   (require (prefix-in log: forge/logging/2023/main))
;;                   (require (only-in racket printf uncaught-exception-handler))
;; 
;;                   (require forge/choose-lang-specific)
;;                   (require forge/temporal/lang/temporal-lang-specific-checks)
;;                   
;;                   (set-checker-hash! temporal-checker-hash)
;;                   (set-ast-checker-hash! temporal-ast-checker-hash)
;;                   (set-inst-checker-hash! temporal-inst-checker-hash)
;;                   (set-check-lang! 'temporal)                  
;; 
;;                   (uncaught-exception-handler (log:error-handler ',logging-on? ',compile-time (uncaught-exception-handler)))
;;                   ;; Override default exception handler
;; 
;;                   (set-option! 'problem_type 'temporal)
;;                   ;; Expanded model, etc.
;;                   ,ints-coerced
;; 
;;                   ; Declare submodule "execs". Macros like "test" or "run" etc. will add to 
;;                   ; this submodule. After execution of execs, print test failures (if any).
;;                   (module+ execs
;;                     ; All tests should have been added to `execs` prior to this point. 
;;                     (output-all-test-failures)
;;                     ; At this point, all commands should be defined. Open Sterling.
;;                     (start-sterling-menu forge:curr-state forge:nsa))
;; 
;;                   ;; Declare submodule "main"
;;                   (module+ main
;;                     ; Invoke the execs submodule
;;                     (require (submod ".." execs)))                                    
;;                   (log:flush-logs ',compile-time "no-error")))
;; 
;;   (define module-datum `(module forge-mod forge/lang/expander
;;                           ,@final))
;;   ; (printf "Ints-coerced: ~a~n" ints-coerced)
;;   ; (raise "STOP")
;;   (define result (datum->syntax #f module-datum))
;;   ;(printf "debug result of expansion: ~a~n" result)
;;   result)
(provide read-syntax)
