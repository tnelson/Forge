#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TEMPORAL FORGE READER
;   This is quite similar to the normal Forge reader, except it
;   injects a problem-type option annotation automatically.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require syntax/parse)
(require (only-in forge/lang/reader generic-forge-reader))
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

(provide read-syntax)
