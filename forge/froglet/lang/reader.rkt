#lang racket/base

(require syntax/parse)
(require (only-in forge/lang/reader generic-forge-reader))
(require forge/shared)
(do-time "forge/froglet/lang/reader")
(provide read-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-syntax path port)
  (generic-forge-reader
   path
   port
   'bsl  ;; TODO: needs to be 'bsl to match, should be robust to 'forge/froglet
   bsl-checker-hash
   bsl-ast-checker-hash
   bsl-inst-checker-hash
   '(forge/froglet/lang/bsl-lang-specific-checks)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

