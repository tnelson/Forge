#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TEMPORAL FORGE CUSTOM ERRORS AND CHECKS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require forge/lang/lang-specific-checks)
(provide (rename-out [forge-checker-hash temporal-checker-hash ]
                     [forge-ast-checker-hash temporal-ast-checker-hash]
                     [forge-inst-checker-hash temporal-inst-checker-hash]))
