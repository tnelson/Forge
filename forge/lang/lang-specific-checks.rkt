#lang racket/base

(require forge/lang/ast)
(require forge/sigs-structs)
(require (for-syntax racket/syntax syntax/srcloc)
         syntax/srcloc)

(provide forge-checker-hash)
(provide forge-ast-checker-hash)
(provide forge-inst-checker-hash)
(define forge-checker-hash (make-hash))
(define forge-ast-checker-hash (make-hash))
(define forge-inst-checker-hash (make-hash))


; The base Forge language (Relational Forge) has no custom errors at the moment. For documentation,
; see forge/bsl/lang/lang-specific-checks.rkt. 

