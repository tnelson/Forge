#lang racket/base

(require racket/runtime-path racket/file)

(provide get-verbosity set-verbosity VERBOSITY_LOW VERBOSITY_HIGH VERBOSITY_DEBUG)
(provide forge-version)

; Level of output when running specs
(define VERBOSITY_SCRIPT 0) ; for test scripts
(define VERBOSITY_LOW 1)
(define VERBOSITY_HIGH 5)
(define VERBOSITY_DEBUG 10)
(define verbosityoption VERBOSITY_DEBUG)
; for accessing verbosity in other modules
(define (get-verbosity) verbosityoption)
(define (set-verbosity x) (set! verbosityoption x))


(define-runtime-path info-path "info.rkt")
(define forge-version "x.x.x")
(with-handlers ([exn:fail? (λ (exn) (println exn))])
  (define info-str (file->string info-path))
  (define parts (regexp-match #px"define\\s+version\\s+\"(\\S+)\"" info-str))
  (set! forge-version (cadr parts))
)
