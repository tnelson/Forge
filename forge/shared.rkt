#lang racket/base

(provide get-verbosity set-verbosity VERBOSITY_LOW VERBOSITY_HIGH VERBOSITY_DEBUG)

; Level of output when running specs
(define VERBOSITY_LOW 1)
(define VERBOSITY_HIGH 5)
(define VERBOSITY_DEBUG 10)
(define verbosityoption VERBOSITY_LOW)
; for accessing verbosity in other modules
(define (get-verbosity) verbosityoption)
(define (set-verbosity x) (set! verbosityoption x))
