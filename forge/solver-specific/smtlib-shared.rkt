#lang racket/base

(require forge/shared)
(provide smtlib-display)

; For exporting to other modules, if they have the proper port (from State struct)
(define (smtlib-display port msg)
  (when (>= (get-verbosity) VERBOSITY_HIGH)
    (printf "smtlib-display: ~a~n" msg))
  ; Send this SMT-LIB expression to the worker process' stdin
  (display msg port)
  ; interactive mode: hit enter to begin processing
  (display "\n" port)
  (flush-output port))
