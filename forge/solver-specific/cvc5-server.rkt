#lang racket/base

; Racket-side server to spawn cvc5 worker process for SMT-LIB backend.
; Based on the Pardinus/Kodkod "CLI" server. Opens `cvc5` (assuming it is
; on the user's path) in interactive mode and remains open until closed.

(require racket/runtime-path
         racket/class
         forge/solver-specific/server-common
         forge/shared)
(require (for-syntax racket/base))
(require (only-in racket curry add-between thunk))

(provide smtlib-initializer server% start-server)

(define server-name "SMT-LIB (CVC5)")

(define (smtlib-initializer solver-type solver-subtype)
  (unless (member solver-type '(incremental stepper))
    (raise (format  "Invalid solver type: ~a" solver-type)))

  ; Rather than finding the location of Java and the corresponding JAR, we just
  ; assume that cvc5 is on the user's path. 
  (let* ([windows? (equal? (system-type) 'windows)]
         [path-separator (if windows? ";" ":")]
         [error-out (build-path (find-system-path 'home-dir) "forge-cvc5-error-output.txt")])
    
    (when (> (get-verbosity) VERBOSITY_LOW)        
      (printf "  Starting solver process for ~a. subtype: ~a~n" server-name solver-subtype))

    ; Supports only the default subtype at the moment. 
    (define solver-subtype-str (cond [(equal? solver-subtype 'default) ""]
                                     [else (error (format "Bad solver subtype: ~a" solver-subtype))]))
    ; Find the cvc5 executable on the path
    (define windows? (equal? (system-type) 'windows))
    (define cvc5 (find-executable-path (if windows? "cvc5.exe" "cvc5")))

    ; Omitting:
    ;  "--finite-model-find": use finite model finding heuristic for quantifier instantiation
    ;  we should not have any quantifiers remaining, save perhaps Int quantifiers.

    ; Options like these are now added in the SMT-lib output, for better debugging from file.
    ; "--force-logic=ALL" "--finite-model-find" "--nl-cov" "--produce-models" "--sets-ext"
    (define cmdline-options
      (list "--incremental" "--interactive"))
    (printf "system type: ~a cvc5 path: ~a~ncommand-line options: ~a~n" (system-type) cvc5 cmdline-options)
    ; --sets-ext to use "extended set" operators, which help reconcile sets/relations
    (apply subprocess #f #f #f cvc5 cmdline-options)))


(define (start-server [solver-type 'stepper] [solver-subtype 'default])
  (when (>= (get-verbosity) VERBOSITY_HIGH)
    (printf "Starting ~a server.~n" server-name))
  (define smts (new server%
                    [name server-name]
                    [initializer (thunk (smtlib-initializer solver-type solver-subtype))]))
  (send smts initialize)
  (define stdin-val (send smts stdin))
  (define stderr-val (send smts stderr))
  (define stdout-val (send smts stdout))
  (define close-server (thunk 
    (begin 
      (when (>= (get-verbosity) VERBOSITY_HIGH)
        (printf "Shutting down ~a solver process...~n" server-name))
      (send smts shutdown))))
  (define is-running? (thunk (send smts initialized?)))
  (values stdin-val stdout-val stderr-val close-server is-running?))