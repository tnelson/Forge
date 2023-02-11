#lang racket

(require forge/sigs-structs
         "lang/ast.rkt"
         "translate-to-kodkod-cli.rkt"
         "translate-from-kodkod-cli.rkt")
(require (prefix-in pardinus: "pardinus-cli/server/kks.rkt")
         (prefix-in pardinus: "pardinus-cli/server/server.rkt")
         (prefix-in pardinus: "pardinus-cli/server/server-common.rkt"))

(provide evaluate)

; TODO: instance isn't used
;  always evaluates with respect to solver's current state
(define (evaluate run instance expression)
  (unless (is-sat? run)
    (raise (format "Can't evaluate on unsat run. Expression: ~a" expression)))
  (define-values (expr-name interpretter)
    (cond [(node/expr? expression)
           (define currents (Run-kodkod-currents run))
           (define expression-number (Kodkod-current-expression currents)) 
           (set-Kodkod-current-expression! currents (add1 expression-number))
           (values (pardinus:e expression-number)
                   interpret-expr)]
          [(node/formula? expression)
           (define currents (Run-kodkod-currents run))
           (define formula-number (Kodkod-current-formula currents)) 
           (set-Kodkod-current-formula! currents (add1 formula-number))
           (values (pardinus:f formula-number)
                   interpret-formula)]
          [(node/int? expression)
           (define currents (Run-kodkod-currents run))
           (define int-number (Kodkod-current-int currents)) 
           (set-Kodkod-current-int! currents (add1 int-number))
           (values (pardinus:i int-number)
                   interpret-int)]
          [else
           (error (format "Forge: unexpected input type to evaluate: ~a" expression))]))

  (define all-rels (get-all-rels run))
  (define atom-names (Run-atoms run))

  (pardinus:cmd 
    [(get-stdin run)]
    ; which run are we asking about
    (pardinus:print-cmd "(with ~a" (Run-name run))
    ; which expression? define it.
    (pardinus:print-cmd-cont "(~a " expr-name)
    (interpretter run expression all-rels atom-names '())
    (pardinus:print-cmd ")")
    ; call the engine's evaluator on that expression
    (pardinus:print-cmd "(evaluate ~a)" expr-name)
    (pardinus:print-cmd ")")
    ; end with EOI
    (pardinus:print-eoi))

  (define run-atoms (Run-atoms run))
  (translate-evaluation-from-kodkod-cli 
    (pardinus:read-evaluation (get-stdout run) (get-stderr run)) 
    run-atoms))
