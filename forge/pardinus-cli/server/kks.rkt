#lang racket

(require (only-in racket/syntax format-symbol))

;(provide (except-out (all-defined-out) kodkod-port define-ops))

(require "../../shared.rkt")

(provide configure declare-ints print-cmd print-cmd-cont print-eof cmd declare-univ
         declare-rel declare-target read-solution solve v r x tupleset (rename-out [-> product]))
(provide assert e f i a define-const)
(provide read-evaluation)
(provide clear)

(require "server.rkt"
         "server-common.rkt")

(define server-name "Pardinus")

(provide start-server) ; stdin stdout)
(define (start-server [solver-type 'stepper] [solver-subtype 'default])
  (when (>= (get-verbosity) VERBOSITY_HIGH)
    (printf "Starting ~a server.~n" server-name))
  (define kks (new server%
                   [name server-name]
                   [initializer (thunk (pardinus-initializer solver-type solver-subtype))]))
  (send kks initialize)
  (define stdin-val (send kks stdin))
  (define stderr-val (send kks stderr))
  (define stdout-val (send kks stdout))
  (define close-server (thunk (begin (printf "***DEBUG: shutting down~n") (send kks shutdown))))
  (define is-running? (thunk (send kks initialized?)))
  (values stdin-val stdout-val stderr-val close-server is-running?))

; Prints all Kodkod commands issued during the dynamic
; extent of the given expressions to the provided port.
(define-syntax-rule (cmd [port] expr ...)
  (parameterize ([pardinus-port port])
    expr ...
    (flush-output port)))


; All command functions from this module (e.g., solve)
; write their Kodkod command to kodkod-port.
(define pardinus-port
  (make-parameter (current-output-port)
                  (lambda (port)
                    (unless (output-port? port)
                      (error 'pardinus-port "expected an output-port?, given ~a" port))
                    port)))

(define-syntax-rule (pardinus-display arg)
  (begin
    (when (>= (get-verbosity) VERBOSITY_HIGH)
      (printf "pardinus-display: ~a~n" arg))
    (display arg [pardinus-port])))

; Prints the given command string to the kodkod-port.
(define-syntax-rule (print-cmd arg ...)
  (begin
    (pardinus-display (format arg ...))
    (pardinus-display #\newline)))

(define-syntax-rule (print-cmd-cont arg ...)
    (pardinus-display (format arg ...)))

(define (print-eof)
  (pardinus-display #\uFFFF))

; Commands
(define (configure . kvs)
  (print-cmd "(configure ~a)" (keyword-apply ~a '(#:separator) '(" ") kvs)))

(define (assert val)      (print-cmd "(assert ~a)" val))
(define (evaluate val)    (print-cmd "(evaluate ~a)" val))

; The solve-type parameter communicates an exploration mode for the next iteration to Pardinus
(define (solve [solve-type ""])
  (print-cmd (format "(solve ~a)" solve-type))
  (print-eof))
(define (clear)
  (print-cmd "(clear)")
  (print-eof))

;; Declarations and definitions
(define (define-const id val) (print-cmd "(~a ~a)" id val))
(define (declare-univ size)   (print-cmd "(univ ~a)" size))

(define (declare-ints ints idxs)
  (print-cmd-cont "(ints [")
  (for ([int ints][idx idxs])
    (print-cmd-cont "(~a ~a)" int idx))
  (print-cmd "])"))

(define declare-rel
  (case-lambda
    [(id lo hi) (print-cmd "(~a [~a :: ~a])" id lo hi)]
    [(id exact) (print-cmd "(~a [~a])" id exact)]))

(define (declare-target id target)
  (print-cmd "(target ~a [~a])" id target))

(define (declare id val)
  (print-cmd "(~a ~a)" id val))

; Identifiers
(define (r idx) (format-symbol "r:~a" idx))  ; relational constant
(define (x idx) (format-symbol "x:~a" idx))  ; time-variable relational constant (Pardinus)
(define (e idx) (format-symbol "e:~a" idx))  ; relational expression
(define (f idx) (format-symbol "f:~a" idx))  ; boolean expression
(define (i idx) (format-symbol "i:~a" idx))  ; bitvector expression
(define (v idx) (format-symbol "v:~a" idx))  ; bitvector expression
(define (a idx) (format-symbol "a:~a" idx))  ; atom expression

; Built-in constants
(define-values (TRUE FALSE UNIV NONE IDEN INTS)
  (apply values '(true false univ none iden ints)))

; Tuples
(define tuple list)
(define-syntax tupleset
  (syntax-rules ()
    [(_ #:range from to) (format "{~a ... ~a}" from to)]
    [(_ #:area  from to) (format "{~a # ~a}" from to)]
    [(_ #:tuples ts)     (format "{~a}" (keyword-apply ~a '(#:separator) '(" ") ts))]
    [(_ t ...)           (format "{~a}" (~a t ... #:separator " "))]))

; Operators
(define-syntax-rule (define-ops [id symbol] ...)
  (define-values (id ...)
    (values (lambda e `(symbol ,@e)) ...)))

(define-ops
  ; polymorphic
  [ite ite] [= =]

  ; boolean
  [not !] [and &&] [or \|\|] [<=> <=>] [=> =>]

  ; bitvector
  [set set] [bvabs abs] [bvsgn sgn] [bvneg -] [bvnot ~]
  [bvslt <] [bvsle <=] [bvand &] [bvor \|] [bvxor ^]
  [bvshl <<] [bvlshr >>>] [bvashr >>]
  [bvadd +] [bvsub -] [bvmul *] [bvsdiv /] [bvsrem %]

  ; relational
  [no no] [lone lone] [one one] [some some] [sum sum] [-> ->] [in in])

; Quantified constraints
(define (quant-constraint quant id mult expr constraint)
  (print-cmd (format "(~a ([~a : ~a ~a]) ~a)" quant id mult expr constraint)))

; Reads a solution to a Kodkod problem from specified
; input port and returns a hashtable or a list.
;
; If the problem  is satisfiable, the result is a
; hashtable mapping relation identifiers to sets of
; tuples.  A set of tuples is represented as a list of
; lists (of natural numbers), with no duplicates.
;
; If the problem is unsatisfiable, the result is a set
; of formula identifiers, represented as a list with no
; duplicates, which identify the formulas in the problem
; that form a minimal unsatisfiable core.  The produced
; list will be non-empty if the underlying solver was
; instructed to provide cores; it will be empty otherwise.
(define (read-solution port err-port)
  (define result (read port))
  (when (> (get-verbosity) VERBOSITY_LOW)
    (writeln result)) 
  (match result
    ;; SAT results. Engine MUST provide statistics and metadata elements
    ; expect a list of instances now (which may be singleton)
    [(list (== 'sat)
           (== ':model) (list (list (list rid val) ...) ...)
           (== ':stats) (list stat ...)
           (== ':metadata) (list md ...))
     (define data (for/list ([rs rid][tupless val])     ; create a list
                    (for/hash ([r rs] [tuples tupless]) ; of hashes
                      (values r tuples))))     
     (list 'sat data stat md)]     ; of rel->tuple-list entries
    
    ;; UNSAT results with and without core
    [(list (== 'unsat)
           (== ':core) (list sid ...)
           (== ':stats) (list stat ...))
     (list 'unsat sid stat)]
    [(list (== 'unsat)
           (== ':stats) (list stat ...))
     (list 'unsat #f stat)]

    ;; end of instance stream (empty data and empty statistics list)
    [(list (== 'no-more-instances))
     (list 'no-more-instances #f '())]
    [(== eof)
     (port-echo err-port (current-error-port) #:title server-name)
     (error (format "~a CLI shut down unexpectedly while running!" server-name))]
    [other (error 'read-solution "Unrecognized solver output: ~a" other)]))

(define (read-evaluation port err-port)
  (define result (read port))
  (when (>= (get-verbosity) VERBOSITY_LOW)
    (writeln result))
  (match result
    [(list (== 'evaluated) (== ':expression) atoms)
     (cons 'expression atoms)]
    [(list (== 'evaluated) (== ':int-expression) val)
     (cons 'int-expression val)]
    [(list (== 'evaluated) (== ':formula) val)
     (cons 'formula (equal? val 'true))]
    [(list (== 'unsat))
     (error "Current engine ran out of instances; evaluator is untrustworthy.")]
    [(== eof)
     (port-echo err-port (current-error-port) #:title server-name)
     (error (format "~a CLI shut down unexpectedly while evaluating!" server-name))]))
