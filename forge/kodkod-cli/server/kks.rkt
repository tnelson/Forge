#lang racket

(require (only-in racket/syntax format-symbol))

;(provide (except-out (all-defined-out) kodkod-port define-ops))

(require "../../shared.rkt")

(provide configure declare-ints print-cmd print-cmd-cont print-eof cmd declare-univ declare-rel read-solution solve v r tupleset (rename-out [-> product]))
(provide assert e f i define-const)
(provide read-evaluation)

(require "server.rkt"
         "server-common.rkt")
(define stdin-val #false)
(define stdout-val #false)
(provide start-server) ; stdin stdout)
(define (start-server)
  (when (>= (get-verbosity) VERBOSITY_HIGH)
    (displayln "Starting kodkod server."))
  (define kks (new server%
                   [initializer (thunk (kodkod-initializer #f))]
                   [stderr-handler (curry kodkod-stderr-handler "blank")]))
  (send kks initialize)
  (set! stdin-val (send kks stdin))
  (set! stdout-val (send kks stdout))
  (values stdin-val stdout-val))
; (define (stdin) stdin-val)
; (define (stdout) stdout-val)

; Prints all Kodkod commands issued during the dynamic
; extent of the given expressions to the provided port.
(define-syntax-rule (cmd [port] expr ...)
  (parameterize ([kodkod-port port])
    expr ...
    (flush-output port)))


; All command functions from this module (e.g., solve)
; write their Kodkod command to kodkod-port.
(define kodkod-port
  (make-parameter (current-output-port)
                  (lambda (port)
                    (unless (output-port? port)
                      (error 'kodkod-port "expected an output-port?, given ~a" port))
                    port)))

(define-syntax-rule (kodkod-display arg)
  (begin
    (when (>= (get-verbosity) VERBOSITY_HIGH)
      (display arg))
    (display arg [kodkod-port])))

; Prints the given command string to the kodkod-port.
(define-syntax-rule (print-cmd arg ...)
  (begin
    (kodkod-display (format arg ...))
    (kodkod-display #\newline)))

(define-syntax-rule (print-cmd-cont arg ...)
    (kodkod-display (format arg ...)))

(define (print-eof)
  (kodkod-display #\uFFFF))

; Commands
(define (configure . kvs)
  (print-cmd "(configure ~a)" (keyword-apply ~a '(#:separator) '(" ") kvs)))

(define (assert val)      (print-cmd "(assert ~a)" val))
(define (evaluate val)    (print-cmd "(evaluate ~a)" val))

(define (solve)
  (print-cmd "(solve)")
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

(define (declare id val)
  (print-cmd "(~a ~a)" id val))

; Identifiers
(define (r idx) (format-symbol "r~a" idx))  ; relational constant
(define (e idx) (format-symbol "e~a" idx))  ; relational expression
(define (f idx) (format-symbol "f~a" idx))  ; boolean expression
(define (i idx) (format-symbol "i~a" idx))  ; bitvector expression
(define (v idx) (format-symbol "v~a" idx))  ; bitvector expression

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
(define (read-solution port)
  (define result (read port))
  (when (>= (get-verbosity) VERBOSITY_LOW)
    (writeln result))
  (match result
    [(list (== 'sat) (== ':model) (list (list rid val) ...))
     ; We do this because our "pairs" are actually little lists, so we can't use make-hash directly.
    (cons 'sat (for/hash ([r rid][tuples val]) (values r tuples)))]
    [(list (== 'unsat) (== ':core) (list sid ...))
     (cons 'unsat sid)]
    [(list (== 'unsat))
     (cons 'unsat #f)]
    [(list (== 'no-more-instances))
     (cons 'no-more-instances #f)]
    [(== eof)
     (error "Kodkod CLI shut down unexpectedly while running!")]
    [other (error 'read-solution "Unrecognized solver output: ~a" other)]))

(define (read-evaluation port)
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
    [(== eof)
     (error "Kodkod CLI shut down unexpectedly while evaluating!")]))
