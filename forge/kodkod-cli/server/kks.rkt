#lang racket

(require (only-in racket/syntax format-symbol)
         "../../shared.rkt")

; Can keep kk-commands, right, because a lot of this stuff actually is specific to kodkod-cli.
; I guess the port commands aren't, really, but I'll worry about generalization when it matters.

(provide configure declare-ints declare-univ declare-rel
         kk-display kk-displaylns kk-print-eof cmd  read-solution
         solve v r tupleset
         (rename-out [-> product])
         assert f define-const
         print-cmd print-cmd-cont)

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

(define (kk-display arg ...)
  (begin
    (when (>= (get-verbosity) VERBOSITY_HIGH)
      (map display arg))
    (map (curryr display [kodkod-port]) display arg )))

(define (kk-displaylns arg ...)
  (begin
    (when (>= (get-verbosity) VERBOSITY_HIGH)
      (map displayln arg))
    (map (curryr displayln [kodkod-port]) display arg )))

(define print-cmd-cont kk-display)
(define print-cmd kk-displaylns)

(define (kk-print-eof)
  (kk-display #\uFFFF))

; Commands
; Ah need to fix this shit.
(define (configure . kvs)
  (kk-displaylns (format "(configure ~a)" (keyword-apply ~a '(#:separator) '(" ") kvs))))

(define (assert val)
  (kk-displaylns (format "(assert ~a)" val)))
(define (solve)
  (kk-displaylns "(solve)")
  (kk-print-eof))
(define (clear)
  (kk-displaylns "(clear)")
  (kk-print-eof))

;; Declarations and definitions
(define (define-const id val) (kk-displaylns (format "(~a ~a)" id val)))
(define (declare-univ size)   (kk-displaylns (format "(univ ~a)" size)))

(define (declare-ints ints idxs)
  (kk-display "(ints [")
  (for ([int ints][idx idxs])
    (kk-display (format "(~a ~a)" int idx)))
  (kk-displaylns "])"))

(define declare-rel
  (case-lambda
    [(id lo hi) (kk-display (format "(~a [~a :: ~a])" id lo hi))]
    [(id exact) (kk-display (format "(~a [~a])" id exact))]))

(define (declare id val)
  (kk-display (format "(~a ~a)" id val)))

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
     (error "Kodkod CLI shut down unexpectedly!")]
    [other (error 'read-solution "Unrecognized solver output: ~a" other)]))
