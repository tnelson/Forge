#lang racket

(require racket/syntax (only-in racket [< racket/<] [- racket/-] [and racket/and]))

(provide (except-out (all-defined-out) printf-smt))

; Prints all smt commands to current-output-port.
; TODO: implement python repl compatability
(define-syntax-rule (printf-smt arg ...)
  (printf arg ...))



; TODO: these might be fake
(define-syntax (set-option stx)
  (syntax-case stx) ()
  [(_ opt val)
  #'(printf-smt "set-option(~a, ~a)\n" opt val)])

(define-syntax (check-sat stx)
  (syntax-case stx) ()
  [(_)
  #'(printf-smt "check-sat()\n")])

(define-syntax (get-model stx)
  (syntax-case stx) ()
  [(_)
  #'(printf-smt "get-model()\n")])

(define-syntax (get-unsat-core-option stx)
  (syntax-case stx) ()
  [(_)
  #'(printf-smt "get-unsat-core()\n")])

(define-syntax (get-info stx)
  (syntax-case stx) ()
  [(_ kw)
  #'(printf-smt "get-info(~a)\n" kw)])

(define-syntax (echo stx)
  (syntax-case stx) ()
  [(_ s)
  #'(printf-smt "echo(~a)\n" s)])

(define-syntax (reset stx)
  (syntax-case stx) ()
  [(_)
  #'(printf-smt "reset()\n")])

(define-syntax (push stx)
  (syntax-case stx) ()
  [(_)
  #'(printf-smt "push()\n")])

(define-syntax (pop stx)
  (syntax-case stx) ()
  [(_)
  #'(printf-smt "pop()\n")])

(define-syntax (exit stx)
  (syntax-case stx) ()
  [(_)
  #'(printf-smt "exit()\n")])

(define-syntax (assert stx)
  (syntax-case stx ()
    [(_ body)
     #'(printf-smt "assert(~a)\n"
                   body)]))

; Declarations and definitions

(define-syntax (declare-const stx)
  (syntax-case stx ()
    [(_ id type)
     #'(printf-smt "~a = ~a(~a))\n"
                'id
                'type
                'id)]))

(define-syntax (declare-fun stx)
  (syntax-case stx ()
    [(_ id domain ... range)
     #'(printf-smt "~a = Function('~a' ~a ~a)\n"
                'id
                'id
                '(domain ...)
                'range)]))

(define-syntax (define-const stx)
  (syntax-case stx ()
    [(_ id type body)
     #'(printf-smt "~a = ~a\n"
                'id
                body)]))


;;;;;TODO: define-fun should compile to a python function;;;;;
(define-syntax (define-fun stx)
  (syntax-case stx ()
    [(_ id ([arg type] ...) out body)
     #'(printf-smt "def ~a(~a):\n\t~a)\n"
                'id
                '(arg ...)
                body)]))



(define-syntax (forall stx)
  (syntax-case stx ()
    [(_ ([id type] ...) body)
     #'(printf-smt "forall(~a ~a)\n"
                   '((id type) ...)
                   body)]))

(define-syntax (exists stx)
  (syntax-case stx ()
    [(_ ([id type] ...) body)
     #'(printf-smt "exists(~a ~a)\n"
                   '((id type) ...)
                   body)]))

(define-syntax (+ stx)
  (syntax-case stx ()
    [(_ a b)
     #'(printf-smt "(~a + ~a)" a b)]))

(define-syntax (- stx)
  (syntax-case stx ()
    [(_ a b)
     #'(printf-smt "(~a - ~a)" a b)]))

(define-syntax (* stx)
  (syntax-case stx ()
    [(_ a b)
     #'(printf-smt "(~a * ~a)" a b)]))

(define-syntax (/ stx)
  (syntax-case stx ()
    [(_ a b)
     #'(printf-smt "(~a / ~a)" a b)]))

(define-syntax (= stx)
  (syntax-case stx ()
    [(_ a b)
     #'(printf-smt "(~a == ~a)" a b)]))

(define-syntax (and stx)
  (syntax-case stx ()
    [(_ a b)
     #'(printf-smt " (~a and ~a)" a b)]))

(define-syntax (or stx)
  (syntax-case stx ()
    [(_ a b)
     #'(printf-smt "(~a or ~a)" a b)]))

(define-syntax (=> stx)
  (syntax-case stx ()
    [(_ a b)
     #'(printf-smt "Implies(~a ~a)" a b)]))

(define-syntax (mod stx)
  (syntax-case stx ()
    [(_ a b)
     #'(printf-smt "(~a % ~a)" a b)]))

