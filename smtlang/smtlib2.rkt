#lang racket

(require racket/syntax (only-in racket [< racket/<] [- racket/-] [and racket/and]))

(provide (except-out (all-defined-out) printf-smt))

; Prints all smt commands to current-output-port.
; TODO: implement
(define-syntax-rule (printf-smt arg ...)
  (printf arg ...))

(define-syntax (set-option stx)
  (syntax-case stx ()
  [(_ opt val)
  #'(printf-smt "(set-option ~a ~a)\n" opt val)]))

(define-syntax (check-sat stx)
  (syntax-case stx ()
  [(_)
  #'(printf-smt "(check-sat)\n")]))

(define-syntax (get-model stx)
  (syntax-case stx ()
  [(_)
  #'(printf-smt "(get-model)\n")]))

(define-syntax (get-unsat-core-option stx)
  (syntax-case stx ()
  [(_)
  #'(printf-smt "(get-unsat-core)\n")]))

(define-syntax (get-info stx)
  (syntax-case stx ()
  [(_ kw)
  #'(printf-smt "(get-info ~a)\n" kw)]))

(define-syntax (echo stx)
  (syntax-case stx ()
  [(_ s)
  #'(printf-smt "(echo ~a)\n" s)]))

(define-syntax (reset stx)
  (syntax-case stx ()
  [(_)
  #'(printf-smt "(reset)\n")]))

(define-syntax (push stx)
  (syntax-case stx ()
  [(_)
  #'(printf-smt "(push)\n")]))

(define-syntax (pop stx)
  (syntax-case stx ()
  [(_)
  #'(printf-smt "(pop)\n")]))

(define-syntax (exit stx)
  (syntax-case stx ()
  [(_)
  #'(printf-smt "(exit)\n")]))

(define-syntax (assert stx)
  (syntax-case stx ()
    [(_ body)
     #'(printf-smt "(assert ~a)\n"
                   body)]))

; Declarations and definitions

(define-syntax (declare-const stx)
  (syntax-case stx ()
    [(_ id type)
     #'(begin
         (define 'id (format "~a" 'id))
         (printf-smt "(declare-const ~a ~a)\n"
                'id
                'type))]))

(define-syntax (declare-fun stx)
  (syntax-case stx ()
    [(_ id domain ... range)
     #'(begin
         (define-syntax ('id stx)
           (syntax-case stx ()
             [(_ args ....)
              #'(printf-smt "(~a ~a)" 'id '(args ....))])
         (printf-smt "(declare-fun ~a ~a ~a)\n"
                'id
                '(domain ...)
                'range)))]))

(define-syntax (define-const stx)
  (syntax-case stx ()
    [(_ id type body)
     #'(printf-smt "(define-const ~a ~a ~a)\n"
                'id
                'type
                body)]))

(define-syntax (define-fun stx)
  (syntax-case stx ()
    [(_ id ([arg type] ...) out body)
     #'(printf-smt "(define-fun ~a ~a ~a ~a)\n"
                'id
                '((arg type) ...)
                'out
                body)]))

(define-syntax (forall stx)
  (syntax-case stx ()
    [(_ ([id type] ...) body)
     #'(printf-smt "(forall ~a ~a)\n"
                   '((id type) ...)
                   body)]))

(define-syntax (exists stx)
  (syntax-case stx ()
    [(_ ([id type] ...) body)
     #'(printf-smt "(exists ~a ~a)\n"
                   '((id type) ...)
                   body)]))

(define-syntax (+ stx)
  (syntax-case stx ()
    [(_ a b)
     #'(printf-smt "(+ ~a ~a)\n" a b)]))

(define-syntax (- stx)
  (syntax-case stx ()
    [(_ a b)
     #'(printf-smt "(- ~a ~a)\n" a b)]))

(define-syntax (* stx)
  (syntax-case stx ()
    [(_ a b)
     #'(printf-smt "(* ~a ~a)\n" a b)]))

(define-syntax (/ stx)
  (syntax-case stx ()
    [(_ a b)
     #'(printf-smt "(/ ~a ~a)\n" a b)]))

(define-syntax (= stx)
  (syntax-case stx ()
    [(_ a b)
     #'(printf-smt "(= ~a ~a)\n" a b)]))

(define-syntax (and stx)
  (syntax-case stx ()
    [(_ a b)
     #'(printf-smt "(and ~a ~a)\n" a b)]))

(define-syntax (or stx)
  (syntax-case stx ()
    [(_ a b)
     #'(printf-smt "(or ~a ~a)\n" a b)]))

(define-syntax (=> stx)
  (syntax-case stx ()
    [(_ a b)
     #'(printf-smt "(=> ~a ~a)\n" a b)]))

(define-syntax (mod stx)
  (syntax-case stx ()
    [(_ a b)
     #'(printf-smt "(mod ~a ~a)\n" a b)]))

