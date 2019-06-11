#lang br/quicklang

#|(define (read-syntax path port)
  (define src-lines (port->lines port))
  (datum->syntax #f '(module lucy br
                       42)))
(provide read-syntax)|#

(require "smtlib2.rkt")

#|(define (read-syntax path port)
  (define src (port->string port))
  (datum->syntax #f (format-datum '(module module-name br "~a") src)))|#

(define out (open-output-file ~a #:exists 'replace))

(define new-source
  '(module
       module-name
     br
     (display ~a out)
     (close-output-port out)
     
     (struct proc (stdout stdin))

     (define (start-program p)
       (define-values (s stdout stdin stderr) (subprocess #f #f #f p ~a))
       (thread (lambda () (copy-port stderr (current-error-port))))
       (proc stdout stdin))

     (define (send-to proc v)
       (write v (proc-stdin proc))
       (flush-output (proc-stdin proc)))

     (define (receive-from proc)
       (read (proc-stdout proc)))

     (parameterize ([current-command-line-arguments (vector ~a)])
       (define programs
         (map find-executable-path (vector->list (current-command-line-arguments))))
       (define running-programs
         (map start-program programs))
       (let loop ([x (receive-from (first running-programs))])
         (displayln x)
         (unless (eof-object? x)
           (loop (receive-from (first running-programs))))))))

(define (read-syntax path port)
  (define src (port->string port))
  (datum->syntax #f (format-datum new-source "\"./z3-input.txt\"" (string-append "\"" src "\"") "\"./z3-input.txt\""  "\"python\"")))

(provide read-syntax)

(define-macro (smtlang-module-begin HANDLE-EXPR ...)
  #'(#%module-begin
     'HANDLE-EXPR ...))
(provide (rename-out [stacker-module-begin #%module-begin]))

(define-syntax-rule (printf-smt arg ...)
  (printf arg ...))

(define-syntax (set-option stx)
  (syntax-case stx) ()
  [(_ opt val)
  #'(printf-smt "(set-option ~a ~a)\n" opt val)])

(define-syntax (check-sat stx)
  (syntax-case stx) ()
  [(_)
  #'(printf-smt "(check-sat)\n")])

(define-syntax (get-model stx)
  (syntax-case stx) ()
  [(_)
  #'(printf-smt "(get-model)\n")])

(define-syntax (get-unsat-core-option stx)
  (syntax-case stx) ()
  [(_)
  #'(printf-smt "(get-unsat-core)\n")])

(define-syntax (get-info stx)
  (syntax-case stx) ()
  [(_ kw)
  #'(printf-smt "(get-info ~a)\n" kw)])

(define-syntax (echo stx)
  (syntax-case stx) ()
  [(_ s)
  #'(printf-smt "(echo ~a)\n" s)])

(define-syntax (reset stx)
  (syntax-case stx) ()
  [(_)
  #'(printf-smt "(reset)\n")])

(define-syntax (push stx)
  (syntax-case stx) ()
  [(_)
  #'(printf-smt "(push)\n")])

(define-syntax (pop stx)
  (syntax-case stx) ()
  [(_)
  #'(printf-smt "(pop)\n")])

(define-syntax (exit stx)
  (syntax-case stx) ()
  [(_)
  #'(printf-smt "(exit)\n")])

(define-syntax (assert stx)
  (syntax-case stx ()
    [(_ body)
     #'(printf-smt "(assert ~a)\n"
                   'body)]))