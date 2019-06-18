#lang br



(struct proc (stdout stdin))

(define (start-program p)
  (define-values (s stdout stdin stderr) (subprocess #f #f #f p "./test.txt"))
  (thread (lambda () (copy-port stderr (current-error-port))))
  (proc stdout stdin))

(define (send-to proc v)
   (write v (proc-stdin proc))
   (flush-output (proc-stdin proc)))

(define (receive-from proc)
  (read (proc-stdout proc)))

(parameterize ([current-command-line-arguments (vector "./z3/build/z3")])
  (define programs
    (map find-executable-path (vector->list (current-command-line-arguments))))
  (define running-programs
    (map start-program programs))
  (let loop ([x (receive-from (first running-programs))])
    (displayln x)
    (unless (eof-object? x)
      (loop (receive-from (first running-programs))))))