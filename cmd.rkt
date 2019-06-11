#lang racket
(struct proc (sp stdout stdin stderr))

(define (start-program p . args)
  (define-values (sp stdout stdin stderr) (apply subprocess (append (list #f #f #f p) args))) ; "/Users/luke/Desktop/z3pytest/tctest.py"))
  ;(thread (lambda () (copy-port stderr (current-error-port))))
  (proc sp stdout stdin stderr))

;its a file-stream port
;ITS A RACE CONDITION. Try changing the raw input line.
;problem is it wasn't flushing.

; ok so it works with cat, which is a repl.

(define (send-to proc v)
   (write v (proc-stdin proc))
   (flush-output (proc-stdin proc)))

(define (receive-from proc)
  (read (proc-stdout proc)))

;(define py (start-program "/usr/bin/python" "/Users/luke/Desktop/z3pytest/proctest.py"))

(define cat (start-program "/bin/cat"))
(define py (start-program "/usr/bin/python" "/Users/luke/Desktop/z3pytest/proctest.py"))


;(define out (open-output-file "/Users/luke/Desktop/midway.py" #:exists 'replace))
;(display "print(\"hello\")\n" out) 
;(close-output-port out)


;(displayln (receive-from py))
;(send-to py "print(\"interactive\")\n")

#|
(define cat (start-program "/bin/cat"))
> (send-to cat "thing")
> (displayln (receive-from cat))|#

#|(send-to py eof)
(send-to py eof)
(send-to py eof)|#
;(displayln (receive-from py))
;(displayln (receive-from py))

#|(parameterize ([current-command-line-arguments (vector "/usr/bin/python")])
  (define programs
    (map find-executable-path (vector->list (current-command-line-arguments))))
  (define running-programs
    (map start-program programs))
  (let loop ([x (receive-from (first running-programs))])
    (displayln x)
    (unless (eof-object? x)
      (loop (receive-from (first running-programs))))))|#