#lang racket

(define (formatem lines)
  (define thing (filter (lambda (x) (not (equal? x "")))
                        lines))
  ;(displayln thing)
  (map (lambda (x) `(cmd [stdin] (print-cmd ,x)))
       thing))

(define (read-syntax path port)
  (define src-lines (port->lines port))
  ;(displayln src-lines)
  ; don't use format-datums, because it's awful with quotes.
  (define src-datums0 (formatem src-lines))
  (define src-datums (append '((require "server.rkt" "kks.rkt" "server-common.rkt")
                               (define kks (new server% 
                                                [initializer (thunk (kodkod-initializer #f))]
                                                [stderr-handler (curry kodkod-stderr-handler "blank")]))
                               (send kks initialize)
                               (define stdin (send kks stdin))
                               (define stdout (send kks stdout)))

                             src-datums0

                             '((cmd [stdin] (print-eof))
                               (write (read-solution stdout)))))
  ;(writeln src-datums)

  (define module-datum `(module kkcli racket
                          ,@src-datums))
  (datum->syntax #f module-datum))
(provide read-syntax)

#|


(define (read-syntax path port)
  (define src-lines (port->lines port))
  (define src-datums (format-datums '(cmd [stdin] (print-cmd ~a)) src-lines))
  (define module-datum `(module stacker-mod racket
                          ,@src-datums))
  (datum->syntax #f module-datum))
(provide read-syntax)|#