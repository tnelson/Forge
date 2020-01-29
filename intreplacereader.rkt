#lang racket

(define (node/int/constant x) 3)

; this assumes that there's nothing sneakier than lists going on in the datum.
; so no vectors, hashes, boxes, etc.
; doesn't replace ints in the run command.
(define (replace-ints datum)
  (cond
    [(list? datum)
     (if (equal? (car datum) 'run)
         datum
         (map replace-ints datum))]
    [(integer? datum)
     `,(node/int/constant datum)]
    [else datum]))
    

(define (read-syntax path port)
  (define src-datum (port->list read port))
  ;(displayln src-lines)
  ; don't use format-datums, because it's awful with quotes.
  (define transformed (replace-ints src-datum))

  (define module-datum `(module kkcli racket
                          ,@transformed))
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