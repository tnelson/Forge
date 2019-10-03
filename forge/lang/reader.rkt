#lang racket

(require "ast.rkt" racket/runtime-path)

(require "../../forge2/forge2/tokenizer.rkt" "../../forge2/forge2/parser.rkt")

(define-runtime-path forge-path "../forge.rkt")

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
  ; (define src-datum (port->list read port))
  (define parse-tree (parse path (make-tokenizer port)))
  (define src-datum (cdr (syntax->datum parse-tree)))

  ; don't use format-datums, because it's awful with quotes.
  (define transformed (replace-ints src-datum))

  (define module-datum `(module kkcli ,forge-path
                          ,@transformed))
  (datum->syntax #f module-datum))
(provide read-syntax)
    