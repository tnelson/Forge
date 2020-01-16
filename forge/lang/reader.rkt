#lang racket

(require "ast.rkt" racket/runtime-path)

(require "../../forge2/forge2/tokenizer.rkt" "../../forge2/forge2/parser.rkt")

(require macro-debugger/syntax-browser)

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

; find all the sig declarations and lift their binding to the top level.
(define (is-sig-decl datum) (equal? (car datum) 'SigDecl)) 

(define (pull-sigs datum)
  (map (lambda (x) (string->symbol (car (cdr (second x)))))
       (begin
         #|(println (filter is-sig-decl datum))|# (filter is-sig-decl datum))))


(define (read-syntax path port)
  ; (define src-datum (port->list read port))
  (define parse-tree (parse path (make-tokenizer port)))

  
  (define src-datum (cdr (syntax->datum parse-tree)))
  ; (println src-datum)
  ; don't use format-datums, because it's awful with quotes.
  (define transformed (replace-ints src-datum))

  ;(println transformed)

  (define sig-inits (map (lambda (x) `(pre-declare-sig ,x)) (pull-sigs transformed)))

  ;(println sig-inits)

  (define final `(,@(append sig-inits transformed)))

  ;(println final)

  (define module-datum `(module kkcli ,forge-path
                          ,@final))
  
  (define stx (datum->syntax #f module-datum))
  ; (browse-syntax stx)
  stx)
(provide read-syntax)