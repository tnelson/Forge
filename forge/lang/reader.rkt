#lang racket

(require "ast.rkt" racket/runtime-path)

(require "alloy-syntax/tokenizer.rkt" "alloy-syntax/parser.rkt")
(require (prefix-in @ racket))
(require "../shared.rkt")

;(require macro-debugger/syntax-browser)

(define-runtime-path forge-path "../forge.rkt")

; this assumes that there's nothing sneakier than lists going on in the datum.
; so no vectors, hashes, boxes, etc.
; doesn't replace ints in the run command.
(define (replace-ints datum)
  (cond
    [(list? datum)
     (cond [(empty? datum)
            '()]
           [(equal? (car datum) 'run)
            datum]
           [else
            (map replace-ints datum)])]
    [(integer? datum)
     `,(node/int/constant datum)]
    [else datum]))

; find all the sig declarations and lift their binding to the top level.
(define (is-sig-decl datum) (equal? (car datum) 'SigDecl))

(define (get-sig-name datum) #|(println datum)|# (if (@and (pair? (first datum)) (equal? (first (first datum)) 'NameList)) (second (first datum)) (get-sig-name (rest datum))))

(define (pull-sigs datum)
  (map (lambda (x)
       (cons (get-sig-name x) #|(car (cdr (second x)))|# #|(if (@and (@> (length x) 2) (equal? (car (third x)) 'SigExt)) (string->symbol (second (third (third x)))) 'univ)|#
             (grab-extension x)))
       (begin
         #|(println (filter is-sig-decl datum))|# (filter is-sig-decl datum))))

(define (grab-extension datum)
  (if (equal? 0 (length datum))
      'univ
      (if (@and (pair? (first datum)) (equal? (first (first datum)) 'SigExt))
          (second (third (first datum)))
          (grab-extension (rest datum)))))

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))

  (define src-datum (cdr (syntax->datum parse-tree)))
  ; don't use format-datums, because it's awful with quotes.
  ;(define transformed (replace-ints src-datum))
  (define sig-inits (map (lambda (x) `(pre-declare-sig ,(car x) #:extends ,(cdr x))) (pull-sigs src-datum)))

  ; Insert the filename of the running file into itself, to be shown in visualizer later,
  ; and used to extract source text.
  (define filename-definition (list
                               `(set-path! ,(format "~a" path))
                               `(printf "Forge Version: ~a - File: ~a~n" ,forge-version ,path)))
  (define final `(,@filename-definition ,@sig-inits ,@(cdr (syntax->list parse-tree))))

  ;(map println final)

  (define module-datum `(module kkcli ,forge-path
                          ,@final))

  (define stx (datum->syntax #f module-datum))
  stx)
(provide read-syntax)
