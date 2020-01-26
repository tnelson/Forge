#lang racket

(require "lang/ast.rkt" "kodkod-cli/server/kks.rkt")

(provide interpret-formula)

(define (get-var-idx var quantvars)
  (- (length quantvars) (index-of quantvars var)))

; quantvars should start at -1
(define (interpret-formula formula relations quantvars)
  (match formula
    [(node/formula/constant type)
     ( print-cmd-cont (format "~a " type))]
    [(node/formula/op args)
     (interpret-formula-op formula relations quantvars args)]
    [(node/formula/multiplicity mult expr)
     ( print-cmd-cont (format "(~a " mult ))
     (interpret-expr expr relations quantvars)
     ( print-cmd-cont ")")]
    [(node/formula/quantified quantifier decls form)
     ;(writeln formula)
     (define var (car (car decls)))
     (let ([quantvars (cons var quantvars)])
       ( print-cmd-cont (format "(~a ([~a : one " quantifier (v (get-var-idx var quantvars))))
       (interpret-expr (cdr (car decls)) relations quantvars)
       ( print-cmd-cont "]) ")
       (interpret-formula form relations quantvars)
       ( print-cmd-cont ")"))]
    [#t ( print-cmd-cont "true ")]
    ))

(define (interpret-formula-op formula relations quantvars args)
  (match formula
    [(? node/formula/op/&&?)
     ( print-cmd-cont "(&& ")
     (map (lambda (x) (interpret-formula x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/formula/op/||?)
     ( print-cmd-cont "(|| ")
     (map (lambda (x) (interpret-formula x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/formula/op/=>?)
     ( print-cmd-cont "(=> ")
     (map (lambda (x) (interpret-formula x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/formula/op/in?)

     (print-cmd-cont "(in ")
     (map (lambda (x) (interpret-expr x relations quantvars)) args)
     (print-cmd-cont ")")]
    [(? node/formula/op/=?)
     (print-cmd-cont "(= ")
     (map (lambda (x) (interpret-expr x relations quantvars)) args)
     (print-cmd-cont ")")]

    [(? node/formula/op/!?)
     (print-cmd-cont "(! ")
     (map (lambda (x) (interpret-formula x relations quantvars)) args)
     (print-cmd-cont ")")]

    [(? node/formula/op/int>?)
     ( print-cmd-cont "(> ")
     (map (lambda (x) (interpret-int x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/formula/op/int<?)
     ( print-cmd-cont "(< ")
     (map (lambda (x) (interpret-int x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/formula/op/int=?)
     ( print-cmd-cont "(= ")
     (map (lambda (x) (interpret-int x relations quantvars)) args)
     ( print-cmd-cont ")")]))

(define (interpret-expr expr relations quantvars)
  (match expr
    [(node/expr/relation arity name typelist parent)
     ( print-cmd-cont (format "r~a " (index-of relations expr)))]
    [(node/expr/constant 1 'Int)
     ( print-cmd-cont "ints ")]
    [(node/expr/constant arity type)
     ( print-cmd-cont (format "~a " type))]
    [(node/expr/op arity args)
     (interpret-expr-op expr relations quantvars args)]
    [(node/expr/quantifier-var arity sym)
     (print-cmd-cont (symbol->string (v (get-var-idx expr quantvars))))
     (print-cmd-cont " ")]
    [(node/expr/comprehension len decls form)
     (define var (car (car decls)))
     (let ([quantvars (cons var quantvars)])
       ( print-cmd-cont (format "{([~a : " (v (get-var-idx var quantvars))))
       (interpret-expr (cdr (car decls)) relations quantvars)
       ( print-cmd-cont "]) ")
       (interpret-formula form relations quantvars)
       ( print-cmd-cont "}"))]))

(define (interpret-expr-op expr relations quantvars args)
  (match expr
    [(? node/expr/op/+?)
     ( print-cmd-cont "(+ ")
     (map (lambda (x) (interpret-expr x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/expr/op/-?)
     ( print-cmd-cont "(- ")
     (map (lambda (x) (interpret-expr x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/expr/op/&?)
     ( print-cmd-cont "(& ")
     (map (lambda (x) (interpret-expr x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/expr/op/->?)
     ( print-cmd-cont "(-> ")
     (map (lambda (x) (interpret-expr x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/expr/op/join?)
     ( print-cmd-cont "(. ")
     (map (lambda (x) (interpret-expr x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/expr/op/^?)
     ( print-cmd-cont "(^ ")
     (map (lambda (x) (interpret-expr x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/expr/op/*?)
     (print-cmd-cont "(* ")
     (map (lambda (x) (interpret-expr x relations quantvars)) args)
     (print-cmd-cont ")")]
    [(? node/expr/op/~?)
     (print-cmd-cont "(~a " '~)
     (map (lambda (x) (interpret-expr x relations quantvars)) args)
     (print-cmd-cont ")")]))

(define (interpret-int expr relations quantvars)
  (match expr
    [(node/int/constant value)
     ( print-cmd-cont (number->string value))]
    [(node/int/op args)
     (interpret-int-op expr relations quantvars args)]))

(define (interpret-int-op expr relations quantvars args)
  (match expr
    [(? node/int/op/add?)
     ( print-cmd-cont "(+ ")
     (map (lambda (x) (interpret-int x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/int/op/subtract?)
     ( print-cmd-cont "(- ")
     (map (lambda (x) (interpret-int x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/int/op/multiply?)
     ( print-cmd-cont "(* ")
     (map (lambda (x) (interpret-int x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/int/op/divide?)
     ( print-cmd-cont "(/ ")
     (map (lambda (x) (interpret-int x relations quantvars )) args)
     ( print-cmd-cont ")")]
    [(? node/int/op/sum?)
     ( print-cmd-cont "(sum ")
     (map (lambda (x) (interpret-expr x relations quantvars )) args)
     ( print-cmd-cont ")")]
    [(? node/int/op/card?)
     ( print-cmd-cont "(# ")
     (map (lambda (x) (interpret-expr x relations quantvars)) args)
     ( print-cmd-cont ")")]))
