#lang racket

(require "lang/ast.rkt" "../kodkod-cli/server/kks.rkt")

(provide interpret-formula)

;(define (kks-config )) TDOO

(define (interpret-formula formula universe relations)
  (match formula
    [(node/formula/constant type)
     (print-cmd (format "~a " type))]
    [(node/formula/op args)
     (interpret-formula-op formula universe relations args)]
    [(node/formula/multiplicity mult expr)
     (print-cmd (format "(~a " mult ))
     (interpret-expr expr universe relations)
     (print-cmd ")")]))

(define (interpret-formula-op formula universe relations args)
  (match formula
    [(? node/formula/op/&&?)
     (print-cmd "(&& ")
     (map (lambda (x) (interpret-formula x universe relations)) args)
     (print-cmd ")")]
    [(? node/formula/op/||?)
     (print-cmd "(|| ")
     (map (lambda (x) (interpret-formula x universe relations)) args)
     (print-cmd ")")]
    [(? node/formula/op/=>?)
     (print-cmd "(=> ")
     (map (lambda (x) (interpret-formula x universe relations)) args)
     (print-cmd ")")]))

(define (interpret-expr expr universe relations)
  (match expr
    [(node/expr/relation arity name)
     (print-cmd (format "r~a " (index-of relations expr)))]
    [(node/expr/constant arity type)
     (print-cmd (format "~a " type))]
    [(node/expr/op arity args)
     (interpret-expr-op expr universe relations args)]))

(define (interpret-expr-op expr universe relations args)
  (match expr
    [(? node/expr/op/+?)
     (print-cmd "(+ ")
     (map (lambda (x) (interpret-expr x universe relations)) args)
     (print-cmd ")")]
    [(? node/expr/op/-?)
     (print-cmd "(- ")
     (map (lambda (x) (interpret-expr x universe relations)) args)
     (print-cmd ")")]
    [(? node/expr/op/&?)
     (print-cmd "(& ")
     (map (lambda (x) (interpret-expr x universe relations)) args)
     (print-cmd ")")]
    [(? node/expr/op/->?)
     (print-cmd "(-> ")
     (map (lambda (x) (interpret-expr x universe relations)) args)
     (print-cmd ")")]
    [(? node/expr/op/join?)
     (print-cmd "(. ")
     (map (lambda (x) (interpret-expr x universe relations)) args)
     (print-cmd ")")]
    [(? node/expr/op/^?)
     (print-cmd "(^ ")
     (map (lambda (x) (interpret-expr x universe relations)) args)
     (print-cmd ")")]
    [(? node/expr/op/*?)
     (print-cmd "(* ")
     (map (lambda (x) (interpret-expr x universe relations)) args)
     (print-cmd ")")]))
