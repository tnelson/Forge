#lang racket/base

; This file is intended to take in the Forge AST and return an identity translation of it.

(require 
  forge/sigs-structs
  forge/lang/ast
  forge/shared
  (only-in racket index-of match string-join first second rest)
  (only-in racket/contract define/contract or/c listof any/c)
  (prefix-in @ (only-in racket/contract ->))
  (prefix-in @ (only-in racket/base >=)))

(provide interpret-formula)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Boolean formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Translate a formula AST node
(define/contract (interpret-formula run-or-state formula relations atom-names quantvars)  
  (@-> (or/c Run? State? Run-spec?)
      node/formula?
      list?
      list?
      list?
      node?)
  (when (@>= (get-verbosity) 2)
    (printf "identity: interpret-formula: ~a~n" formula))
  (match formula
    [(node/formula/constant info type)
     (node/formula/constant info type)]    
    [(node/fmla/pred-spacer info name args expanded)
     (interpret-formula run-or-state expanded relations atom-names quantvars)]
    [(node/formula/op info args)
     (interpret-formula-op run-or-state formula relations atom-names quantvars args)]
    [(node/formula/multiplicity info mult expr)
    (let ([processed-expr (interpret-expr run-or-state expr relations atom-names quantvars)])
     (node/formula/multiplicity info mult processed-expr))]
    [(node/formula/quantified info quantifier decls form)
     (define new-vs-and-decls
       (for/fold ([vs-and-decls (list quantvars '())])
                 ([decl decls])
         (define curr-quantvars (first vs-and-decls))
         (define curr-decls (second vs-and-decls))
         (define new-quantvars (cons (car decl) quantvars))
         (define new-decl-domain (interpret-expr run-or-state (cdr decl) relations atom-names new-quantvars))
         (define new-decls (cons (cons (car decl) new-decl-domain) curr-decls))
         (list new-quantvars new-decls)))
     (define new-quantvars (first new-vs-and-decls))
     (let ([processed-form (interpret-formula run-or-state form relations atom-names new-quantvars)])
       (define new-decls (second new-vs-and-decls))
       (node/formula/quantified info quantifier new-decls processed-form))]
    [(node/formula/sealed info)
     (node/formula/sealed info)]
    [#t "true"]
    [#f "false"]
    ))

(define (process-children-formula run-or-state children relations atom-names quantvars)
  (map (lambda (x) (interpret-formula run-or-state x relations atom-names quantvars)) children))

(define (process-children-expr run-or-state children relations atom-names quantvars)
  (map (lambda (x) (interpret-expr run-or-state x relations atom-names quantvars)) children))

(define (process-children-int run-or-state children relations atom-names quantvars)
  (map (lambda (x) (interpret-int run-or-state x relations atom-names quantvars)) children))

(define (interpret-formula-op run-or-state formula relations atom-names quantvars args)
  (when (@>= (get-verbosity) 2)
    (printf "identity: interpret-formula-op: ~a~n" formula))
  (match formula
    [(node/formula/op-on-formulas/&& info children)
      (node/formula/op-on-formulas/&& info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op-on-formulas/|| info children)
     (node/formula/op-on-formulas/|| info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op-on-formulas/=> info children)
     (node/formula/op-on-formulas/=> info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op-on-formulas/always info children)
     (node/formula/op-on-formulas/always info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op-on-formulas/eventually info children)
     (node/formula/op-on-formulas/eventually info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op-on-formulas/next_state info children)
      (node/formula/op-on-formulas/next_state info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op-on-formulas/releases info children)
      (node/formula/op-on-formulas/releases info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op-on-formulas/until info children)
     (node/formula/op-on-formulas/until info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op-on-formulas/historically info children)
      (node/formula/op-on-formulas/historically info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op-on-formulas/once info children)
      (node/formula/op-on-formulas/once info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op-on-formulas/prev_state info children)
      (node/formula/op-on-formulas/prev_state info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op-on-formulas/since info children)
      (node/formula/op-on-formulas/since info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op-on-formulas/triggered info children)
      (node/formula/op-on-formulas/triggered info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op-on-exprs/in info children)
      (node/formula/op-on-exprs/in info (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/formula/op-on-exprs/= info children)
      (node/formula/op-on-exprs/= info (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/formula/op-on-formulas/! info children)
      (node/formula/op-on-formulas/! info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op-on-ints/int> info children)
      (node/formula/op-on-ints/int> info (process-children-int run-or-state args relations atom-names quantvars))]
    [(node/formula/op-on-ints/int< info children)
      (node/formula/op-on-ints/int< info (process-children-int run-or-state args relations atom-names quantvars))]
    [(node/formula/op-on-ints/int= info children)
     (node/formula/op-on-ints/int= info (process-children-int run-or-state args relations atom-names quantvars))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Relational expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interpret-expr run-or-state expr relations atom-names quantvars)
  (when (@>= (get-verbosity) 2)
      (printf "identity: interpret-expr: ~a~n" expr))
  (match expr
    [(node/expr/relation info arity name typelist-thunk parent isvar)
     (node/expr/relation info arity name typelist-thunk parent isvar)]
    [(node/expr/atom info arity name)
     (node/expr/atom info arity name)]
    [(node/expr/fun-spacer info arity name args result expanded)
     (interpret-expr run-or-state expanded relations atom-names quantvars)]
    [(node/expr/ite info arity a b c)  
    (let ([processed-a (interpret-formula run-or-state a relations atom-names quantvars)]
          [processed-b (interpret-expr run-or-state b relations atom-names quantvars)]
          [processed-c (interpret-expr run-or-state c relations atom-names quantvars)])
     (node/expr/ite info arity processed-a processed-b processed-c))]
    [(node/expr/constant info 1 'Int)
     (node/expr/constant info 1 'Int)]
    [(node/expr/constant info arity type)
     (node/expr/constant info arity type)]
    [(node/expr/op info arity args)
     (interpret-expr-op run-or-state expr relations atom-names quantvars args)]
    [(node/expr/quantifier-var info arity sym name)  
     (node/expr/quantifier-var info arity sym name)]
    [(node/expr/comprehension info len decls form)   
     (define new-vs-and-decls
       (for/fold ([vs-and-decls (list quantvars '())])
                 ([decl decls])
         (define curr-quantvars (first vs-and-decls))
         (define curr-decls (second vs-and-decls))
         (define new-quantvars (cons (car decl) curr-quantvars))
         (define new-decl-domain (interpret-expr run-or-state (cdr decl) relations atom-names new-quantvars))
         (define new-decls (cons (cons (car decl) new-decl-domain) curr-decls))
         (list new-quantvars new-decls)))
     (define new-quantvars (first new-vs-and-decls))
     (let ([processed-form (interpret-formula run-or-state form relations atom-names new-quantvars)])
       (define new-decls (second new-vs-and-decls))
     (node/expr/comprehension info len new-decls processed-form))]))

(define (interpret-expr-op run-or-state expr relations atom-names quantvars args)
    (when (@>= (get-verbosity) 2)
      (printf "identity: interpret-expr-op: ~a~n" expr))
  (match expr
    [(node/expr/op-on-exprs/+ info arity children)
     (node/expr/op-on-exprs/+ info arity (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/expr/op-on-exprs/- info arity children)
     (node/expr/op-on-exprs/- info arity (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/expr/op-on-exprs/& info arity children)
     (node/expr/op-on-exprs/& info arity (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/expr/op-on-exprs/-> info arity children)
     (node/expr/op-on-exprs/-> info arity (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/expr/op-on-exprs/prime info arity children)
     (node/expr/op-on-exprs/prime info arity (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/expr/op-on-exprs/join info arity children)
     (node/expr/op-on-exprs/join info arity (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/expr/op-on-exprs/^ info arity children)
     (node/expr/op-on-exprs/^ info arity (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/expr/op-on-exprs/* info arity children)
    (node/expr/op-on-exprs/* info arity (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/expr/op-on-exprs/~ info arity children)
     (node/expr/op-on-exprs/~ info arity (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/expr/op-on-exprs/++ info arity children)
     (node/expr/op-on-exprs/++ info arity (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/expr/op-on-ints/sing info arity children)
     (node/expr/op-on-ints/sing info arity (process-children-int run-or-state args relations atom-names quantvars))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Integer expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interpret-int run-or-state expr relations atom-names quantvars)
  (when (@>= (get-verbosity) 2)
    (printf "identity: interpret-int: ~a~n" expr))
  (match expr
    [(node/int/constant info value)
     (node/int/constant info value)]
    [(node/int/op info args)
     (interpret-int-op run-or-state expr relations atom-names quantvars args)]
    [(node/int/sum-quant info decls int-expr)
     (define new-vs-and-decls
       (for/fold ([vs-and-decls (list quantvars '())])
                 ([decl decls])
         (define curr-quantvars (first vs-and-decls))
         (define curr-decls (second vs-and-decls))
         (define new-quantvars (cons (car decl) quantvars))
         (define new-decl-domain (interpret-expr run-or-state (cdr decl) relations atom-names new-quantvars))
         (define new-decls (cons (cons (car decl) new-decl-domain) curr-decls))
         (list new-quantvars new-decls)))
     (define new-quantvars (first new-vs-and-decls))
     (let ([processed-int (interpret-int run-or-state int-expr relations atom-names new-quantvars)])
       (define new-decls (second new-vs-and-decls))
      (node/int/sum-quant info new-decls processed-int))]))

(define (interpret-int-op run-or-state expr relations atom-names quantvars args)
  (when (@>= (get-verbosity) 2)
    (printf "identity: interpret-int-op: ~a~n" expr))
  (match expr
    [(node/int/op-on-ints/add info children)
      (node/int/op-on-ints/add info (process-children-int run-or-state args relations atom-names quantvars))]
    [(node/int/op-on-ints/subtract info children)
    (node/int/op-on-ints/subtract info (process-children-int run-or-state args relations atom-names quantvars))]
    [(node/int/op-on-ints/multiply info children)
    (node/int/op-on-ints/multiply info (process-children-int run-or-state args relations atom-names quantvars))]
    [(node/int/op-on-ints/divide info children)
    (node/int/op-on-ints/divide info (process-children-int run-or-state args relations atom-names quantvars))]
    [(node/int/op-on-exprs/sum info children)
    (node/int/op-on-exprs/sum info (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/int/op-on-exprs/card info children)
    (node/int/op-on-exprs/card info (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/int/op-on-ints/remainder info children)
     (node/int/op-on-ints/remainder info (process-children-int run-or-state args relations atom-names quantvars))]
    [(node/int/op-on-ints/abs info children)
     (node/int/op-on-ints/abs info (process-children-int run-or-state args relations atom-names quantvars))]
    [(node/int/op-on-ints/sign info children)
     (node/int/op-on-ints/sign info (process-children-int run-or-state args relations atom-names quantvars))]
    [(node/int/sum-quant info decls int-expr)
     (raise-forge-error #:msg "Reached expected unreachable code." #:context expr)]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;