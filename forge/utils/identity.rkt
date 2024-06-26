#lang racket/base

; This file is intended to take in the Forge AST and return an identity translation of it.

(require 
  forge/sigs-structs
  forge/lang/ast
  forge/shared
  (only-in racket index-of match string-join first second rest)
  (only-in racket/contract define/contract or/c listof any/c)
  (prefix-in @ (only-in racket/contract ->)))

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
  (begin (printf "Interpreting formula: ~a\n" formula)
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
    )))

(define (process-children-formula run-or-state children relations atom-names quantvars)
  (map (lambda (x) (interpret-formula run-or-state x relations atom-names quantvars)) children))

(define (process-children-expr run-or-state children relations atom-names quantvars)
  (map (lambda (x) (interpret-expr run-or-state x relations atom-names quantvars)) children))

(define (process-children-int run-or-state children relations atom-names quantvars)
  (map (lambda (x) (interpret-int run-or-state x relations atom-names quantvars)) children))

(define (interpret-formula-op run-or-state formula relations atom-names quantvars args)
  (begin (printf "Interpreting formula op: ~a\n" formula)
  (match formula
    [(node/formula/op/&& info children)
      (node/formula/op/&& info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op/|| info children)
     (node/formula/op/|| info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op/=> info children)
     (node/formula/op/=> info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op/always info children)
     (node/formula/op/always info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op/eventually info children)
     (node/formula/op/eventually info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op/next_state info children)
      (node/formula/op/next_state info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op/releases info children)
      (node/formula/op/releases info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op/until info children)
     (node/formula/op/until info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op/historically info children)
      (node/formula/op/historically info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op/once info children)
      (node/formula/op/once info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op/prev_state info children)
      (node/formula/op/prev_state info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op/since info children)
      (node/formula/op/since info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op/triggered info children)
      (node/formula/op/triggered info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op/in info children)
      (node/formula/op/in info (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/formula/op/= info children)
      (node/formula/op/= info (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/formula/op/! info children)
      (node/formula/op/! info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op/int> info children)
      (node/formula/op/int> info (process-children-int run-or-state args relations atom-names quantvars))]
    [(node/formula/op/int< info children)
      (node/formula/op/int< info (process-children-int run-or-state args relations atom-names quantvars))]
    [(node/formula/op/int= info children)
     (node/formula/op/int= info (process-children-int run-or-state args relations atom-names quantvars))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Relational expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interpret-expr run-or-state expr relations atom-names quantvars)
  (begin (printf "Interpreting expr: ~a\n" expr)
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
         (define new-quantvars (cons (car decl) quantvars))
         (define new-decl-domain (interpret-expr run-or-state (cdr decl) relations atom-names new-quantvars))
         (define new-decls (cons (cons (car decl) new-decl-domain) curr-decls))
         (list new-quantvars new-decls)))
     (define new-quantvars (first new-vs-and-decls))
     (let ([processed-form (interpret-formula run-or-state form relations atom-names new-quantvars)])
       (define new-decls (second new-vs-and-decls))
     (node/expr/comprehension info len new-decls processed-form))])))

(define (interpret-expr-op run-or-state expr relations atom-names quantvars args)
  (begin (printf "Interpreting expr op: ~a\n" expr)
  (match expr
    [(node/expr/op/+ info arity children)
     (node/expr/op/+ info arity (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/expr/op/- info arity children)
     (node/expr/op/- info arity (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/expr/op/& info arity children)
     (node/expr/op/& info arity (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/expr/op/-> info arity children)
     (node/expr/op/-> info arity (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/expr/op/prime info arity children)
     (node/expr/op/prime info arity (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/expr/op/join info arity children)
     (node/expr/op/join info arity (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/expr/op/^ info arity children)
     (node/expr/op/^ info arity (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/expr/op/* info arity children)
    (node/expr/op/* info arity (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/expr/op/~ info arity children)
     (node/expr/op/~ info arity (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/expr/op/++ info arity children)
     (node/expr/op/++ info arity (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/expr/op/sing info arity children)
     (node/expr/op/sing info arity (process-children-int run-or-state args relations atom-names quantvars))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Integer expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interpret-int run-or-state expr relations atom-names quantvars)
(begin (printf "Interpreting int: ~a\n" expr)
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
      (node/int/sum-quant info new-decls processed-int))])))

(define (interpret-int-op run-or-state expr relations atom-names quantvars args)
(begin (printf "Interpreting int-op: ~a\n" expr)
  (match expr
    [(node/int/op/add info children)
      (node/int/op/add info (process-children-int run-or-state args relations atom-names quantvars))]
    [(node/int/op/subtract info children)
    (node/int/op/subtract info (process-children-int run-or-state args relations atom-names quantvars))]
    [(node/int/op/multiply info children)
    (node/int/op/multiply info (process-children-int run-or-state args relations atom-names quantvars))]
    [(node/int/op/divide info children)
    (node/int/op/divide info (process-children-int run-or-state args relations atom-names quantvars))]
    [(node/int/op/sum info children)
    (node/int/op/sum info (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/int/op/card info children)
    (node/int/op/card info (process-children-expr run-or-state args relations atom-names quantvars))]
    [(node/int/op/remainder info children)
     (node/int/op/remainder info (process-children-int run-or-state args relations atom-names quantvars))]
    [(node/int/op/abs info children)
     (node/int/op/abs info (process-children-int run-or-state args relations atom-names quantvars))]
    [(node/int/op/sign info children)
     (node/int/op/sign info (process-children-int run-or-state args relations atom-names quantvars))]
    [(node/int/sum-quant info decls int-expr)
     (raise-forge-error #:msg "Reached expected unreachable code." #:context expr)]
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;