#lang racket/base

; This file is intended to take in the Forge AST and return an identity translation of it.

(require 
  forge/sigs-structs
  forge/lang/ast
  forge/shared
  (only-in racket index-of match string-join first second rest)
  (only-in racket/contract define/contract or/c listof any/c)
  (prefix-in @ (only-in racket/contract ->)))

(provide substitute-formula)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Boolean formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Translate a formula AST node
; target - the node to be replaced
; value - the node to replace the target with
; TODO: can't use straight up equals comparison, because of the info field.
; or define a custom = function that only checks... arity and name?
(define/contract (substitute-formula run-or-state formula relations atom-names quantvars target value)  
  (@-> (or/c Run? State? Run-spec?)
      node/formula?
      list?
      list?
      list?
      node? 
      node? 
      node?)
  (begin (printf "Interpreting formula: ~a\n" formula)
  (match formula
    [(node/formula/constant info type)
     (if (equal? formula target) value formula)]    
    [(node/fmla/pred-spacer info name args expanded)
     (substitute-formula run-or-state expanded relations atom-names quantvars target value)]
    [(node/formula/op info args)
     (substitute-formula-op run-or-state formula relations atom-names quantvars args target value)]
    [(node/formula/multiplicity info mult expr)
    (let ([processed-expr (substitute-expr run-or-state expr relations atom-names quantvars target value)])
     (node/formula/multiplicity info mult processed-expr))]
    [(node/formula/quantified info quantifier decls form)
    ; how do we want to handle shadowing?
    ; lucy reyes throws an error if a qv shadows target
    ; and she also throws an error if a qv shadows an existing quantvar
    (define new-vs-and-decls
       (for/fold ([vs-and-decls (list quantvars '())])
                 ([decl decls])
         (define curr-quantvars (first vs-and-decls))
         (define curr-decls (second vs-and-decls))
         (define new-quantvars (cons (car decl) quantvars))
         (define new-decl-domain (substitute-expr run-or-state (cdr decl) relations atom-names new-quantvars target value))
         (define new-decls (cons (cons (car decl) new-decl-domain) curr-decls))
         (list new-quantvars new-decls)))
     (define new-quantvars (first new-vs-and-decls))
     (let ([processed-form (substitute-formula run-or-state form relations atom-names new-quantvars target value)])
       (define new-decls (second new-vs-and-decls))
       (node/formula/quantified info quantifier new-decls processed-form))]
    [(node/formula/sealed info)
     (node/formula/sealed info)]
    [#t "true"]
    [#f "false"]
    )))

(define (process-children-formula run-or-state children relations atom-names quantvars target value)
  (map (lambda (x) (substitute-formula run-or-state x relations atom-names quantvars target value)) children))

(define (process-children-expr run-or-state children relations atom-names quantvars target value)
  (map (lambda (x) (substitute-expr run-or-state x relations atom-names quantvars target value)) children))

(define (process-children-int run-or-state children relations atom-names quantvars target value)
  (map (lambda (x) (substitute-int run-or-state x relations atom-names quantvars target value)) children))

(define (substitute-formula-op run-or-state formula relations atom-names quantvars args target value)
  (begin (printf "Interpreting formula op: ~a\n" formula)
  (match formula
    [(node/formula/op/&& info children)
      (node/formula/op/&& info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(node/formula/op/|| info children)
     (node/formula/op/|| info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(node/formula/op/=> info children)
     (node/formula/op/=> info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(node/formula/op/always info children)
     (node/formula/op/always info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(node/formula/op/eventually info children)
     (node/formula/op/eventually info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(node/formula/op/next_state info children)
      (node/formula/op/next_state info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(node/formula/op/releases info children)
      (node/formula/op/releases info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(node/formula/op/until info children)
     (node/formula/op/until info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(node/formula/op/historically info children)
      (node/formula/op/historically info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(node/formula/op/once info children)
      (node/formula/op/once info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(node/formula/op/prev_state info children)
      (node/formula/op/prev_state info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(node/formula/op/since info children)
      (node/formula/op/since info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(node/formula/op/triggered info children)
      (node/formula/op/triggered info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(node/formula/op/in info children)
      (node/formula/op/in info (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(node/formula/op/= info children)
      (node/formula/op/= info (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(node/formula/op/! info children)
      (node/formula/op/! info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(node/formula/op/int> info children)
      (node/formula/op/int> info (process-children-int run-or-state args relations atom-names quantvars target value))]
    [(node/formula/op/int< info children)
      (node/formula/op/int< info (process-children-int run-or-state args relations atom-names quantvars target value))]
    [(node/formula/op/int= info children)
     (node/formula/op/int= info (process-children-int run-or-state args relations atom-names quantvars target value))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Relational expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (substitute-expr run-or-state expr relations atom-names quantvars target value)
  (begin (printf "Interpreting expr: ~a\n" expr)
  (match expr
    [(node/expr/relation info arity name typelist-thunk parent isvar)
     (if (equal? expr target) value expr)]
    [(node/expr/atom info arity name)
     (if (equal? expr target) value expr)]
    [(node/expr/fun-spacer info arity name args result expanded)
     (substitute-expr run-or-state expanded relations atom-names quantvars target value)]
    [(node/expr/ite info arity a b c)  
    (let ([processed-a (substitute-formula run-or-state a relations atom-names quantvars target value)]
          [processed-b (substitute-expr run-or-state b relations atom-names quantvars target value)]
          [processed-c (substitute-expr run-or-state c relations atom-names quantvars target value)])
     (node/expr/ite info arity processed-a processed-b processed-c))]
    [(node/expr/constant info 1 'Int)
     (if (equal? expr target) value expr)]
    [(node/expr/constant info arity type)
     (if (equal? expr target) value expr)]
    [(node/expr/op info arity args)
     (substitute-expr-op run-or-state expr relations atom-names quantvars args target value)]
    [(node/expr/quantifier-var info arity sym name)  
     (if (equal? expr target) value expr)]
    [(node/expr/comprehension info len decls form)     
     (define new-vs-and-decls
       (for/fold ([vs-and-decls (list quantvars '())])
                 ([decl decls])
         (define curr-quantvars (first vs-and-decls))
         (define curr-decls (second vs-and-decls))
         (define new-quantvars (cons (car decl) quantvars))
         (define new-decl-domain (substitute-expr run-or-state (cdr decl) relations atom-names new-quantvars target value))
         (define new-decls (cons (cons (car decl) new-decl-domain) curr-decls))
         (list new-quantvars new-decls)))
     (define new-quantvars (first new-vs-and-decls))
     (let ([processed-form (substitute-formula run-or-state form relations atom-names new-quantvars target value)])
       (define new-decls (second new-vs-and-decls))
     (node/expr/comprehension info len new-decls processed-form))])))

(define (substitute-expr-op run-or-state expr relations atom-names quantvars args target value)
  (begin (printf "Interpreting expr op: ~a\n" expr)
  (match expr
    [(node/expr/op/+ info arity children)
     (node/expr/op/+ info arity (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(node/expr/op/- info arity children)
     (node/expr/op/- info arity (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(node/expr/op/& info arity children)
     (node/expr/op/& info arity (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(node/expr/op/-> info arity children)
     (node/expr/op/-> info arity (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(node/expr/op/prime info arity children)
     (node/expr/op/prime info arity (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(node/expr/op/join info arity children)
     (node/expr/op/join info arity (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(node/expr/op/^ info arity children)
     (node/expr/op/^ info arity (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(node/expr/op/* info arity children)
    (node/expr/op/* info arity (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(node/expr/op/~ info arity children)
     (node/expr/op/~ info arity (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(node/expr/op/++ info arity children)
     (node/expr/op/++ info arity (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(node/expr/op/sing info arity children)
     (node/expr/op/sing info arity (process-children-int run-or-state args relations atom-names quantvars target value))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Integer expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (substitute-int run-or-state expr relations atom-names quantvars target value)
(begin (printf "Interpreting int: ~a\n" expr)
  (match expr
    [(node/int/constant info value)
     (if (equal? expr target) value expr)]
    [(node/int/op info args)
     (substitute-int-op run-or-state expr relations atom-names quantvars args target value)]
    [(node/int/sum-quant info decls int-expr)
     (define new-vs-and-decls
       (for/fold ([vs-and-decls (list quantvars '())])
                 ([decl decls])
         (define curr-quantvars (first vs-and-decls))
         (define curr-decls (second vs-and-decls))
         (define new-quantvars (cons (car decl) quantvars))
         (define new-decl-domain (substitute-expr run-or-state (cdr decl) relations atom-names new-quantvars target value))
         (define new-decls (cons (cons (car decl) new-decl-domain) curr-decls))
         (list new-quantvars new-decls)))
     (define new-quantvars (first new-vs-and-decls))
     (let ([processed-int (substitute-int run-or-state int-expr relations atom-names new-quantvars target value)])
       (define new-decls (second new-vs-and-decls))
      (node/int/sum-quant info new-decls processed-int))])))

(define (substitute-int-op run-or-state expr relations atom-names quantvars args target value)
(begin (printf "Interpreting int-op: ~a\n" expr)
  (match expr
    [(node/int/op/add info children)
      (node/int/op/add info (process-children-int run-or-state args relations atom-names quantvars target value))]
    [(node/int/op/subtract info children)
    (node/int/op/subtract info (process-children-int run-or-state args relations atom-names quantvars target value))]
    [(node/int/op/multiply info children)
    (node/int/op/multiply info (process-children-int run-or-state args relations atom-names quantvars target value))]
    [(node/int/op/divide info children)
    (node/int/op/divide info (process-children-int run-or-state args relations atom-names quantvars target value))]
    [(node/int/op/sum info children)
    (node/int/op/sum info (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(node/int/op/card info children)
    (node/int/op/card info (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(node/int/op/remainder info children)
     (node/int/op/remainder info (process-children-int run-or-state args relations atom-names quantvars target value))]
    [(node/int/op/abs info children)
     (node/int/op/abs info (process-children-int run-or-state args relations atom-names quantvars target value))]
    [(node/int/op/sign info children)
     (node/int/op/sign info (process-children-int run-or-state args relations atom-names quantvars target value))]
    [(node/int/sum-quant info decls int-expr)
     (raise-forge-error #:msg "Reached expected unreachable code." #:context expr)]
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;