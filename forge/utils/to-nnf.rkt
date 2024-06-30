#lang racket/base

; This file is intended to take in the Forge AST and return an NNF translation of it.
; All negations have been pushed down maximally

; TODO: Note that we do not yet descend into formulas with set comprehensions.

; TODO: use verbosity checker to determine whether to print debug statements.
;  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
;    (printf "to-nnf: interpret-___: ~a~n" arg))

(require 
  forge/sigs-structs
  forge/lang/ast
  forge/shared
  (only-in racket index-of match string-join first second rest)
  (only-in racket/contract define/contract or/c listof any/c)
  (prefix-in @ (only-in racket/contract ->))
  (prefix-in @ (only-in racket/base >=)) )

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
  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "to-nnf: interpret-formula: ~a~n" formula))
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
     ; We want to make sure to we apply the replacement to the quantified variable's domain.
     ; (In NNF, this is only important if it involves set comprehension, but in general we
     ;  might be doing, e.g., substitution! So it's vital that this get substituted.)
     ; But if a quantifier declares multiple variables, each preceding variable is in scope
     ; for the domain of its successor! So we must fold over these, updating the environment...
     (define new-vs-and-decls
       (for/fold ([vs-and-decls (list quantvars '())])
                 ([decl decls])
         (define curr-quantvars (first vs-and-decls))
         (define curr-decls (second vs-and-decls))
         (define new-quantvars (cons (car decl) quantvars))
         (define new-decl-domain (interpret-expr run-or-state (cdr decl) relations atom-names new-quantvars))
         (define new-decls (cons (cons (car decl) new-decl-domain) curr-decls))
         (list new-quantvars new-decls)))
     ; We now have the full updated quantified variables list and the updated decls with new domains.
     (define new-quantvars (first new-vs-and-decls))
     (let ([processed-form (interpret-formula run-or-state form relations atom-names new-quantvars)])
       (define new-decls (second new-vs-and-decls))
       (node/formula/quantified info quantifier new-decls processed-form))]
    [(node/formula/sealed info)
     (node/formula/sealed info)]
     ; TODO: Need to convert to node/formula/constant bool (maybe just use the true / false keyword constructors from ast)
     ; and widen the contract to include racket bool on input
    [#t "true"]
    [#f "false"]
    ))

(define (process-children-formula run-or-state children relations atom-names quantvars)
  (map (lambda (x) (interpret-formula run-or-state x relations atom-names quantvars)) children))

(define (process-children-expr run-or-state children relations atom-names quantvars)
  (map (lambda (x) (interpret-expr run-or-state x relations atom-names quantvars)) children))

(define (process-children-int run-or-state children relations atom-names quantvars)
  (map (lambda (x) (interpret-int run-or-state x relations atom-names quantvars)) children))

(define (nnf-implies run-or-state children relations atom-names quantvars info)
  (let ([a (car children)]
        [b (cdr children)])
    (list (interpret-formula run-or-state (node/formula/op/! info (list a)) relations atom-names quantvars)
          (interpret-formula run-or-state (car b) relations atom-names quantvars))))

(define (distribute-not run-or-state args relations atom-names quantvars info)
  (map (lambda (x) (interpret-formula run-or-state (node/formula/op/! info (list x)) relations atom-names quantvars)) args))

(define (negate-quantifier run-or-state new-quantifier decls form relations atom-names quantvars info)
  ; Called in a context like !(some ...) ~~~> (all !...).
  ; NNF conversion thus needs to recur on the inner formula _including_ the negation.
  (define negated-inner-form (node/formula/op/! info (list form)))
  (define nnf-negated-inner-form (interpret-formula run-or-state negated-inner-form relations atom-names quantvars))
  (node/formula/quantified info new-quantifier decls nnf-negated-inner-form))

(define (interpret-formula-op run-or-state formula relations atom-names quantvars args)
  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "to-nnf: interpret-formula-op: ~a~n" formula))
  (match formula
    [(node/formula/op/&& info children)
      (node/formula/op/&& info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op/|| info children)
     (node/formula/op/|| info (process-children-formula run-or-state args relations atom-names quantvars))]
    [(node/formula/op/=> info children)
     (node/formula/op/|| info (nnf-implies run-or-state args relations atom-names quantvars info))]
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
      (match (car children)
        ; (not (a and b)) = (not a or not b)
        [(node/formula/op/&& and-info and-children) (node/formula/op/|| and-info (distribute-not run-or-state and-children relations atom-names quantvars and-info))]
        ; (not (a or b)) = (not a and not b)
        [(node/formula/op/|| or-info or-children) (node/formula/op/&& or-info (distribute-not run-or-state or-children relations atom-names quantvars or-info))]
        ; Remove double not (!(!a)) = a
        [(node/formula/op/! not-info not-children) (interpret-formula run-or-state (car not-children) relations atom-names quantvars)]
        ; Converting quantifiers to NNF
        [(node/formula/quantified quant-info quantifier decls form)
          (match quantifier
            ; !!! 
            ; not (all x | A) = some (x | not A)
            ['all (negate-quantifier run-or-state 'some decls form relations atom-names quantvars quant-info)]
            ; not (some x | A) = all (x | not A)
            ['some (negate-quantifier run-or-state 'all decls form relations atom-names quantvars quant-info)]
            ; raise forge error if we hit something else
            [_ (raise-forge-error #:msg (format "Unexpected quantifier: ~a" quantifier) #:context formula)])]
        [_ (node/formula/op/! info (process-children-formula run-or-state args relations atom-names quantvars))])]
    [(node/formula/op/int> info children)
      (node/formula/op/int> info (process-children-int run-or-state args relations atom-names quantvars))]
    [(node/formula/op/int< info children)
      (node/formula/op/int< info (process-children-int run-or-state args relations atom-names quantvars))]
    [(node/formula/op/int= info children)
     (node/formula/op/int= info (process-children-int run-or-state args relations atom-names quantvars))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Relational expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interpret-expr run-or-state expr relations atom-names quantvars)
  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
      (printf "to-nnf: interpret-expr: ~a~n" expr))
  (match expr
    [(node/expr/relation info arity name typelist-thunk parent isvar)
     expr]
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
     (node/expr/comprehension info len new-decls processed-form))]))

(define (interpret-expr-op run-or-state expr relations atom-names quantvars args)
    (when (@>= (get-verbosity) VERBOSITY_DEBUG)
      (printf "to-nnf: interpret-expr-op: ~a~n" expr))
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
     (node/expr/op/sing info arity (process-children-int run-or-state args relations atom-names quantvars))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Integer expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interpret-int run-or-state expr relations atom-names quantvars)
  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "to-nnf: interpret-int: ~a~n" expr))
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
  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "to-nnf: interpret-int-op: ~a~n" expr))
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
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;