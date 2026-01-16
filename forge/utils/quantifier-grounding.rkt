#lang racket/base

; This file is intended to take in the Forge AST and return a translation of it
; where quantifier variables are 'grounded out' into applying the formula to each 
; element of the upper bound of the quantifier variable.

(require 
  forge/sigs-structs
  forge/lang/ast
  forge/shared
  forge/lang/bounds
  forge/utils/substitutor
  (only-in racket index-of match string-join first second rest flatten)
  (only-in racket/contract define/contract or/c listof any/c)
  (prefix-in @ (only-in racket/contract ->))
  (prefix-in @ (only-in racket/base >=)))

(provide interpret-formula)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Boolean formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Translate a formula AST node
(define/contract (interpret-formula run-or-state formula relations atom-names quantvars quantvar-types bounds)  
  (@-> (or/c Run? State? Run-spec?)
      node/formula?
      list?
      list?
      list?
      list?
      list?
      node?)
  (when (@>= (get-verbosity) 2)
    (printf "quantifier-grounding: interpret-formula: ~a~n" formula))
  (match formula
    [(node/formula/constant info type)
     (node/formula/constant info type)]    
    [(node/fmla/pred-spacer info name args expanded)
     formula]
    [(? node/formula/op?)
     (interpret-formula-op run-or-state formula relations atom-names quantvars quantvar-types (node/formula/op-children formula) bounds)]
    [(node/formula/multiplicity info mult expr)
    (let ([processed-expr (interpret-expr run-or-state expr relations atom-names quantvars quantvar-types bounds)])
     (node/formula/multiplicity info mult processed-expr))]
    [(node/formula/quantified info quantifier decls form)
      ; match the quantifier - when we encounter a universal,
      ; we don't want to generate the quantvar. instead, we 'ground it out'
      ; by enumerating the formulas with each element of the upper bound 
      ; and wrapping them in an 'and' statement.
      (match quantifier
        ['all 
          ; recursively descend on the inner formula, in case there are nested quantifiers.
          (define new-inner-form (interpret-formula run-or-state form relations atom-names quantvars quantvar-types bounds))
          ; now ground out the quantifier we are currently on, using the helper
          (grounding-helper run-or-state decls new-inner-form relations atom-names quantvars quantvar-types bounds info)]
        [_
          (define new-vs-and-decls
            (for/fold ([vs-and-decls (list quantvars '())])
                      ([decl decls])
              (define curr-quantvars (first vs-and-decls))
              (define curr-decls (second vs-and-decls))
              (define new-quantvars (cons (car decl) quantvars))
              (define new-decl-domain (interpret-expr run-or-state (cdr decl) relations atom-names new-quantvars quantvar-types bounds))
              (define new-decls (cons (cons (car decl) new-decl-domain) curr-decls))
              (list new-quantvars new-decls)))
          (define new-quantvars (first new-vs-and-decls))
          (let ([processed-form (interpret-formula run-or-state form relations atom-names new-quantvars quantvar-types bounds)])
            (define new-decls (second new-vs-and-decls))
            (node/formula/quantified info quantifier new-decls processed-form))])]
    [(node/formula/sealed info)
     (node/formula/sealed info)]
    [#t "true"]
    [#f "false"]
    ))

(define (grounding-helper run-or-state decls new-inner-form relations atom-names quantvars quantvar-types bounds info)
  ; decls has the info we need to ground out the quantifier.
  (define dummy-hash (make-hash))
  ; map each type to the respective upper bound
  (define vars-atoms
    (for/fold ([acc '()]) ([bound bounds])
      (append acc
              (for/fold ([inner-acc '()]) ([decl decls])
                                (if (and (not (equal? (cdr decl) 'Int))
                                            (equal? (relation-name (cdr decl)) (relation-name (bound-relation bound))))
                                (cons (cons (car decl) (flatten (bound-upper bound))) inner-acc)
                                inner-acc)))))
  ; now we want to essentially return an 'and' node where the sub expressions are 
  ; the inner formula with each element of the upper bound substituted in for the quantvar.
  (define (inner-formula-helper inner-formula-list var-and-atoms)
    (define var-to-replace (car var-and-atoms))
    (define vars-to-substitute (cdr var-and-atoms))
    (define inner-substituted-list
      (for/list ([inner-fmla inner-formula-list])
        (for/list ([var vars-to-substitute])
          (substitute-formula run-or-state inner-fmla relations atom-names quantvars var-to-replace (node/expr/atom info 1 var))
        )
      )
    )
    (apply append inner-substituted-list)
  )

  (define (inner-formula-recursive-helper current-pair current-list)
    (if (equal? current-pair '())
        current-list 
        (inner-formula-recursive-helper (cdr current-pair) (inner-formula-helper current-list (car current-pair)))
    )
  )

  (define inner-formula-list (inner-formula-recursive-helper vars-atoms (list new-inner-form)))
  (define and-node (node/formula/op-on-formulas/&& info inner-formula-list))
  and-node
)

(define (process-children-formula run-or-state children relations atom-names quantvars quantvar-types bounds)
  (map (lambda (x) (interpret-formula run-or-state x relations atom-names quantvars quantvar-types bounds)) children))

(define (process-children-expr run-or-state children relations atom-names quantvars quantvar-types bounds)
  (map (lambda (x) (interpret-expr run-or-state x relations atom-names quantvars quantvar-types bounds)) children))

(define (process-children-int run-or-state children relations atom-names quantvars quantvar-types bounds)
  (map (lambda (x) (interpret-int run-or-state x relations atom-names quantvars quantvar-types bounds)) children))

(define (process-children-ambiguous run-or-state children relations atom-names quantvars quantvar-types bounds)
  (for/list ([child children])
      (match child
        [(? node/formula? f) (interpret-formula run-or-state f relations atom-names quantvars quantvar-types bounds)]
        [(? node/expr? e) (interpret-expr run-or-state e relations atom-names quantvars quantvar-types bounds)]
        [(? node/int? i) (interpret-int run-or-state i relations atom-names quantvars quantvar-types bounds)])))

(define (interpret-formula-op run-or-state formula relations atom-names quantvars quantvar-types args bounds)
  (when (@>= (get-verbosity) 2)
    (printf "quantifier-grounding: interpret-formula-op: ~a~n" formula))
  (match formula
    [(node/formula/op-on-formulas/&& info children)
      (node/formula/op-on-formulas/&& info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op-on-formulas/|| info children)
     (node/formula/op-on-formulas/|| info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op-on-formulas/=> info children)
     (node/formula/op-on-formulas/=> info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op-on-formulas/always info children)
     (node/formula/op-on-formulas/always info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op-on-formulas/eventually info children)
     (node/formula/op-on-formulas/eventually info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op-on-formulas/next_state info children)
      (node/formula/op-on-formulas/next_state info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op-on-formulas/releases info children)
      (node/formula/op-on-formulas/releases info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op-on-formulas/until info children)
     (node/formula/op-on-formulas/until info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op-on-formulas/historically info children)
      (node/formula/op-on-formulas/historically info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op-on-formulas/once info children)
      (node/formula/op-on-formulas/once info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op-on-formulas/prev_state info children)
      (node/formula/op-on-formulas/prev_state info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op-on-formulas/since info children)
      (node/formula/op-on-formulas/since info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op-on-formulas/triggered info children)
      (node/formula/op-on-formulas/triggered info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op-on-exprs/in info children)
      (node/formula/op-on-exprs/in info (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op-on-exprs/= info children)
      (node/formula/op-on-exprs/= info (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op-on-formulas/! info children)
      (node/formula/op-on-formulas/! info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op-on-ints/int> info children)
      (node/formula/op-on-ints/int> info (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op-on-ints/int< info children)
      (node/formula/op-on-ints/int< info (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op-on-ints/int= info children)
     (node/formula/op-on-ints/int= info (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Relational expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interpret-expr run-or-state expr relations atom-names quantvars quantvar-types bounds)
  (when (@>= (get-verbosity) 2)
      (printf "quantifier-grounding: interpret-expr: ~a~n" expr))
  (match expr
    [(node/expr/relation info arity name typelist-thunk parent isvar)
     expr]
    [(node/expr/atom info arity name)
     (node/expr/atom info arity name)]
    [(node/expr/fun-spacer info arity name args result expanded)
     expr]
    [(node/expr/ite info arity a b c)  
    (let ([processed-a (interpret-formula run-or-state a relations atom-names quantvars quantvar-types bounds)]
          [processed-b (interpret-expr run-or-state b relations atom-names quantvars quantvar-types bounds)]
          [processed-c (interpret-expr run-or-state c relations atom-names quantvars quantvar-types bounds)])
     (node/expr/ite info arity processed-a processed-b processed-c))]
    [(node/expr/constant info 1 'Int)
     (node/expr/constant info 1 'Int)]
    [(node/expr/constant info arity type)
     (node/expr/constant info arity type)]
    [(? node/expr/op? op)
     (interpret-expr-op run-or-state expr relations atom-names quantvars quantvar-types (node/expr/op-children op) bounds)]
    [(node/expr/quantifier-var info arity sym name)  
     (node/expr/quantifier-var info arity sym name)]
    [(node/expr/comprehension info len decls form)   
     (define new-vs-and-decls
       (for/fold ([vs-and-decls (list quantvars '())])
                 ([decl decls])
         (define curr-quantvars (first vs-and-decls))
         (define curr-decls (second vs-and-decls))
         (define new-quantvars (cons (car decl) curr-quantvars))
         (define new-decl-domain (interpret-expr run-or-state (cdr decl) relations atom-names new-quantvars quantvar-types bounds))
         (define new-decls (cons (cons (car decl) new-decl-domain) curr-decls))
         (list new-quantvars new-decls)))
     (define new-quantvars (first new-vs-and-decls))
     (let ([processed-form (interpret-formula run-or-state form relations atom-names new-quantvars quantvar-types bounds)])
       (define new-decls (second new-vs-and-decls))
     (node/expr/comprehension info len new-decls processed-form))]))

(define (interpret-expr-op run-or-state expr relations atom-names quantvars quantvar-types args bounds)
    (when (@>= (get-verbosity) 2)
      (printf "quantifier-grounding: interpret-expr-op: ~a~n" expr))
  (match expr
    [(node/expr/op-on-exprs/+ info arity children)
     (node/expr/op-on-exprs/+ info arity (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/expr/op-on-exprs/- info arity children)
     (node/expr/op-on-exprs/- info arity (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/expr/op-on-exprs/& info arity children)
     (node/expr/op-on-exprs/& info arity (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/expr/op-on-exprs/-> info arity children)
     (node/expr/op-on-exprs/-> info arity (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/expr/op-on-exprs/prime info arity children)
     (node/expr/op-on-exprs/prime info arity (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/expr/op-on-exprs/join info arity children)
     (node/expr/op-on-exprs/join info arity (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/expr/op-on-exprs/^ info arity children)
     (node/expr/op-on-exprs/^ info arity (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/expr/op-on-exprs/* info arity children)
    (node/expr/op-on-exprs/* info arity (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/expr/op-on-exprs/~ info arity children)
     (node/expr/op-on-exprs/~ info arity (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/expr/op-on-exprs/++ info arity children)
     (node/expr/op-on-exprs/++ info arity (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/expr/op-on-ints/sing info arity children)
     (node/expr/op-on-ints/sing info arity (process-children-int run-or-state args relations atom-names quantvars quantvar-types bounds))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Integer expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interpret-int run-or-state expr relations atom-names quantvars quantvar-types bounds)
  (when (@>= (get-verbosity) 2)
    (printf "quantifier-grounding: interpret-int: ~a~n" expr))
  (match expr
    [(node/int/constant info value)
     (node/int/constant info value)]
    [(? node/int/op? op)
     (interpret-int-op run-or-state expr relations atom-names quantvars quantvar-types (node/int/op-children op) bounds)]
    [(node/int/sum-quant info decls int-expr)
     (define new-vs-and-decls
       (for/fold ([vs-and-decls (list quantvars '())])
                 ([decl decls])
         (define curr-quantvars (first vs-and-decls))
         (define curr-decls (second vs-and-decls))
         (define new-quantvars (cons (car decl) quantvars))
         (define new-decl-domain (interpret-expr run-or-state (cdr decl) relations atom-names new-quantvars quantvar-types bounds))
         (define new-decls (cons (cons (car decl) new-decl-domain) curr-decls))
         (list new-quantvars new-decls)))
     (define new-quantvars (first new-vs-and-decls))
     (let ([processed-int (interpret-int run-or-state int-expr relations atom-names new-quantvars quantvar-types bounds)])
       (define new-decls (second new-vs-and-decls))
      (node/int/sum-quant info new-decls processed-int))]))

(define (interpret-int-op run-or-state expr relations atom-names quantvars quantvar-types args bounds)
  (when (@>= (get-verbosity) 2)
    (printf "quantifier-grounding: interpret-int-op: ~a~n" expr))
  (match expr
    [(node/int/op-on-ints/add info children)
    (node/int/op-on-ints/add info (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/int/op-on-ints/subtract info children)
    (node/int/op-on-ints/subtract info (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/int/op-on-ints/multiply info children)
    (node/int/op-on-ints/multiply info (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/int/op-on-ints/divide info children)
    (node/int/op-on-ints/divide info (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/int/op-on-exprs/sum info children)
    (node/int/op-on-exprs/sum info (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/int/op-on-exprs/card info children)
    (node/int/op-on-exprs/card info (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/int/op-on-ints/remainder info children)
     (node/int/op-on-ints/remainder info (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/int/op-on-ints/abs info children)
     (node/int/op-on-ints/abs info (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/int/op-on-ints/sign info children)
     (node/int/op-on-ints/sign info (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/int/sum-quant info decls int-expr)
     (raise-forge-error #:msg "Reached expected unreachable code." #:context expr)]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;