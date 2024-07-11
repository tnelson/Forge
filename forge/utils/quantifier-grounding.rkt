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
    [(node/formula/op info args)
     (interpret-formula-op run-or-state formula relations atom-names quantvars quantvar-types args bounds)]
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
  (define and-node (node/formula/op/&& info inner-formula-list))
  (printf "and node: ~a~n" and-node)
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
    [(node/formula/op/&& info children)
      (node/formula/op/&& info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op/|| info children)
     (node/formula/op/|| info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op/=> info children)
     (node/formula/op/=> info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op/always info children)
     (node/formula/op/always info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op/eventually info children)
     (node/formula/op/eventually info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op/next_state info children)
      (node/formula/op/next_state info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op/releases info children)
      (node/formula/op/releases info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op/until info children)
     (node/formula/op/until info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op/historically info children)
      (node/formula/op/historically info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op/once info children)
      (node/formula/op/once info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op/prev_state info children)
      (node/formula/op/prev_state info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op/since info children)
      (node/formula/op/since info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op/triggered info children)
      (node/formula/op/triggered info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op/in info children)
      (node/formula/op/in info (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op/= info children)
      (node/formula/op/= info (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op/! info children)
      (node/formula/op/! info (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op/int> info children)
      (node/formula/op/int> info (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op/int< info children)
      (node/formula/op/int< info (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/formula/op/int= info children)
     (node/formula/op/int= info (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds))]))

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
    [(node/expr/op info arity args)
     (interpret-expr-op run-or-state expr relations atom-names quantvars quantvar-types args bounds)]
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
    [(node/expr/op/+ info arity children)
     (node/expr/op/+ info arity (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/expr/op/- info arity children)
     (node/expr/op/- info arity (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/expr/op/& info arity children)
     (node/expr/op/& info arity (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/expr/op/-> info arity children)
     (node/expr/op/-> info arity (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/expr/op/prime info arity children)
     (node/expr/op/prime info arity (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/expr/op/join info arity children)
     (node/expr/op/join info arity (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/expr/op/^ info arity children)
     (node/expr/op/^ info arity (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/expr/op/* info arity children)
    (node/expr/op/* info arity (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/expr/op/~ info arity children)
     (node/expr/op/~ info arity (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/expr/op/++ info arity children)
     (node/expr/op/++ info arity (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/expr/op/sing info arity children)
     (node/expr/op/sing info arity (process-children-int run-or-state args relations atom-names quantvars quantvar-types bounds))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Integer expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interpret-int run-or-state expr relations atom-names quantvars quantvar-types bounds)
  (when (@>= (get-verbosity) 2)
    (printf "quantifier-grounding: interpret-int: ~a~n" expr))
  (match expr
    [(node/int/constant info value)
     (node/int/constant info value)]
    [(node/int/op info args)
     (interpret-int-op run-or-state expr relations atom-names quantvars quantvar-types args bounds)]
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
    [(node/int/op/add info children)
      (node/int/op/add info (process-children-int run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/int/op/subtract info children)
    (node/int/op/subtract info (process-children-int run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/int/op/multiply info children)
    (node/int/op/multiply info (process-children-int run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/int/op/divide info children)
    (node/int/op/divide info (process-children-int run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/int/op/sum info children)
    (node/int/op/sum info (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/int/op/card info children)
    (node/int/op/card info (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/int/op/remainder info children)
     (node/int/op/remainder info (process-children-int run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/int/op/abs info children)
     (node/int/op/abs info (process-children-int run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/int/op/sign info children)
     (node/int/op/sign info (process-children-int run-or-state args relations atom-names quantvars quantvar-types bounds))]
    [(node/int/sum-quant info decls int-expr)
     (raise-forge-error #:msg "Reached expected unreachable code." #:context expr)]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;