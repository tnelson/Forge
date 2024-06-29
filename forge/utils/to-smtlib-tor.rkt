#lang racket/base

; This file is intended to take in the Forge AST and return a (list of) string(s) in the 
; SMTlibv2 file, which are intended to be ran with CVC5.

(require 
  forge/sigs-structs
  forge/lang/ast
  forge/shared
  (only-in racket index-of match string-join first second rest)
  (only-in racket/contract define/contract or/c listof any/c)
  (prefix-in @ (only-in racket/contract ->))
  (prefix-in @ (only-in racket/base >=)))

(provide convert-formula)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Boolean formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Translate a formula AST node
(define/contract (convert-formula run-or-state formula relations atom-names quantvars)  
  (@-> (or/c Run? State? Run-spec?)
      node/formula?
      list?
      list?
      list?
      string?)
  (when (@>= (get-verbosity) 2)
    (printf "to-smtlib-tor: convert-formula: ~a~n" formula))
  (match formula
    [(node/formula/constant info type)
     (if type "true" "false")]    
    [(node/fmla/pred-spacer info name args expanded)
     (convert-formula run-or-state expanded relations atom-names quantvars)]
    [(node/formula/op info args)
     (convert-formula-op run-or-state formula relations atom-names quantvars args)]
    [(node/formula/multiplicity info mult expr)
    (let ([processed-expr (convert-expr run-or-state expr relations atom-names quantvars)])
     ; I think these require specific cases... not sure which ones get desugared
      "TODO: MULTIPLICITY")]
    [(node/formula/quantified info quantifier decls form)
     (define new-vs-and-decls
       (for/fold ([vs-and-decls (list quantvars '())])
                 ([decl decls])
         (define curr-quantvars (first vs-and-decls))
         (define curr-decls (second vs-and-decls))
         (define new-quantvars (cons (car decl) quantvars))
         (define new-decl-domain (convert-expr run-or-state (cdr decl) relations atom-names new-quantvars))
         (define new-decls (cons (cons (car decl) new-decl-domain) curr-decls))
         (list new-quantvars new-decls)))
     (define new-quantvars (first new-vs-and-decls))
     (let ([processed-form (convert-formula run-or-state form relations atom-names new-quantvars)])
       (define new-decls (second new-vs-and-decls))
       (format "(~a (~a) ~a)"
               ; SMT-LIB uses "forall", not "all" and "exists", not "some"
               (if (equal? quantifier 'all) "forall" "exists")
               (string-join (map (lambda (x) (format "(~a ~a)" (car x) (cdr x))) new-decls) " ")
               processed-form))]
    [(node/formula/sealed info)
     (node/formula/sealed info)]
    [#t "true"]
    [#f "false"]
    ))

(define (process-children-formula run-or-state children relations atom-names quantvars)
  (map (lambda (x) (convert-formula run-or-state x relations atom-names quantvars)) children))

(define (process-children-expr run-or-state children relations atom-names quantvars)
  (map (lambda (x) (convert-expr run-or-state x relations atom-names quantvars)) children))

(define (process-children-int run-or-state children relations atom-names quantvars)
  (map (lambda (x) (convert-int run-or-state x relations atom-names quantvars)) children))

(define (process-children-ambiguous run-or-state children relations atom-names quantvars)
  (for/list ([child children])
    (match child
      [(? node/formula? f) (convert-formula run-or-state f relations atom-names quantvars)]
      [(? node/expr? e) (convert-expr run-or-state e relations atom-names quantvars)]
      [(? node/int? i) (convert-int run-or-state i relations atom-names quantvars)])))

(define (convert-formula-op run-or-state formula relations atom-names quantvars args)
  (when (@>= (get-verbosity) 2)
    (printf "to-smtlib-tor: convert-formula-op: ~a~n" formula))
  (match formula
    [(node/formula/op/&& info children)
      (format "(and ~a)" (string-join (process-children-formula run-or-state args relations atom-names quantvars) " "))]
    [(node/formula/op/|| info children)
     (format "(or ~a)" (string-join (process-children-formula run-or-state args relations atom-names quantvars) " "))]
    [(node/formula/op/=> info children)
     (format "(=> ~a)" (string-join (process-children-formula run-or-state args relations atom-names quantvars) " "))]
    [(node/formula/op/always info children)
     (format "(always ~a)" (string-join (process-children-formula run-or-state args relations atom-names quantvars) " "))]
    [(node/formula/op/eventually info children)
     (format "(eventually ~a)" (string-join (process-children-formula run-or-state args relations atom-names quantvars) " "))]
    [(node/formula/op/next_state info children)
      (format "(next_state ~a)" (string-join (process-children-formula run-or-state args relations atom-names quantvars) " "))]
    [(node/formula/op/releases info children)
      (format "(releases ~a)" (string-join (process-children-formula run-or-state args relations atom-names quantvars) " "))]
    [(node/formula/op/until info children)
     (format "(until ~a)" (string-join (process-children-formula run-or-state args relations atom-names quantvars) " "))]
    [(node/formula/op/historically info children)
      (format "(historically ~a)" (string-join (process-children-formula run-or-state args relations atom-names quantvars) " "))]
    [(node/formula/op/once info children)
      (format "(once ~a)" (string-join (process-children-formula run-or-state args relations atom-names quantvars) " "))]
    [(node/formula/op/prev_state info children)
      (format "(prev_state ~a)" (string-join (process-children-formula run-or-state args relations atom-names quantvars) " "))]
    [(node/formula/op/since info children)
      (format "(since ~a)" (string-join (process-children-formula run-or-state args relations atom-names quantvars) " "))]
    [(node/formula/op/triggered info children)
      (format "(triggered ~a)" (string-join (process-children-formula run-or-state args relations atom-names quantvars) " "))]
    [(node/formula/op/in info children)
      (format "(set.subset ~a)" (string-join (process-children-expr run-or-state args relations atom-names quantvars) " "))]
    [(node/formula/op/= info children)
      (format "(= ~a)" (string-join (process-children-expr run-or-state args relations atom-names quantvars) " "))]
    [(node/formula/op/! info children)
      (format "(not ~a)" (string-join (process-children-formula run-or-state args relations atom-names quantvars) ""))]
    [(node/formula/op/int> info children)
      (format "(> ~a)" (string-join (process-children-ambiguous run-or-state args relations atom-names quantvars)))]
    [(node/formula/op/int< info children)
      (format "(< ~a)" (string-join (process-children-ambiguous run-or-state args relations atom-names quantvars)))]
    [(node/formula/op/int= info children)
     (format "(= ~a)" (string-join (process-children-ambiguous run-or-state args relations atom-names quantvars)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Relational expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (convert-expr run-or-state expr relations atom-names quantvars)
  (when (@>= (get-verbosity) 2)
      (printf "to-smtlib-tor: convert-expr: ~a~n" expr))
  (match expr
    [(node/expr/relation info arity name typelist-thunk parent isvar)
     (if (equal? name "Int")
         ; *******
         ; TODO: this is not correct *****
         ;; ************
         ;"(as set.universe (Set Int))"
         "(as set.universe (Relation Int))"
         (format "~a" name))]
    [(node/expr/atom info arity name)
     "TODO: ATOM"]
    [(node/expr/fun-spacer info arity name args result expanded)
     (convert-expr run-or-state expanded relations atom-names quantvars)]
    [(node/expr/ite info arity a b c)  
    (let ([processed-a (convert-formula run-or-state a relations atom-names quantvars)]
          [processed-b (convert-expr run-or-state b relations atom-names quantvars)]
          [processed-c (convert-expr run-or-state c relations atom-names quantvars)])
     (format "(ite ~a ~a ~a)" processed-a processed-b processed-c))]
    [(node/expr/constant info 1 'Int)
     "TODO: EXPR CONSTANT? IDK WHAT THIS IS"]
    [(node/expr/constant info arity type)
     "TODO: ANOTHER EXPR CONSTANT? IDK WHAT THIS IS"]
    [(node/expr/op info arity args)
     (convert-expr-op run-or-state expr relations atom-names quantvars args)]
    [(node/expr/quantifier-var info arity sym name)  
     (format "(set.singleton (tuple ~a))" name)]
    [(node/expr/comprehension info len decls form)   
     (define new-vs-and-decls
       (for/fold ([vs-and-decls (list quantvars '())])
                 ([decl decls])
         (define curr-quantvars (first vs-and-decls))
         (define curr-decls (second vs-and-decls))
         (define new-quantvars (cons (car decl) quantvars))
         (define new-decl-domain (convert-expr run-or-state (cdr decl) relations atom-names new-quantvars))
         (define new-decls (cons (cons (car decl) new-decl-domain) curr-decls))
         (list new-quantvars new-decls)))
     (define new-quantvars (first new-vs-and-decls))
     (let ([processed-form (convert-formula run-or-state form relations atom-names new-quantvars)])
       (define new-decls (second new-vs-and-decls))
     "TODO: COMPREHENSION")]))

(define (convert-expr-op run-or-state expr relations atom-names quantvars args)
    (when (@>= (get-verbosity) 2)
      (printf "to-smtlib-tor: convert-expr-op: ~a~n" expr))
  (match expr
    [(node/expr/op/+ info arity children)
     (format "(set.union ~a)" (string-join (process-children-expr run-or-state args relations atom-names quantvars) " "))]
    [(node/expr/op/- info arity children)
     (format "(set.minus ~a)" (string-join (process-children-expr run-or-state args relations atom-names quantvars) " "))]
    [(node/expr/op/& info arity children)
     (format "(set.inter ~a)" (string-join (process-children-expr run-or-state args relations atom-names quantvars) " "))]
    [(node/expr/op/-> info arity children)
     (format "(rel.product ~a)" (string-join (process-children-expr run-or-state args relations atom-names quantvars) " "))]
    [(node/expr/op/prime info arity children)
     "TODO: temporal ?"]
    [(node/expr/op/join info arity children)
     (format "(rel.join ~a)" (string-join (process-children-expr run-or-state args relations atom-names quantvars) " "))]
    [(node/expr/op/^ info arity children)
     (format "(rel.tclosure ~a)" (string-join (process-children-expr run-or-state args relations atom-names quantvars) " "))]
    [(node/expr/op/* info arity children)
      "TODO: reflexive tclosure"]
    [(node/expr/op/~ info arity children)
     (format "(rel.transpose ~a)" (string-join (process-children-expr run-or-state args relations atom-names quantvars) " "))]
    [(node/expr/op/++ info arity children)
     "TODO: ++ (idk what that is)"]
    [(node/expr/op/sing info arity children)
     (format "(set.singleton (tuple ~a))" (string-join (process-children-int run-or-state args relations atom-names quantvars) " "))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Integer expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (convert-int run-or-state expr relations atom-names quantvars)
  (when (@>= (get-verbosity) 2)
    (printf "to-smtlib-tor: convert-int: ~a~n" expr))
  (match expr
    [(node/int/constant info value)
     (format "~a" value)]
    [(node/int/op info args)
     (convert-int-op run-or-state expr relations atom-names quantvars args)]
    [(node/int/sum-quant info decls int-expr)
     (define new-vs-and-decls
       (for/fold ([vs-and-decls (list quantvars '())])
                 ([decl decls])
         (define curr-quantvars (first vs-and-decls))
         (define curr-decls (second vs-and-decls))
         (define new-quantvars (cons (car decl) quantvars))
         (define new-decl-domain (convert-expr run-or-state (cdr decl) relations atom-names new-quantvars))
         (define new-decls (cons (cons (car decl) new-decl-domain) curr-decls))
         (list new-quantvars new-decls)))
     (define new-quantvars (first new-vs-and-decls))
     (let ([processed-int (convert-int run-or-state int-expr relations atom-names new-quantvars)])
       (define new-decls (second new-vs-and-decls))
      "TODO: sum quant")]
    [_ "oops"]))

(define (convert-int-op run-or-state expr relations atom-names quantvars args)
  (when (@>= (get-verbosity) 2)
    (printf "to-smtlib-tor: convert-int-op: ~a~n" expr))
  (match expr
    [(node/int/op/add info children)
      (format "(+ ~a)" (string-join (process-children-int run-or-state args relations atom-names quantvars) " "))]
    [(node/int/op/subtract info children)
      (format "(- ~a)" (string-join (process-children-int run-or-state args relations atom-names quantvars) " "))]
    [(node/int/op/multiply info children)
      (format "(* ~a)" (string-join (process-children-int run-or-state args relations atom-names quantvars) " "))]
    [(node/int/op/divide info children)
      (format "(div ~a)" (string-join (process-children-int run-or-state args relations atom-names quantvars) " "))]
    [(node/int/op/sum info children)
    "TODO: sum"]
    [(node/int/op/card info children)
    "TODO: cardinality"]
    [(node/int/op/remainder info children)
     (format "(mod ~a)" (string-join (process-children-int run-or-state args relations atom-names quantvars) " "))]
    [(node/int/op/abs info children)
     (format "(abs ~a)" (string-join (process-children-int run-or-state args relations atom-names quantvars) " "))]
    [(node/int/op/sign info children)
     "TODO: sign"]
    [(node/int/sum-quant info decls int-expr)
     (raise-forge-error #:msg "Reached expected unreachable code." #:context expr)]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;