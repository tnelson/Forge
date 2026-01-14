#lang typed/racket/base/optional

; This file performs variable substitution in Forge AST nodes.

(require
  forge/types/ast-adapter
  forge/types/sigs-structs-adapter
  forge/shared
  (only-in typed/racket match first second rest cons append list)
  (prefix-in @ (only-in typed/racket >=)))

(provide substitute-formula substitute-ambig)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Boolean formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Translate a formula AST node
; target - the node to be replaced
; value - the node to replace the target with
; relations and atom-names are currently unused (dead parameters)
(: substitute-formula (-> (U Run State Run-spec) node/formula (Listof Any) (Listof Any) (Listof node/expr/quantifier-var) node node node/formula))
(define (substitute-formula run-or-state formula relations atom-names quantvars target value)
  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "substitutor: interpret-formula: ~a~n" formula))
  (match formula
    [(node/formula/constant info type)
     (if (equal? formula target) (assert value node/formula?) formula)]    
    [(node/fmla/pred-spacer info name args expanded)
     (substitute-formula run-or-state expanded relations atom-names quantvars target value)]
    [(? node/formula/op?)
     (substitute-formula-op run-or-state formula relations atom-names quantvars (node/formula/op-children formula) target value)]
    [(node/formula/multiplicity info mult expr)
    (let ([processed-expr (substitute-expr run-or-state expr relations atom-names quantvars target value)])
     (node/formula/multiplicity info mult processed-expr))]
    [(node/formula/quantified info quantifier decls form)
    (define new-vs-and-decls
       (for/fold ([vs-and-decls : (List (Listof node/expr/quantifier-var) (Listof Decl)) (list quantvars '())])
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
    ))

(: process-children-formula (-> (U Run State Run-spec) (Listof node) (Listof Any) (Listof Any) (Listof node/expr/quantifier-var) node node (Listof node/formula)))
(define (process-children-formula run-or-state children relations atom-names quantvars target value)
  (map (lambda ([x : node]) (substitute-formula run-or-state (assert x node/formula?) relations atom-names quantvars target value)) children))

(: process-children-expr (-> (U Run State Run-spec) (Listof node) (Listof Any) (Listof Any) (Listof node/expr/quantifier-var) node node (Listof node/expr)))
(define (process-children-expr run-or-state children relations atom-names quantvars target value)
  (map (lambda ([x : node]) (substitute-expr run-or-state (assert x node/expr?) relations atom-names quantvars target value)) children))

(: process-children-int (-> (U Run State Run-spec) (Listof node) (Listof Any) (Listof Any) (Listof node/expr/quantifier-var) node node (Listof node/int)))
(define (process-children-int run-or-state children relations atom-names quantvars target value)
  (map (lambda ([x : node]) (substitute-int run-or-state (assert x node/int?) relations atom-names quantvars target value)) children))

(: process-children-ambiguous (-> (U Run State Run-spec) (Listof node) (Listof Any) (Listof Any) (Listof node/expr/quantifier-var) node node (Listof node)))
(define (process-children-ambiguous run-or-state children relations atom-names quantvars target value)
  (for/list : (Listof node) ([child : node children])
    (match child
      [(? node/formula? f) (substitute-formula run-or-state f relations atom-names quantvars target value)]
      [(? node/expr? e) (substitute-expr run-or-state e relations atom-names quantvars target value)]
      [(? node/int? i) (substitute-int run-or-state i relations atom-names quantvars target value)])))

(: substitute-ambig (-> (U Run State Run-spec) node (Listof Any) (Listof Any) (Listof node/expr/quantifier-var) node node node))
(define (substitute-ambig run-or-state formula relations atom-names quantvars target value)
  (match formula
    [(? node/formula? f) (substitute-formula run-or-state f relations atom-names quantvars target value)]
    [(? node/expr? e) (substitute-expr run-or-state e relations atom-names quantvars target value)]
    [(? node/int? i) (substitute-int run-or-state i relations atom-names quantvars target value)]))

(: substitute-formula-op (-> (U Run State Run-spec) node/formula/op (Listof Any) (Listof Any) (Listof node/expr/quantifier-var) (Listof node) node node node/formula))
(define (substitute-formula-op run-or-state formula relations atom-names quantvars args target value)
  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "substitutor: interpret-formula-op: ~a~n" formula))
  ; Use the info from the formula and the args parameter for children
  (define info (node-info formula))
  (match formula
    [(? node/formula/op-on-formulas/&&?)
      (node/formula/op-on-formulas/&& info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(? node/formula/op-on-formulas/||?)
     (node/formula/op-on-formulas/|| info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(? node/formula/op-on-formulas/=>?)
     (node/formula/op-on-formulas/=> info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(? node/formula/op-on-formulas/always?)
     (node/formula/op-on-formulas/always info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(? node/formula/op-on-formulas/eventually?)
     (node/formula/op-on-formulas/eventually info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(? node/formula/op-on-formulas/next_state?)
      (node/formula/op-on-formulas/next_state info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(? node/formula/op-on-formulas/releases?)
      (node/formula/op-on-formulas/releases info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(? node/formula/op-on-formulas/until?)
     (node/formula/op-on-formulas/until info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(? node/formula/op-on-formulas/historically?)
      (node/formula/op-on-formulas/historically info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(? node/formula/op-on-formulas/once?)
      (node/formula/op-on-formulas/once info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(? node/formula/op-on-formulas/prev_state?)
      (node/formula/op-on-formulas/prev_state info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(? node/formula/op-on-formulas/since?)
      (node/formula/op-on-formulas/since info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(? node/formula/op-on-formulas/triggered?)
      (node/formula/op-on-formulas/triggered info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(? node/formula/op-on-exprs/in?)
      (node/formula/op-on-exprs/in info (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(? node/formula/op-on-exprs/=?)
      (node/formula/op-on-exprs/= info (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(? node/formula/op-on-formulas/!?)
      (node/formula/op-on-formulas/! info (process-children-formula run-or-state args relations atom-names quantvars target value))]
    [(? node/formula/op-on-ints/int>?)
      (node/formula/op-on-ints/int> info (process-children-int run-or-state args relations atom-names quantvars target value))]
    [(? node/formula/op-on-ints/int<?)
      (node/formula/op-on-ints/int< info (process-children-int run-or-state args relations atom-names quantvars target value))]
    [(? node/formula/op-on-ints/int=?)
     (node/formula/op-on-ints/int= info (process-children-int run-or-state args relations atom-names quantvars target value))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Relational expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: substitute-expr (-> (U Run State Run-spec) node/expr (Listof Any) (Listof Any) (Listof node/expr/quantifier-var) node node node/expr))
(define (substitute-expr run-or-state expr relations atom-names quantvars target value)
  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
      (printf "substitutor: interpret-expr: ~a~n" expr))
  (if (equal? expr target)
      (assert value node/expr?)
      (match expr
        [(node/expr/relation info arity name typelist-thunk parent isvar)
         expr]  ; Already checked target above
        [(node/expr/atom info arity name)
         expr]  ; Already checked target above
        [(node/expr/fun-spacer info arity name args result expanded)
         (let ([new-expanded (substitute-expr run-or-state expanded relations atom-names quantvars target value)])
           (node/expr/fun-spacer info arity name args result new-expanded))]
        [(node/expr/ite info arity a b c)  
         (let ([processed-a (substitute-formula run-or-state a relations atom-names quantvars target value)]
               [processed-b (substitute-expr run-or-state b relations atom-names quantvars target value)]
               [processed-c (substitute-expr run-or-state c relations atom-names quantvars target value)])
           (node/expr/ite info arity processed-a processed-b processed-c))]
        [(node/expr/constant info 1 'Int)
         expr]  ; Already checked target above
        [(node/expr/constant info arity type)
         expr]  ; Already checked target above
        [(? node/expr/op?)
         (substitute-expr-op run-or-state expr relations atom-names quantvars (node/expr/op-children expr) target value)]
        [(node/expr/quantifier-var info arity sym name)
         expr]  ; Already checked target above
        [(node/expr/comprehension info len decls form)
         (define new-vs-and-decls
           (for/fold ([vs-and-decls : (List (Listof node/expr/quantifier-var) (Listof Decl)) (list quantvars '())])
                     ([decl decls])
             (define curr-quantvars (first vs-and-decls))
             (define curr-decls (second vs-and-decls))
             (define new-quantvars (cons (car decl) curr-quantvars))
             (define new-decl-domain (substitute-expr run-or-state (cdr decl) relations atom-names new-quantvars target value))
             (define new-decls (cons (cons (car decl) new-decl-domain) curr-decls))
             (list new-quantvars new-decls)))
         (define new-quantvars (first new-vs-and-decls))
         (let ([processed-form (substitute-formula run-or-state form relations atom-names new-quantvars target value)])
           (define new-decls (second new-vs-and-decls))
           (node/expr/comprehension info len new-decls processed-form))])))
  
(: substitute-expr-op (-> (U Run State Run-spec) node/expr/op (Listof Any) (Listof Any) (Listof node/expr/quantifier-var) (Listof node) node node node/expr))
(define (substitute-expr-op run-or-state expr relations atom-names quantvars args target value)
  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "substitutor: interpret-expr-op: ~a~n" expr))
  ; Use accessors for info and arity
  (define info (node-info expr))
  (define arity (node/expr-arity expr))
  (match expr
    [(? node/expr/op-on-exprs/+?)
     (node/expr/op-on-exprs/+ info arity (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(? node/expr/op-on-exprs/-?)
     (node/expr/op-on-exprs/- info arity (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(? node/expr/op-on-exprs/&?)
     (node/expr/op-on-exprs/& info arity (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(? node/expr/op-on-exprs/->?)
     (node/expr/op-on-exprs/-> info arity (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(? node/expr/op-on-exprs/prime?)
     (node/expr/op-on-exprs/prime info arity (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(? node/expr/op-on-exprs/join?)
     (node/expr/op-on-exprs/join info arity (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(? node/expr/op-on-exprs/^?)
     (node/expr/op-on-exprs/^ info arity (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(? node/expr/op-on-exprs/*?)
    (node/expr/op-on-exprs/* info arity (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(? node/expr/op-on-exprs/~?)
     (node/expr/op-on-exprs/~ info arity (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(? node/expr/op-on-exprs/++?)
     (node/expr/op-on-exprs/++ info arity (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(? node/expr/op-on-ints/sing?)
     (if (equal? (car args) target)
         (assert value node/expr?)
         (node/expr/op-on-ints/sing info arity (process-children-int run-or-state args relations atom-names quantvars target value)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Integer expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: substitute-int (-> (U Run State Run-spec) node/int (Listof Any) (Listof Any) (Listof node/expr/quantifier-var) node node node/int))
(define (substitute-int run-or-state expr relations atom-names quantvars target value)
  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "substitutor: interpret-int: ~a~n" expr))
  ; TEMP fix to match int variables. Should probably modify process-children-int to handle constants.
  (if (equal? expr target)
      (assert value node/int?)
      (match expr
        [(node/int/constant info val)
         expr]  ; Already checked target above
        [(? node/int/op?)
         (substitute-int-op run-or-state expr relations atom-names quantvars (node/int/op-children expr) target value)]
        [(node/int/sum-quant info decls int-expr)
         (define new-vs-and-decls
           (for/fold ([vs-and-decls : (List (Listof node/expr/quantifier-var) (Listof Decl)) (list quantvars '())])
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

(: substitute-int-op (-> (U Run State Run-spec) node/int/op (Listof Any) (Listof Any) (Listof node/expr/quantifier-var) (Listof node) node node node/int))
(define (substitute-int-op run-or-state expr relations atom-names quantvars args target value)
  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "substitutor: interpret-int-op: ~a~n" expr))
  ; Use accessor for info
  (define info (node-info expr))
  (match expr
    [(? node/int/op-on-ints/add?)
     (node/int/op-on-ints/add info (process-children-int run-or-state args relations atom-names quantvars target value))]
    [(? node/int/op-on-ints/subtract?)
     (node/int/op-on-ints/subtract info (process-children-int run-or-state args relations atom-names quantvars target value))]
    [(? node/int/op-on-ints/multiply?)
     (node/int/op-on-ints/multiply info (process-children-int run-or-state args relations atom-names quantvars target value))]
    [(? node/int/op-on-ints/divide?)
     (node/int/op-on-ints/divide info (process-children-int run-or-state args relations atom-names quantvars target value))]
    [(? node/int/op-on-exprs/sum?)
     (node/int/op-on-exprs/sum info (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(? node/int/op-on-exprs/card?)
     (node/int/op-on-exprs/card info (process-children-expr run-or-state args relations atom-names quantvars target value))]
    [(? node/int/op-on-ints/remainder?)
     (node/int/op-on-ints/remainder info (process-children-int run-or-state args relations atom-names quantvars target value))]
    [(? node/int/op-on-ints/abs?)
     (node/int/op-on-ints/abs info (process-children-int run-or-state args relations atom-names quantvars target value))]
    [(? node/int/op-on-ints/sign?)
     (node/int/op-on-ints/sign info (process-children-int run-or-state args relations atom-names quantvars target value))]
    [(node/int/sum-quant info decls int-expr)
     (raise-forge-error #:msg "Reached expected unreachable code." #:context expr)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;