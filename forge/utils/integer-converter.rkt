#lang racket/base

; This file is intended to take in the Forge AST and return the AST in a form where we have 
; replaced integer expressions with an existential generation of the Int.

(require 
  forge/sigs-structs
  forge/lang/ast
  forge/shared
  forge/utils/substitutor
  forge/utils/collector
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
  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "integer-converter: interpret-formula: ~a~n" formula))
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

(define (reconcile-integer-expr run-or-state children relations atom-names quantvars info form)
  ; sample: <= s.spent 15
    ; exists x : Int | sing[x] in s.spent && x <= 15
  ; we know we have an int expression.
  ; quantified with children
    ; and with children
      ; in with children
        ; sing[x]
        ; initial child of the non-int-expr child of the thing passed in
      ; SUBSTITUTED child of the thing passed in (subbing quantified var in for the thing we have 'in' before)
    ; TODO: only works on 1 expr for now. If we have 2 exprs, we need to handle that (multiple quantvars...)

  
  (define annotated-info (update-annotation info 'smt/int-unwrap form))

  ;;;;;;;;;;;;;;;
  ; TN comment: read the below not as "fix these" but perhaps notes to help us to think how to solve
  ;   the conversion problem. The problem we're facing (outside the below) is one of context.
  ;   E.g., in p.age >= 100, we need to recognize later on, in the final SMT-LIB conversion, that
  ;   in (p.age = x and p.age >= 100) only the first needs wrapping in set.singleton and tuple:
  ;   (and (= (rel.join p age) (set.singleton (tuple x)))
  ;        (>= x 100))
  ;   - Option: match on, specifically, (some ((x Int)) (&& FOO BAR) with this form. But is there
  ;     risk of over-eager matching?
  ;   - Option: leave a contextual tag as part of this process. sum[x] perhaps, if x is a quant-var.
  ;     This does feel mildly kludgey, since users could have written sum themselves.
  ;   - Option: We know the type of "x" when we're doing the final recursive descent that produces
  ;     strings. So we could tell that "x" is of type "Int". We also know that this converter has
  ;     been run. Is there a resulting invariant we can make use of? If "x" is "Int", and we see
  ;     an integer comparison operator with quant-var "x" on one side, can we know for sure?

  ;;;;;;;;;;;;;;;
  ; TN question: what should we do for `all i: Int | i >= -8 or i <= 7`? 
  ;   There's no relational operator, no conversion is needed! Can we detect this via the type of the var?
  ;   There might be `sing` and `sum` application in the original AST, though. 
  
  ;;;;;;;;;;;;;;;
  ; TN question: what happens if both sides are relational? E.g., p.father.age > p.age ?
  ;    Right now, I think expr-to-replace is empty, and expr-to-use will throw away all but last match.
  ;   (I think this is a real correctness issue; referred to in TODO above?)

  ;;;;;;;;;;;;;;;
  ; TN question: what happens if we have multiple parallel comparison operators?
  ;    Right now, I think (e.g.)  (p.age >= 18 and p.age <= 100) would generate
  ;    multiple quantifier variables for the Int value of p.age, rather than one.
  ;  (This is just a potential performance issue, right? We don't know if it actually is one.)


  ; TN QUESTION: Why are we removing a layer, here?
  ; It seems to result in changing (int= (sign 5) 1) to (&& (in (sing x) 5) (int= x 1)))
  ; which I initially thought was a typo. Is the problem that the (car ...) is meant to
  ; remove a sing or sum, but is removing sign instead?

  ;; TN QUESTION: can we rely on these always being binary? That is,
  ;   <form> is always a node/formula? with 2 children?

  ; This function is only called for int=, int<, and int>.
  (unless (equal? (length children) 2)
    (raise-forge-error #:msg (format "SMT-LIB conversion reconciling ~a, but unexpected number of arguments to int operator" form)
                       #:context form))
  (define lhs (first children))
  (define rhs (second children))

  ; We are now in an expression context. It is possible that LHS and/or RHS use a
  ; relational->integer operator (sum or card) at any depth.
  ; E.g., add[a.r, 5] = sign[abs[remainder[b.r, c.r]]]
  ;   would need to have both a.r and b.r unwrapped, becoming
  ;   some x_1, x_2, x_3 : Int |
  ;       x_1 = a.r and x_2 = b.r and x_3 = c.r and
  ;       add[x_1, 5] = sign[abs[remainder[x_2, x_3]]]
  ; Because of the arbitrary depth, and potential need to quantify for any number of expressions,
  ; We proceed as follows:
  ;  (1) Gather all relational->integer expressions within the LHS and RHS
  ;  (2) Create one new quantifier variable for each
  ;  (3) Substitute each rel->int expression for its corresponding quantifier variable
  ;  (4) Assemble a quantified formula, over those variables, with domain Int, with body
  ;     a conjunction of (v1 = expr1) and ... and (vk = exprk) and subst(LHS) = SUBST(RHS)
  

  ; 1 = 2 : no unwrapping needed
  ; 1 = sum[relexpr] : unwrap the RHS (after reconciling the sum?)
  ; sum[relexpr] = 1 : unwrap the LHS (after reconciling the sum?)
  ; sum[relexpr] = sum[relexpr] : need to unwrap both LHS and RHS (reconcile sums?)

  ; 1 = add[intexpr, intexpr] : no unwrapping to do, but must descend in case of inner expr
  ; add[intexpr, intexpr] = 1 : no unwrapping to do, but must descend in case of inner expr

  ; (1) Gather all relational->integer expressions within the LHS and RHS
    ; Use the collector for this. 
    ; Collector lambda should return non-int-op nodes, since we're looking for relational->int ops.
    ; 7/17 - We don't want to stop on, or collect, 'sing' nodes
  (define collector-lambda (lambda (n ctxt) (if (and (not (node/expr/op/sing? n)) (not (node/int? n))) n #f)))
    ; The collector also requires a stopping lambda, which should stop at the same condition as when we collect.
    ; This is because supposed we had sign[sum[join[sum x, y]]], we would want to stop at the join,
    ; since recursive descent will have already unwrapped the inner x and y.
  (define stopping-lambda (lambda (n ctxt) (if (or (node/expr/op/sing? n) (node/int? n)) #f #t)))
    ; Context? Not entirely sure
  (define lhs-relational-exprs (collect lhs collector-lambda #:order 'pre-order #:stop stopping-lambda))
  (define rhs-relational-exprs (collect rhs collector-lambda #:order 'pre-order #:stop stopping-lambda))


  (define lhs-quantifiers 
    (for/list ([x lhs-relational-exprs])
      (var (string->symbol (string-append "x_" (symbol->string (gensym)))) #:info info)))
  (define rhs-quantifiers 
    (for/list ([x rhs-relational-exprs])
      (var (string->symbol (string-append "x_" (symbol->string (gensym)))) #:info info)))

  ; (3) Substitute each rel->int expression for its corresponding quantifier variable
  ; Should be a fold, not a for/list
  (define lhs-substituted (if (equal? lhs-relational-exprs '()) lhs
                            (for/fold ([substituted-expr lhs])
                                    ([rel-expr lhs-relational-exprs] [new-quantifier lhs-quantifiers])
                                    (substitute-ambig run-or-state substituted-expr relations atom-names 
                                    quantvars (node/int/op/sum (node-info rel-expr) (list rel-expr)) new-quantifier))))
  (define rhs-substituted (if (equal? rhs-relational-exprs '()) rhs
                            (for/fold ([substituted-expr rhs])
                                    ([rel-expr rhs-relational-exprs] [new-quantifier rhs-quantifiers])
                                    (substitute-ambig run-or-state substituted-expr relations atom-names 
                                    quantvars (node/int/op/sum (node-info rel-expr) (list rel-expr)) new-quantifier))))
  
  (define int-flag true)
  ; if both sides of the equality are equal to the empty list, both the 'in' and '=' case should NOT contain annotated-info. 
  ; this case is added because we need to potentially reconcile integer expressions for those two operators,
  ; but if there are no sub int expressions we just want to return the normal node.
  (if (and (equal? rhs-relational-exprs '()) (equal? lhs-relational-exprs '())) (set! int-flag false) (void))

  ; (4) Assemble a quantified formula, over those variables, with domain Int, with body
  ;    a conjunction of (v1 = expr1) and ... and (vk = exprk) and subst(LHS) (operator) SUBST(RHS)
  (define lhs-equality-formulas (for/list ([lhs-quant lhs-quantifiers] [lhs-expr lhs-relational-exprs])
                            (node/formula/op/= info (list lhs-quant lhs-expr))))
  (define rhs-equality-formulas (for/list ([rhs-quant rhs-quantifiers] [rhs-expr rhs-relational-exprs])
                            (node/formula/op/= info (list rhs-quant rhs-expr))))
  
  (define new-fmla 
    (match form 
      [(? node/formula/op/int<? form) (node/formula/op/int< (if int-flag annotated-info info) (list lhs-substituted rhs-substituted))]
      [(? node/formula/op/int=? form) (node/formula/op/int= (if int-flag annotated-info info) (list lhs-substituted rhs-substituted))]
      [(? node/formula/op/int>? form) (node/formula/op/int> (if int-flag annotated-info info) (list lhs-substituted rhs-substituted))]
      [(? node/formula/op/=? form) (node/formula/op/= (if int-flag annotated-info info) (list lhs-substituted rhs-substituted))]
      [(? node/formula/op/in? form) (node/formula/op/in (if int-flag annotated-info info) (list lhs-substituted rhs-substituted))]
    )
  )

  (define quantifiers (append lhs-quantifiers rhs-quantifiers))
  (define var-int-pairs (for/list ([quant quantifiers])
                          (cons quant Int)))

  ; if var-int-pairs is empty, just return new-fmla
  (if (equal? var-int-pairs '())
    new-fmla
    (node/formula/quantified info 'some var-int-pairs 
      (node/formula/op/&& info (cons new-fmla (append lhs-equality-formulas rhs-equality-formulas))))
  )
)

(define (interpret-formula-op run-or-state formula relations atom-names quantvars args)
  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "integer-converter: interpret-formula-op: ~a~n" formula))
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
      (reconcile-integer-expr run-or-state args relations atom-names quantvars info (node/formula/op/int> info children))]
    [(node/formula/op/int< info children)
      (reconcile-integer-expr run-or-state args relations atom-names quantvars info (node/formula/op/int< info children))]
    [(node/formula/op/int= info children)
     (reconcile-integer-expr run-or-state args relations atom-names quantvars info (node/formula/op/int= info children))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Relational expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interpret-expr run-or-state expr relations atom-names quantvars)
  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
      (printf "integer-converter: interpret-expr: ~a~n" expr))
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
         (define new-quantvars (cons (car decl) curr-quantvars))
         (define new-decl-domain (interpret-expr run-or-state (cdr decl) relations atom-names new-quantvars))
         (define new-decls (cons (cons (car decl) new-decl-domain) curr-decls))
         (list new-quantvars new-decls)))
     (define new-quantvars (first new-vs-and-decls))
     (let ([processed-form (interpret-formula run-or-state form relations atom-names new-quantvars)])
       (define new-decls (second new-vs-and-decls))
     (node/expr/comprehension info len new-decls processed-form))]))

(define (interpret-expr-op run-or-state expr relations atom-names quantvars args)
    (when (@>= (get-verbosity) VERBOSITY_DEBUG)
      (printf "integer-converter: interpret-expr-op: ~a~n" expr))
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
    (printf "integer-converter: interpret-int: ~a~n" expr))
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
    (printf "integer-converter: interpret-int-op: ~a~n" expr))
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