#lang racket/base

; This file is intended to take in the Forge AST and return a (list of) string(s) in the 
; SMTlibv2 file, which are intended to be ran with CVC5.

(require 
  forge/sigs-structs
  forge/lang/ast
  forge/shared
  forge/solver-specific/smtlib-shared
  forge/last-checker
  forge/lang/bounds
  (only-in racket index-of match string-join first second rest flatten last drop-right third)
  (only-in racket/contract define/contract or/c listof any/c)
  (prefix-in @ (only-in racket/contract ->))
  (prefix-in @ (only-in racket/base >= > -)))

(provide convert-formula)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Boolean formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Translate a formula AST node
(define/contract (convert-formula run-or-state formula relations atom-names quantvars quantvar-types bounds)  
  (@-> (or/c Run? State? Run-spec?)
      node/formula?
      list?
      list?
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
     (convert-formula run-or-state expanded relations atom-names quantvars quantvar-types bounds)]
    [(node/formula/op info args)
     (convert-formula-op run-or-state formula relations atom-names quantvars quantvar-types args bounds)]
    [(node/formula/multiplicity info mult expr)
    (let ([processed-expr (convert-expr run-or-state expr relations atom-names quantvars quantvar-types bounds)])
      (match mult
        ; crucially, we can't use 'processed-expr' in the get-k-bounds - we have to use the preprocessed so it has nodes, not strings
        ['no (format "(= (as set.empty ~a) ~a)" (get-k-bounds run-or-state expr quantvars quantvar-types) processed-expr)]
        ['one (format-one run-or-state expr quantvars quantvar-types processed-expr bounds)]
        ['some (format "(not (= (as set.empty ~a) ~a))"  (get-k-bounds run-or-state expr quantvars quantvar-types) processed-expr)]
        ['lone (format "(or ~a (= (as set.empty ~a) ~a))" (format-one run-or-state expr quantvars quantvar-types processed-expr bounds) 
                                                        (get-k-bounds run-or-state expr quantvars quantvar-types) processed-expr)]
        [else (raise-forge-error #:msg "SMT backend does not support this multiplicity.")]))]
    [(node/formula/quantified info quantifier decls form)
     ; new vs-decls => list ((list qv) (list (pair qv expr-decl)))
     (define new-vs-decls
       (for/fold ([vs-decls (list quantvars '())])
                 ([decl decls])
         (define curr-quantvars (first vs-decls))
         (define curr-expr-decls (second vs-decls))
         (define new-quantvars (cons (car decl) curr-quantvars))
         (define new-expr-decls (cons decl curr-expr-decls))
         (list new-quantvars new-expr-decls)))
      (define new-quantvars  (first new-vs-decls))
      (define new-expr-decls (second new-vs-decls))
      (define new-quantvar-types (map cdr new-expr-decls))
     (let ([processed-form (convert-formula run-or-state form relations atom-names new-quantvars new-quantvar-types bounds)])
       ; In the quantifier declaration, we need the *sort* name, which is Atom or Int
       ; In the guard, we need a SMT *expression*, which is the sort name or the Int-universe expression.
       ; To get the string expression, we map convert-expr to the cdr of each decl.
       (define vars-str-decls (map (lambda (x) (cons (car x) (convert-expr run-or-state (cdr x) relations atom-names new-quantvars new-quantvar-types bounds))) new-expr-decls))
       (printf "vars-str-decls: ~a~n" vars-str-decls)
       (format "(~a (~a) ~a)"
                         ; SMT-LIB uses "forall", not "all" and "exists", not "some"
                         (if (equal? quantifier 'all) "forall" "exists")
                         (string-join (map (lambda (x) (format "(~a ~a)" (car x) (atom-or-int (cdr x)))) new-expr-decls) " ")
                         (if (equal? quantifier 'all) (format "(=> ~a ~a)" (membership-guard vars-str-decls) processed-form)
                             (format "(and ~a ~a)" (membership-guard vars-str-decls) processed-form))))]
    [(node/formula/sealed info)
     (node/formula/sealed info)]
    [#t "true"]
    [#f "false"]
    ))

(define (get-k-bounds run-or-state expr quantvars quantvar-types)
  (define quantvar-pairs (map list quantvars quantvar-types))
  (define dummy-hash (make-hash))
  (define list-of-types (expression-type-type (checkExpression run-or-state expr quantvar-pairs dummy-hash)))
  (define top-level-type-list (for/list ([type list-of-types])
    (if (index-of type 'Int) "Int" "Atom")))
  (format "(Relation ~a)" (string-join top-level-type-list " "))
)

(define (format-one run-or-state expr quantvars quantvar-types processed-expr bounds)
  ; anticipate the type of the expr with last checker
  (define quantvar-pairs (map list quantvars quantvar-types))
  (define dummy-hash (make-hash))
  (define list-of-types (expression-type-type (checkExpression run-or-state expr quantvar-pairs dummy-hash)))
  ; map each type to the respective upper bound
  (define nested-atoms-to-use (for/list ([bound bounds])
    (for/list ([type-union list-of-types])
      (for/list ([type type-union] #:when (and (not (equal? type 'Int)) (equal? (symbol->string type) (relation-name (bound-relation bound)))))
          (bound-upper bound)
      )
    )
  ))
  ; probably want to remove duplicates but for now it's fine
  (define atoms-to-use (flatten nested-atoms-to-use))
  ; if we have an Int somewhere in the list, we don't want to enumerate the ints.
  ; let's use a working example to figure out how we want to do this.
  ; if we have F : A -> one Int, 
  ; we want to check that the domain is only in A. 
  ; so we should check that the domain is a subset of A.
  ; but to do that we have to right join the relation :/ 
  ; give me one second to check on ints.
  (if (equal? (length atoms-to-use) 0)
    ; temporary patch - just don't enumerate anything if it only contains ints
    (format "true")
    (format "(or ~a)" (string-join (map (lambda (x) (format "(= ~a (set.singleton (tuple ~a)))" processed-expr x)) atoms-to-use) " "))
  )
)

(define (process-children-formula run-or-state children relations atom-names quantvars quantvar-types bounds)
  (map (lambda (x) (convert-formula run-or-state x relations atom-names quantvars quantvar-types bounds)) children))

(define (process-children-expr run-or-state children relations atom-names quantvars quantvar-types bounds)
  (map (lambda (x) (convert-expr run-or-state x relations atom-names quantvars quantvar-types bounds)) children))

(define (process-children-int run-or-state children relations atom-names quantvars quantvar-types bounds)
  (map (lambda (x) (convert-int run-or-state x relations atom-names quantvars quantvar-types bounds)) children))

(define (process-children-ambiguous run-or-state children relations atom-names quantvars quantvar-types bounds)
  (for/list ([child children])
    (match child
      [(? node/formula? f) (convert-formula run-or-state f relations atom-names quantvars quantvar-types bounds)]
      [(? node/expr? e) (convert-expr run-or-state e relations atom-names quantvars quantvar-types bounds)]
      [(? node/int? i) (convert-int run-or-state i relations atom-names quantvars quantvar-types bounds)])))

(define (convert-formula-op run-or-state formula relations atom-names quantvars quantvar-types args bounds)
  (when (@>= (get-verbosity) 2)
    (printf "to-smtlib-tor: convert-formula-op: ~a~n" formula))
  (match formula
    [(node/formula/op/&& info children)
      (format "(and ~a)" (string-join (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds) " "))]
    [(node/formula/op/|| info children)
     (format "(or ~a)" (string-join (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds) " "))]
    [(node/formula/op/=> info children)
     (format "(=> ~a)" (string-join (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds) " "))]

    [(node/formula/op/always info children)
     (raise-forge-error #:msg "Temporal operators are unsupported by SMT backend."
                        #:context formula)]
    [(node/formula/op/eventually info children)
     (raise-forge-error #:msg "Temporal operators are unsupported by SMT backend."
                        #:context formula)]
    [(node/formula/op/next_state info children)
      (raise-forge-error #:msg "Temporal operators are unsupported by SMT backend."
                        #:context formula)]
    [(node/formula/op/releases info children)
      (raise-forge-error #:msg "Temporal operators are unsupported by SMT backend."
                        #:context formula)]
    [(node/formula/op/until info children)
     (raise-forge-error #:msg "Temporal operators are unsupported by SMT backend."
                        #:context formula)]
    [(node/formula/op/historically info children)
      (raise-forge-error #:msg "Temporal operators are unsupported by SMT backend."
                        #:context formula)]
    [(node/formula/op/once info children)
      (raise-forge-error #:msg "Temporal operators are unsupported by SMT backend."
                        #:context formula)]
    [(node/formula/op/prev_state info children)
      (raise-forge-error #:msg "Temporal operators are unsupported by SMT backend."
                        #:context formula)]
    [(node/formula/op/since info children)
      (raise-forge-error #:msg "Temporal operators are unsupported by SMT backend."
                        #:context formula)]
    [(node/formula/op/triggered info children)
      (raise-forge-error #:msg "Temporal operators are unsupported by SMT backend."
                        #:context formula)]

    [(node/formula/op/in info children)
      (format "(set.subset ~a)" (string-join (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds) " "))]
    [(node/formula/op/= info children)
      (format "(= ~a)" (string-join (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds) " "))]
    [(node/formula/op/! info children)
      (format "(not ~a)" (string-join (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds) ""))]
    [(node/formula/op/int> info children)
      (format "(> ~a)" (string-join (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds)))]
    [(node/formula/op/int< info children)
      (format "(< ~a)" (string-join (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds)))]
    [(node/formula/op/int= info children)
     (format "(= ~a)" (string-join (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Relational expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (deparen lst)
  (string-join (map (lambda (x) (format "~a" x)) lst) " "))

(define (check-skolem-type run-or-state expr relations atom-names quantvars quantvar-types bounds)
  (for/or ([bound bounds])
    (if (equal? (bound-relation bound) expr)
      (cond [(equal? (car ((relation-typelist-thunk (bound-relation bound)))) 'Int) 
              (format "~a" (node/expr/relation-name expr))] 
            [else (format "(set.singleton (tuple ~a))" (node/expr/relation-name expr))])
      #f
    )
  )
)

(define (convert-expr run-or-state expr relations atom-names quantvars quantvar-types bounds)
  (when (@>= (get-verbosity) 2)
      (printf "to-smtlib-tor: convert-expr: ~a~n" expr))
  (match expr
    [(node/expr/relation info arity name typelist-thunk parent isvar)
     ; Declared sigs are referred to as Atoms, so we refer to them as such
     ; Ints are separate
     (cond [(equal? name "Int") "(as set.universe (Relation Int))"]
           ; Skolem relations are now handled in the fun-spacer case; this must be
           ; a normal relation that, somehow, starts with $. So fail noisily; this shouldn't
           ; actually be reachable code.
           [(equal? #\$ (string-ref name 0))
            (raise-forge-error #:msg (format "Unexpected reachable code; untagged skolem relation: ~a" expr)
                               #:context expr)]
           [else (format "~a" name)])]
    [(node/expr/atom info arity name)
     (format "(set.singleton (tuple ~a))" name)]
    [(node/expr/fun-spacer info arity name args result expanded)
     ; "arity" will always be 1, since this represents a Skolem function.
     ; Use the expanded expr's arity instead:
     (cond [(and (@> (node/expr-arity expanded) 1) (equal? #\$ (string-ref (symbol->string name) 0)))
            ; This marker is to aid in recognizing the application of a Skolem function.
            ; E.g., to convert something like b.a.$x into ($x a b).
            (define components (join->list/right expanded))
            (unless (and (node/expr/relation? (last components))
                         (equal? (node/expr/relation-name (last components)) (symbol->string name)))
              (raise-forge-error #:msg (format "Skolem (function) fun-spacer marker did not match: ~a vs. ~a" (last components) name)
                                 #:context info))
            ;; replace (note: the arguments must be variables)
            (for ([a (drop-right components 1)])
              (unless (node/expr/quantifier-var? a)
                (raise-forge-error #:msg (format "Unexpected argument to Skolem function; was not a variable: ~a" a)
                                   #:context a)))
            ; TODO: if used in an int context, do we want to wrap still?
            (format "(set.singleton (tuple (~a ~a)))" name (deparen (drop-right components 1)))]
           [(equal? #\$ (string-ref (symbol->string name) 0))
            (unless (and (node/expr/relation? expanded)
                         (equal? (node/expr/relation-name expanded) (symbol->string name)))
              (raise-forge-error #:msg (format "Skolem (constant) fun-spacer marker did not match: ~a vs. ~a" expanded name)
                                 #:context info))
            (format "(set.singleton (tuple ~a))" name)
            ;[(equal? #\$ (string-ref name 0)) (check-skolem-type run-or-state expr relations atom-names quantvars quantvar-types bounds)]
            ]
           [else
            ; This is not a Skolem function; continue as normal.
            (convert-expr run-or-state expanded relations atom-names quantvars quantvar-types bounds)])]
    [(node/expr/ite info arity a b c)  
    (let ([processed-a (convert-formula run-or-state a relations atom-names quantvars quantvar-types bounds)]
          [processed-b (convert-expr run-or-state b relations atom-names quantvars quantvar-types bounds)]
          [processed-c (convert-expr run-or-state c relations atom-names quantvars quantvar-types bounds)])
     (format "(ite ~a ~a ~a)" processed-a processed-b processed-c))]
    [(node/expr/constant info 1 'Int)
     (raise-forge-error #:msg "Unexpected node reached by to-smtlib-tor: node/expr/constant with 'Int"
                        #:context info)]
    ; univ, iden, none
    [(node/expr/constant info arity 'univ)
     "TODO: universe -- atoms + ints"]
    [(node/expr/constant info arity 'iden)
     "TODO: identity relation"]
    [(node/expr/constant info arity 'none)
     ; produce an empty set of the appropriate arity
     (for/fold ([acc "set.empty"])
               ([todo (build-list (@- arity 1) (lambda (x) x))])
       (format "(rel.product set.empty ~a)" acc))]
    [(node/expr/constant info arity type)
     (raise-forge-error #:msg (format "Unexpected node reached by to-smtlib-tor: node/expr/constant with type " type)
                        #:context info)]
    [(node/expr/op info arity args)
     (convert-expr-op run-or-state expr relations atom-names quantvars quantvar-types args bounds)]
    [(node/expr/quantifier-var info arity sym name)
     ; If this is an integer-unwrapping quantifier variable, it will be declared of sort Int,
     ; and should not be wrapped to make it a singleton-set-of-tuples. Otherwise, it must be
     ; wrapped so that relational operators can work with it directly.
     (if (get-annotation info 'smt/int-unwrap)
         (format "~a" name)
         (format "(set.singleton (tuple ~a))" name))]
    [(node/expr/comprehension info len decls form)   
     (define new-vs-decls
       (for/fold ([vs-decls (list quantvars '())])
                 ([decl decls])
         (define curr-quantvars (first vs-decls))
         (define curr-expr-decls (second vs-decls))
         (define new-quantvars (cons (car decl) quantvars))
         (define new-expr-decls (cons decl curr-expr-decls))
         (list new-quantvars new-expr-decls)))
      (define new-quantvars  (first new-vs-decls))
      (define new-expr-decls (second new-vs-decls))
      (define new-quantvar-types (map cdr new-expr-decls))
     (let ([processed-form (convert-formula run-or-state form relations atom-names new-quantvars new-quantvar-types bounds)])
       (define new-decls (second new-vs-decls))
     "TODO: COMPREHENSION")]))

(define (convert-expr-op run-or-state expr relations atom-names quantvars quantvar-types args bounds)
    (when (@>= (get-verbosity) 2)
      (printf "to-smtlib-tor: convert-expr-op: ~a~n" expr))
  (match expr
    [(node/expr/op/+ info arity children)
     (format "(set.union ~a)" (string-join (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds) " "))]
    [(node/expr/op/- info arity children)
     (format "(set.minus ~a)" (string-join (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds) " "))]
    [(node/expr/op/& info arity children)
     (format "(set.inter ~a)" (string-join (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds) " "))]
    [(node/expr/op/-> info arity children)
     ; rel.product in CVC5 is _binary_, not nary, so need to chain this.
     (define child-strings (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds))
     (define rest-string (for/fold ([acc (second child-strings)])
                                   ([str (rest (rest child-strings))])
                           (format "(rel.product ~a ~a)" acc str)))
     (format "(rel.product ~a ~a)" (first child-strings) rest-string)]
    [(node/expr/op/prime info arity children)
     (raise-forge-error #:msg "Temporal operators are unsupported by SMT backend."
                        #:context expr)]
    [(node/expr/op/join info arity children)
     (format "(rel.join ~a)" (string-join (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds) " "))]
    [(node/expr/op/^ info arity children)
     (format "(rel.tclosure ~a)" (string-join (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds) " "))]
    [(node/expr/op/* info arity children)
      "TODO: reflexive tclosure"]
    [(node/expr/op/~ info arity children)
     (format "(rel.transpose ~a)" (string-join (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds) " "))]
    [(node/expr/op/++ info arity children)
     (raise-forge-error #:msg "Relational update currently unsupported by SMT-LIB translation in Forge."
                        #:context info)]
    [(node/expr/op/sing info arity children)
     (format "(set.singleton (tuple ~a))" (string-join (process-children-int run-or-state args relations atom-names quantvars quantvar-types bounds) " "))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Integer expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (convert-int run-or-state expr relations atom-names quantvars quantvar-types bounds)
  (when (@>= (get-verbosity) 2)
    (printf "to-smtlib-tor: convert-int: ~a~n" expr))
  (match expr
    [(node/int/constant info value)
     (format "~a" value)]
    [(node/int/op info args)
     (convert-int-op run-or-state expr relations atom-names quantvars quantvar-types args bounds)]
    [(node/int/sum-quant info decls int-expr)
    (define new-vs-decls
       (for/fold ([vs-decls (list quantvars '())])
                 ([decl decls])
         (define curr-quantvars (first vs-decls))
         (define curr-expr-decls (second vs-decls))
         (define new-quantvars (cons (car decl) curr-quantvars))
         (define new-expr-decls (cons decl curr-expr-decls))
         (list new-quantvars new-expr-decls)))
      (define new-quantvars  (first new-vs-decls))
      (define new-expr-decls (second new-vs-decls))
      (define new-quantvar-types (map cdr new-expr-decls))
     (let ([processed-form (convert-formula run-or-state int-expr relations atom-names new-quantvars new-quantvar-types bounds)])
       (define new-decls (second new-vs-decls))
      "TODO: sum quant")]))

(define (convert-int-op run-or-state expr relations atom-names quantvars quantvar-types args bounds)
  (when (@>= (get-verbosity) 2)
    (printf "to-smtlib-tor: convert-int-op: ~a~n" expr))
  (match expr
    [(node/int/op/add info children)
      (format "(+ ~a)" (string-join (process-children-int run-or-state args relations atom-names quantvars quantvar-types bounds) " "))]
    [(node/int/op/subtract info children)
      (format "(- ~a)" (string-join (process-children-int run-or-state args relations atom-names quantvars quantvar-types bounds) " "))]
    [(node/int/op/multiply info children)
      (format "(* ~a)" (string-join (process-children-int run-or-state args relations atom-names quantvars quantvar-types bounds) " "))]
    [(node/int/op/divide info children)
      (format "(div ~a)" (string-join (process-children-int run-or-state args relations atom-names quantvars quantvar-types bounds) " "))]
    [(node/int/op/sum info children)
    ; if we can discover that the argument is provably a singleton, we can just erase the 'sum' node
    ; what in the world do we do in the other case? probably either be inspired by cvc5 work or fortress translation
    "TODO: sum"]
    [(node/int/op/card info children)
    "TODO: cardinality"]
    [(node/int/op/remainder info children)
     (format "(mod ~a)" (string-join (process-children-int run-or-state args relations atom-names quantvars quantvar-types bounds) " "))]
    [(node/int/op/abs info children)
     (format "(abs ~a)" (string-join (process-children-int run-or-state args relations atom-names quantvars quantvar-types bounds) " "))]
    [(node/int/op/sign info children)
     ; The Forge->SMT-LIB generator preamble defines a function "sign"
     (format "(sign ~a)")]
    [(node/int/sum-quant info decls int-expr)
     (raise-forge-error #:msg "Reached expected unreachable code." #:context expr)]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Convert a right-associating join to a list. If the join is not of that form,
; throw an error. 
(define (join->list/right expr)
  (match expr
    [(node/expr/relation info arity name typelist-thunk parent is-var)
     (list expr)]
    [(node/expr/op/join info 2 children)
     (cons (first children) (join->list/right (second children)))]
    [else (raise-forge-error #:msg (format "join->list/right expected relation or right-chain of binary joins, got ~a" expr)
                             #:context expr)]))
