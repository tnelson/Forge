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
  (only-in racket index-of match string-join first second rest flatten last drop-right)
  (only-in racket/contract define/contract or/c listof any/c)
  (prefix-in @ (only-in racket/contract ->))
  (prefix-in @ (only-in racket/base >= >)))

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
        ['no (format "(= (as set.empty ~a) ~a))" (get-k-bounds run-or-state expr quantvars quantvar-types) processed-expr)]
        ['one (format-one run-or-state expr quantvars quantvar-types processed-expr bounds)]
        ['some (format "(not (= (as set.empty ~a) ~a)"  (get-k-bounds run-or-state expr quantvars quantvar-types) processed-expr)]
        ['all (format "(= set.universe ~a)" processed-expr)]
        ['lone (format "(or ~a (= (as set.empty ~a) ~a))" (format-one run-or-state expr quantvars quantvar-types processed-expr bounds) 
                                                        (get-k-bounds run-or-state expr quantvars quantvar-types) processed-expr)]
        [else (raise-forge-error #:msg "SMT backend does not support this multiplicity.")]))]
     ; I think these require specific cases... not sure which ones get desugared
      ;"TODO: MULTIPLICITY")]
    [(node/formula/quantified info quantifier decls form)
     (define new-vs-decls-types
       (for/fold ([vs-decls-types (list quantvars '() quantvar-types)])
                 ([decl decls])
         (define curr-quantvars (first vs-decls-types))
         (define curr-decls (second vs-decls-types))
         (define new-quantvars (cons (car decl) quantvars))
         (define new-decl-domain (convert-expr run-or-state (cdr decl) relations atom-names new-quantvars quantvar-types bounds))
         (define new-decls (cons (cons (car decl) new-decl-domain) curr-decls))
         (define new-quantvar-types (cons (cdr decl) quantvar-types))
         (list new-quantvars new-decls new-quantvar-types)))
      (define new-quantvars (list-ref new-vs-decls-types 0))
      (define new-quantvar-types (list-ref new-vs-decls-types 2))
     (let ([processed-form (convert-formula run-or-state form relations atom-names new-quantvars new-quantvar-types bounds)])
       (define new-decls (second new-vs-decls-types))
       (format "(~a (~a) ~a)"
               ; SMT-LIB uses "forall", not "all" and "exists", not "some"
               (if (equal? quantifier 'all) "forall" "exists")
               (string-join (map (lambda (x) (format "(~a ~a)" (car x) (atom-or-int (cdr x)))) new-decls) " ")
               ; insert guard predicates here
               (if (equal? quantifier 'all) (format "(=> ~a ~a)" (membership-guard decls) processed-form)
                                            (format "(and ~a ~a)" (membership-guard decls) processed-form))))]
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
      (for/list ([type type-union] #:when (equal? (symbol->string type) (relation-name (bound-relation bound))))
          (bound-upper bound)
      )
    )
  ))
  ; probably want to remove duplicates but for now it's fine
  (define atoms-to-use (flatten nested-atoms-to-use))
  (format "(or ~a)" (string-join (map (lambda (x) (format "(= ~a (set.singleton (tuple ~a)))" processed-expr x)) atoms-to-use)))
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

(define (convert-expr run-or-state expr relations atom-names quantvars quantvar-types bounds)
  (when (@>= (get-verbosity) 2)
      (printf "to-smtlib-tor: convert-expr: ~a~n" expr))
  (match expr
    [(node/expr/relation info arity name typelist-thunk parent isvar)
     ; Declared sigs are referred to as Atoms, so we refer to them as such
     ; Ints are separate
     (cond [(equal? name "Int") "(as set.universe (Relation Int))"]
           ; Assuming tagging fun-spacers are being added, this should not be reached:
           [(equal? #\$ (string-ref name 0)) (format "(set.singleton (tuple ~a))" name)]
           [else (format "~a" name)])]
    [(node/expr/atom info arity name)
     (raise-forge-error #:msg (format "direct atom references unsupported by SMT backend")
                        #:context expr)]
    [(node/expr/fun-spacer info arity name args result expanded)
     ; "arity" will always be 1, since this represents a Skolem function.
     ; Use the expanded expr's arity instead:
     (cond [(and (@> (node/expr-arity expanded) 1) (equal? #\$ (string-ref (symbol->string name) 0)))
            ; This marker is to aid in recognizing the application of a Skolem function.
            ; E.g., to convert something like b.a.$x into ($x a b).
            (define components (join->list/right expanded))
            (unless (and (node/expr/relation? (last components))
                         (equal? (node/expr/relation-name (last components)) (symbol->string name)))
              (raise-forge-error #:msg (format "fun-spacer marker did not match: ~a vs. ~a" (last components) name)
                                 #:context info))
            ;; replace (note: the arguments must be variables)
            (for ([a (drop-right components 1)])
              (unless (node/expr/quantifier-var? a)
                (raise-forge-error #:msg (format "Unexpected argument to Skolem function; was not a variable: ~a" a)
                                   #:context a)))
            ; TODO: if used in an int context, do we want to wrap still?
            (format "(set.singleton (tuple (~a ~a)))" name (deparen (drop-right components 1)))]
           [else
            ; This is either not a Skolem function, or a *nullary* Skolem function, which can be
            ; treated as a normal expression.
            (convert-expr run-or-state expanded relations atom-names quantvars quantvar-types bounds)])]
    [(node/expr/ite info arity a b c)  
    (let ([processed-a (convert-formula run-or-state a relations atom-names quantvars quantvar-types bounds)]
          [processed-b (convert-expr run-or-state b relations atom-names quantvars quantvar-types bounds)]
          [processed-c (convert-expr run-or-state c relations atom-names quantvars quantvar-types bounds)])
     (format "(ite ~a ~a ~a)" processed-a processed-b processed-c))]
    [(node/expr/constant info 1 'Int)
     ; TODO: Is this production being used? 'Int is a symbol, not expanded to the Int sig?
     "TODO: EXPR CONSTANT? IDK WHAT THIS IS"]
    [(node/expr/constant info arity type)
     "TODO: ANOTHER EXPR CONSTANT? IDK WHAT THIS IS"]
    [(node/expr/op info arity args)
     (convert-expr-op run-or-state expr relations atom-names quantvars quantvar-types args bounds)]
    [(node/expr/quantifier-var info arity sym name)  
     (format "(set.singleton (tuple ~a))" name)]
    [(node/expr/comprehension info len decls form)   
     (define new-vs-and-decls
       (for/fold ([vs-and-decls (list quantvars '())])
                 ([decl decls])
         (define curr-quantvars (first vs-and-decls))
         (define curr-decls (second vs-and-decls))
         (define new-quantvars (cons (car decl) quantvars))
         (define new-decl-domain (convert-expr run-or-state (cdr decl) relations atom-names new-quantvars quantvar-types bounds))
         (define new-decls (cons (cons (car decl) new-decl-domain) curr-decls))
         (list new-quantvars new-decls)))
     (define new-quantvars (first new-vs-and-decls))
     (let ([processed-form (convert-formula run-or-state form relations atom-names new-quantvars quantvar-types bounds)])
       (define new-decls (second new-vs-and-decls))
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
     "TODO: ++ (idk what that is)"]
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
     (define new-vs-and-decls
       (for/fold ([vs-and-decls (list quantvars '())])
                 ([decl decls])
         (define curr-quantvars (first vs-and-decls))
         (define curr-decls (second vs-and-decls))
         (define new-quantvars (cons (car decl) quantvars))
         (define new-decl-domain (convert-expr run-or-state (cdr decl) relations atom-names new-quantvars quantvar-types bounds))
         (define new-decls (cons (cons (car decl) new-decl-domain) curr-decls))
         (list new-quantvars new-decls)))
     (define new-quantvars (first new-vs-and-decls))
     (let ([processed-int (convert-int run-or-state int-expr relations atom-names new-quantvars quantvar-types bounds)])
       (define new-decls (second new-vs-and-decls))
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
    "TODO: sum"]
    [(node/int/op/card info children)
    "TODO: cardinality"]
    [(node/int/op/remainder info children)
     (format "(mod ~a)" (string-join (process-children-int run-or-state args relations atom-names quantvars quantvar-types bounds) " "))]
    [(node/int/op/abs info children)
     (format "(abs ~a)" (string-join (process-children-int run-or-state args relations atom-names quantvars quantvar-types bounds) " "))]
    [(node/int/op/sign info children)
     "TODO: sign"]
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