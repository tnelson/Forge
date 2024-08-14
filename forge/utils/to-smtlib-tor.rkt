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
  forge/utils/collector
  (only-in racket index-of match string-join first second rest flatten last drop-right third empty remove-duplicates empty? filter-map member)
  (only-in racket/contract define/contract or/c listof any/c)
  (prefix-in @ (only-in racket/contract -> ->*))
  (prefix-in @ (only-in racket/base >= > - + <)))

(provide convert-formula get-new-top-level-strings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Boolean formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define new-top-level-strings '())

(define (get-new-top-level-strings)
  new-top-level-strings
)

; Translate a formula AST node
(define/contract (convert-formula run-or-state formula relations atom-names quantvars quantvar-types bounds [int-ctxt #f])  
  (@->* ((or/c Run? State? Run-spec?)
      node/formula?
      list?
      list?
      list?
      list?
      list?)
     (boolean?)
      string?)
  (defensive-checks "convert-formula" run-or-state formula relations atom-names quantvars quantvar-types '() bounds) 
  (when (@> (get-verbosity) VERBOSITY_LOW)
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
       ['no (format "(= (as set.empty ~a) ~a)" (get-k-bounds run-or-state expr quantvars quantvar-types) processed-expr)]
        ; ['one (format "(and (forall ((x1 ~a) (x2 ~a)) (=> (and (set.subset (set.singleton (tuple x1)) ~a) (set.subset (set.singleton (tuple x2)) ~a)) (= x1 x2))) (exists ((x1 ~a)) (set.subset (set.singleton (tuple x1)) ~a)))"
        ;       (get-expr-type run-or-state expr quantvars quantvar-types)
        ;       (get-expr-type run-or-state expr quantvars quantvar-types) processed-expr processed-expr
        ;       (get-expr-type run-or-state expr quantvars quantvar-types) processed-expr)]
        ['one (format "(and ~a ~a)" (format-one-forall run-or-state expr quantvars quantvar-types processed-expr)
                                     (format-one-exists run-or-state expr quantvars quantvar-types processed-expr))]
        ['some (format "(not (= (as set.empty ~a) ~a))"  (get-k-bounds run-or-state expr quantvars quantvar-types) processed-expr)]
        ['lone (format-one-forall run-or-state expr quantvars quantvar-types processed-expr)]
        [else (raise-forge-error #:msg "SMT backend does not support this multiplicity.")]))]
    [(node/formula/quantified info quantifier decls form)

     (define newly-defined-vars (map car decls))
     (define updated-quantvars (append newly-defined-vars quantvars))
     (define updated-quantvar-types (append decls quantvar-types))
     
     (let ([processed-form (convert-formula run-or-state form relations atom-names updated-quantvars updated-quantvar-types bounds)])
       ; In the quantifier declaration, we need the *sort* name, which is Atom or IntAtom
       ; In the guard, we need a SMT *expression*, which is the sort name or the Int-universe expression.
       ; To get the string expression, we map convert-expr to the cdr of each decl.
       (define vars-str-decls (map (lambda (x) (cons (car x) (convert-expr run-or-state (cdr x) relations atom-names updated-quantvars updated-quantvar-types bounds)))
                                   decls))
       (format "(~a (~a) ~a)"
               ; SMT-LIB uses "forall", not "all" and "exists", not "some"
               (if (equal? quantifier 'all) "forall" "exists")
               (string-join (map (lambda (x) (format "(~a ~a)" (car x) (atom-or-int (cdr x))))
                                 decls) " ")
               (if (equal? quantifier 'all)
                   (format "(=> ~a ~a)" (membership-guard vars-str-decls) processed-form)
                   (format "(and ~a ~a)" (membership-guard vars-str-decls) processed-form))))]
    [(node/formula/sealed info)
     (node/formula/sealed info)]
    [#t "true"]
    [#f "false"]
    ))

(define (pair->list p) (list (car p) (cdr p)))

(define (format-one-forall run-or-state expr quantvars quantvar-types processed-expr)
  (define list-of-types (expression-type-top-level-types (checkExpression run-or-state expr quantvar-types (make-hash))))
  (define new-quantvars (for/list ([i (in-range (@+ (length list-of-types) (length list-of-types)))])
    (format "x_~a" i)))
  (define new-decls (for/list ([var new-quantvars] [type (append list-of-types list-of-types)])
    (format "(~a ~a)" var (if (equal? type 'Int) "IntAtom" "Atom"))))
  (define subset-constraints-first
    (format "(set.subset (set.singleton (tuple ~a)) ~a)" 
      (string-join 
        (for/list ([i (in-range (length list-of-types))])
          (format "~a" (list-ref new-quantvars i))) 
      " ") 
      processed-expr))
  (define subset-constraints-second
    (format "(set.subset (set.singleton (tuple ~a)) ~a)" 
      (string-join 
        (for/list ([i (in-range (length list-of-types))])
          (format "~a" (list-ref new-quantvars (@+ i (length list-of-types))))) 
      " ") 
      processed-expr))
  (define pairwise-equality-constraints 
    (string-join (for/list ([i (in-range (length list-of-types))])
      (format "(= ~a ~a)" 
        (list-ref new-quantvars i) 
        (list-ref new-quantvars (@+ i (length list-of-types))))) " ")
  )
  (format "(forall ~a (=> (and ~a ~a) (and ~a)))" new-decls subset-constraints-first subset-constraints-second pairwise-equality-constraints)
)

(define (format-one-exists run-or-state expr quantvars quantvar-types processed-expr)
  (define list-of-types (expression-type-top-level-types (checkExpression run-or-state expr quantvar-types (make-hash))))
  (define new-quantvars (for/list ([i (in-range (length list-of-types))]) (format "x_~a" i)))
  (define new-decls (for/list ([var new-quantvars] [type list-of-types]) (format "(~a ~a)" var (if (equal? type 'Int) "IntAtom" "Atom"))))
  (define subset-constraint (format "(set.subset (set.singleton (tuple ~a)) ~a)" (string-join new-quantvars " ") processed-expr))
  (format "(exists ~a ~a)" new-decls subset-constraint)
)

(define (get-expr-type run-or-state expr quantvars quantvar-types)
 (define quantvar-pairs (map pair->list quantvar-types))
  (define dummy-hash (make-hash))
  (printf "expr: ~a~n" expr)
  (define list-of-types (expression-type-top-level-types (checkExpression run-or-state expr quantvar-pairs dummy-hash)))
  (printf "list-of-types: ~a~n" list-of-types)
  (define top-level-type-list (for/list ([type list-of-types])
    (if (equal? type 'Int) "IntAtom" "Atom")))
    (format "~a" (string-join top-level-type-list " "))
)

(define (get-k-bounds run-or-state expr quantvars quantvar-types)
  (define quantvar-pairs (map pair->list quantvar-types))
  (define dummy-hash (make-hash))
  (define list-of-types (expression-type-top-level-types (checkExpression run-or-state expr quantvar-pairs dummy-hash)))
  (define top-level-type-list (for/list ([type list-of-types])
    (if (equal? type 'Int) "IntAtom" "Atom")))
  (format "(Relation ~a)" (string-join top-level-type-list " "))
)

(define (process-children-formula run-or-state children relations atom-names quantvars quantvar-types bounds [int-ctxt #f])
  (map (lambda (x) (convert-formula run-or-state x relations atom-names quantvars quantvar-types bounds)) children))

(define (process-children-expr run-or-state children relations atom-names quantvars quantvar-types bounds [int-ctxt #f])
  (map (lambda (x) (convert-expr run-or-state x relations atom-names quantvars quantvar-types bounds)) children))

(define (process-children-int run-or-state children relations atom-names quantvars quantvar-types bounds [int-ctxt #f])
  (map (lambda (x) (convert-int run-or-state x relations atom-names quantvars quantvar-types bounds int-ctxt)) children))

(define (process-children-ambiguous run-or-state children relations atom-names quantvars quantvar-types bounds [int-ctxt #f])
  (for/list ([child children])
    (match child
      [(? node/formula? f) (convert-formula run-or-state f relations atom-names quantvars quantvar-types bounds)]
      [(? node/expr? e) (convert-expr run-or-state e relations atom-names quantvars quantvar-types bounds)]
      [(? node/int? i) (convert-int run-or-state i relations atom-names quantvars quantvar-types bounds int-ctxt)])))

; Spot bugs, etc. noisily
(define (defensive-checks label run-or-state node relations atom-names quantvars quantvar-types args bounds)
  (unless (and (list? quantvar-types)
               (or (empty? quantvar-types)
                   (pair? (first quantvar-types))))
    (raise-forge-error #:msg (format "Internal error; ~a on ~a; quantvar-types was not an association list: ~a"
                                     label node quantvar-types)
                       #:context node))
  (unless (equal? (length quantvars) (length quantvar-types))
    (raise-forge-error #:msg (format "Internal error; ~a on ~a; quantvars and types were not of the same length: ~a vs. ~a"
                                     label node quantvars quantvar-types)
                       #:context node)))


(define (convert-formula-op run-or-state formula relations atom-names quantvars quantvar-types args bounds [int-ctxt #f])
  (defensive-checks "convert-formula-op" run-or-state formula relations atom-names quantvars quantvar-types args bounds) 
  (when (@> (get-verbosity) VERBOSITY_LOW)
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
      (if (for/or ([child children]) (if (node/int/op/card? child) #t #f))
      (form-cardinality run-or-state formula relations atom-names quantvars quantvar-types children "=" bounds)
      (format "(= ~a)" (string-join (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds))))]
    [(node/formula/op/! info children)
      (format "(not ~a)" (string-join (process-children-formula run-or-state args relations atom-names quantvars quantvar-types bounds) ""))]
    [(node/formula/op/int> info children)
      (if (for/or ([child children]) (if (node/int/op/card? child) #t #f))
      (form-cardinality run-or-state formula relations atom-names quantvars quantvar-types children ">" bounds)
      (format "(> ~a)" (string-join (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds #t))))]
    [(node/formula/op/int< info children)
      (if (for/or ([child children]) (if (node/int/op/card? child) #t #f))
      (form-cardinality run-or-state formula relations atom-names quantvars quantvar-types children "<" bounds)
      (format "(< ~a)" (string-join (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds #t))))]
    [(node/formula/op/int= info children)
      (if (for/or ([child children]) (if (node/int/op/card? child) #t #f))
      (form-cardinality run-or-state formula relations atom-names quantvars quantvar-types children "=" bounds)
      (format "(= ~a)" (string-join (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds #t))))]))

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

(define (form-comprehension run-or-state expr relations atom-names quantvars quantvar-types decls bounds processed-form)
  ; step 1: collect each of the variables in the formula
  (define collector-lambda (lambda (n ctxt) (if (node/expr/quantifier-var? n) n #f)))
  (define comprehension-quantvars (remove-duplicates (collect expr collector-lambda #:order 'pre-order)))
  ; step 2: split up the decls
  (define decl-vars (map car decls))
  (define decl-types (map cdr decls))
  (define processed-decl-types (string-join (map (lambda (decl) (convert-expr run-or-state (cdr decl) relations atom-names quantvars quantvar-types bounds)) decls) " "))
  ; step 3: we only want the quantvars that are not declared within the comprehension to be the arguments to the function that returns the set.
  (define argument-vars (filter-map (lambda (var) (if (not (member var decl-vars)) var #f)) quantvars))
  (define argument-types (map (lambda (var) (cdr (assoc var quantvar-types))) argument-vars))
  ; step 4: establish our set we are creating
  (define set-name (format "_~a_set" (gensym)))
  (define declaration-str (format "(declare-fun ~a (~a) (Relation ~a))" set-name (string-join (map (lambda (x) (atom-or-int x)) argument-types) " ")
                                                                                 (string-join (map (lambda (x) (atom-or-int x)) decl-types) " ")))
  ; step 5: create the constraints: the equality gives us the IFF, and subset is how we build our new set
  (define membership-string (format "(and ~a)" (string-join (map (lambda (atom) 
                                               (format "(set.subset (set.singleton (tuple ~a)) ~a)" atom (convert-expr run-or-state (cdr (assoc atom decls)) relations atom-names quantvars quantvar-types bounds))) decl-vars) " ")))
  (define subset-string (if (equal? (length decl-vars) 1) 
                            (format "(set.singleton (tuple ~a))" (car decl-vars)) 
                            (format "(set.singleton (tuple ~a))" (string-join (map (lambda (x) (format "~a" x)) decl-vars) " "))))
  (define get-set (if (equal? (length argument-vars) 0) (format "~a" set-name) (format "(~a ~a)" set-name (string-join (map (lambda (var) (format "~a" var)) argument-vars) " "))))
  (define constraint-args (append decl-vars argument-vars))
  (define constraint-types (append decl-types argument-types))
  (define constraint-pairs (map cons constraint-args constraint-types))
  (define constraint-str (format "(assert (forall (~a) (= (and ~a ~a) (set.subset ~a ~a))))" 
                                  (string-join (map (lambda (x) (format "(~a ~a)" (car x) (atom-or-int (cdr x)))) constraint-pairs) " ") 
                                  ; first subset is ensuring quantvars are in the decl set
                                  membership-string
                                  processed-form 
                                  ; second subset is ensuring quantvars are in the new set
                                  subset-string get-set))
  (set! new-top-level-strings (append (list declaration-str constraint-str) new-top-level-strings))
  get-set
)

(define (form-int-op-comp run-or-state expr relations atom-names quantvars quantvar-types processed-form bounds)
  ; This function is used to handle `sing` expressions.
  ; This allows for us to not worry about type mismatches between Int and IntAtom, because 
  ; any Int wrapped in a singleton (aka becoming a (Relation Int)) will now produce a set 
  ; that is really just a (Relation IntAtom), and so whatever it is being compared to will 
  ; certainly also be a (Relation IntAtom). 
  (define argument-vars quantvars)
  (define argument-types (map (lambda (var) (cdr (assoc var quantvar-types))) quantvars))
  (define set-name (format "_~a_comp" (gensym)))
  (define declaration-str (format "(declare-fun ~a (~a) (Relation IntAtom))" set-name (string-join (map (lambda (x) (atom-or-int x)) argument-types) " ")))
  (define get-set (if (equal? (length argument-vars) 0) (format "~a" set-name) (format "(~a ~a)" set-name (string-join (map (lambda (var) (format "~a" var)) argument-vars) " "))))
  (define constraint-pairs (map cons argument-vars argument-types))
  (define const-name (format "_~a_atom" (gensym)))
  (define new-decl (format "(declare-const ~a IntAtom)" const-name))
  (define new-constraint (format "(= (IntAtom-to-Int ~a) ~a)" const-name (car processed-form)))
  (define constraint-str (if (equal? (length constraint-pairs) 0) 
                                  (format "(assert (and ~a (set.subset (set.singleton (tuple ~a)) ~a)))" new-constraint const-name get-set)
                                  (format "(assert (exists (~a) (and ~a (set.subset (set.singleton (tuple ~a)) ~a))))" 
                                  (string-join (map (lambda (x) (format "(~a ~a)" (car x) (atom-or-int (cdr x)))) constraint-pairs) " ") 
                                  new-constraint const-name get-set)))
  (set! new-top-level-strings (append (list new-decl declaration-str) new-top-level-strings))
  (set! new-top-level-strings (append new-top-level-strings (list constraint-str)))
  get-set
)

(define (flip-op op) 
  (match op 
    [">" "<"]
    ["<" ">"]
    ["=" "="]
  )
)

(define (form-cardinality run-or-state formula relations atom-names quantvars quantvar-types children op bounds)
  ; Ensure that the child that is not a cardinality operator is an int constant.
  (define int-expr (for/or ([child children] #:when (not (node/int/op/card? child))) child))
  (define card-expr (for/or ([child children] #:when (node/int/op/card? child)) (car (node/int/op-children child))))
  (define card-expr-type (expression-type-top-level-types (checkExpression run-or-state card-expr quantvar-types (make-hash))))
  (define processed-card-expr (convert-expr run-or-state card-expr relations atom-names quantvars quantvar-types bounds))
  (define value 0)
  (if (not (node/int/constant? int-expr))
    (raise-forge-error #:msg "Cardinality operator must be applied to an integer constant."
                       #:context formula)                
    (set! value (node/int/constant-value int-expr))
  )
  ; Flip if LHS is the int to make casework simpelr
  (if (equal? (car children) int-expr) (set! op (flip-op op)) (void))
  ; modify value to adhere to strictness (since <= >= get desugared)
  (match op 
    [">" (set! value (@+ value 1))]
    ["<" (set! value (@- value 1))]
    ["=" (set! value value)]
  )
  (if (and (@< value 0) (not (equal? op ">")))
    (raise-forge-error #:msg "Cardinality operator must be applied to a non-negative integer constant."
                       #:context formula) 
    (set! value 0) ; case where we have > negative... should set to 0 so we dont try to construct list of negative len
  )
  ; build atoms for existential
  (define type-list (string-join (for/list ([type card-expr-type])
    (if (equal? type 'Int) "IntAtom" "Atom")) " "))
  (define atom-list (for/list ([i (in-range value)]) (format "(x_~a ~a)" i type-list)))
  ; build union of singletons for containment
  (define union-list (if (equal? (length atom-list) 0) "(as set.empty (Relation Atom))" 
                         (if (equal? (length atom-list) 1) (format "(set.singleton (tuple x_0))") 
                         (format "(set.union ~a)" (string-join (for/list ([i (in-range value)]) (format "(set.singleton (tuple x_~a))" i)) " ")))))
  ; assert each atom is distinct
  (define distinct-list (if (equal? (length atom-list) 0) "true" 
                          (if (equal? (length atom-list) 1) "true" 
                          (format "(distinct ~a)" (string-join (for/list ([i (in-range value)]) (format "x_~a" i)) " ")))))
  (if (equal? value 0) (format "(= ~a ~a)" union-list processed-card-expr) 
  (match op 
    ["<" (format "(exists ~a (and (set.subset ~a ~a) ~a))" atom-list processed-card-expr union-list distinct-list)]
    [">" (format "(exists ~a (and (set.subset ~a ~a) ~a))" atom-list union-list processed-card-expr distinct-list)]
    ["=" (format "(exists ~a (and (= ~a ~a) ~a))" atom-list processed-card-expr union-list distinct-list)]
  ))
)


(define (convert-expr run-or-state expr relations atom-names quantvars quantvar-types bounds)
  (defensive-checks "convert-expr" run-or-state expr relations atom-names quantvars quantvar-types '() bounds) 
  (when (@> (get-verbosity) VERBOSITY_LOW)
      (printf "to-smtlib-tor: convert-expr: ~a~n" expr))
  (match expr
    [(node/expr/relation info arity name typelist-thunk parent isvar)
     ; Declared sigs are referred to as Atoms, so we refer to them as such
     ; Ints are separate
     (cond [(equal? name "Int") "(as set.universe (Relation IntAtom))"]
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
            (printf "EXPANDED: ~a~n " expanded)
            (define components (join->list/right expanded))
            (unless (and (node/expr/relation? (last components))
                         (equal? (node/expr/relation-name (last components)) (symbol->string name)))
              (raise-forge-error #:msg (format "Skolem (function) fun-spacer marker did not match: ~a vs. ~a" (last components) name)
                                 #:context info))
            ;; replace (note: the arguments must be variables)
            (for ([a (drop-right components 1)])
              (unless (or (node/expr/quantifier-var? a) (node/expr/atom? a))
                (raise-forge-error #:msg (format "Unexpected argument to Skolem function; was not a variable: ~a" a)
                                   #:context a)))
            (printf "components: ~a~n" (deparen (drop-right components 1)))
            (printf "node/expr/atom? ~a~n" (node/expr/atom? (car (drop-right components 1))))
            (define modified-args (map (lambda (x) (if (node/expr/atom? x) (node/expr/atom-name x) x)) (drop-right components 1)))
            (printf "modified args: ~a~n" modified-args)
            ; TODO: if used in an int context, do we want to wrap still?
            (if (get-annotation info 'smt/int-unwrap)
                (format "(~a ~a)" name (deparen modified-args))
                (format "(set.singleton (tuple (~a ~a)))" name (deparen modified-args)))]
           [(equal? #\$ (string-ref (symbol->string name) 0))
            (unless (and (node/expr/relation? expanded)
                         (equal? (node/expr/relation-name expanded) (symbol->string name)))
              (raise-forge-error #:msg (format "Skolem (constant) fun-spacer marker did not match: ~a vs. ~a" expanded name)
                                 #:context info))
            (if (get-annotation info 'smt/int-unwrap)
                (format "~a" name)
                (format "(set.singleton (tuple ~a))" name))
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
     (format "(as set.universe (Relation Atom))")]
    [(node/expr/constant info arity 'iden)
     (format "(rel.iden (as set.universe (Relation Atom)))")]
    [(node/expr/constant info arity 'none)
     ; produce an empty set of the appropriate arity
    ;  (for/fold ([acc "set.empty"])
    ;            ([todo (build-list (@- arity 1) (lambda (x) x))])
    ;    (format "(rel.product set.empty ~a)" acc))]
    ; temporary patch - make it an empty set of int relations, but eventually we want to use atom-or-int to some capacity
    (format "(as set.empty (Relation Int))")]
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
     (define newly-defined-vars (map car decls))
     (define updated-quantvars (append newly-defined-vars quantvars))
     (define updated-quantvar-types (append decls quantvar-types))
     (let ([processed-form (convert-formula run-or-state form relations atom-names updated-quantvars updated-quantvar-types bounds)])
      (form-comprehension run-or-state expr relations atom-names updated-quantvars updated-quantvar-types decls bounds processed-form))]))

(define (convert-expr-op run-or-state expr relations atom-names quantvars quantvar-types args bounds [int-ctxt #f])
  (defensive-checks "convert-expr-op" run-or-state expr relations atom-names quantvars quantvar-types args bounds) 
    (when (@> (get-verbosity) VERBOSITY_LOW)
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
     (format "(set.union (rel.tclosure ~a) (rel.iden (as set.universe (Relation Atom))))" (string-join (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds) " "))]
    [(node/expr/op/~ info arity children)
     (format "(rel.transpose ~a)" (string-join (process-children-expr run-or-state args relations atom-names quantvars quantvar-types bounds) " "))]
    [(node/expr/op/++ info arity children)
      ; R 
      (define R (first children))
      (define k_u (node/expr/op-children (second children)))
      (define k (first k_u))
      (define u (second k_u))
      (define R_codomain (get-expr-type run-or-state u quantvars quantvar-types))
      (format "(set.union (set.minus ~a (rel.product ~a (as set.universe (Relation ~a)))) (rel.product ~a ~a))" 
            (convert-expr run-or-state R relations atom-names quantvars quantvar-types bounds)
            (convert-expr run-or-state k relations atom-names quantvars quantvar-types bounds)
            R_codomain
            (convert-expr run-or-state k relations atom-names quantvars quantvar-types bounds)
            (convert-expr run-or-state u relations atom-names quantvars quantvar-types bounds))]
    [(node/expr/op/sing info arity children)
      (let ([processed-form (process-children-int run-or-state children relations atom-names quantvars quantvar-types bounds #t)])
        (form-int-op-comp run-or-state expr relations atom-names quantvars quantvar-types processed-form bounds))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Integer expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (convert-int run-or-state expr relations atom-names quantvars quantvar-types bounds [int-ctxt #f])
  (defensive-checks "convert-int" run-or-state expr relations atom-names quantvars quantvar-types '() bounds) 
  (when (@> (get-verbosity) VERBOSITY_LOW)
    (printf "to-smtlib-tor: convert-int: ~a~n" expr))
  (match expr
    [(node/int/constant info value)
     (cond 
      [int-ctxt (format "~a" value)]
      [else         
        (define const-name (format "_~a_atom" (gensym)))
        (define new-decl (format "(declare-const ~a IntAtom)" const-name))
        (define new-constraint (format "(assert (= (IntAtom-to-Int ~a) ~a))" const-name value))
        (set! new-top-level-strings (append (list new-decl new-constraint) new-top-level-strings))
        (format "~a" const-name)])]
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

(define (convert-int-op run-or-state expr relations atom-names quantvars quantvar-types args bounds [int-ctxt #f])
  (defensive-checks "convert-int-op" run-or-state expr relations atom-names quantvars quantvar-types args bounds) 
  (when (@> (get-verbosity) VERBOSITY_LOW)
    (printf "to-smtlib-tor: convert-int-op: ~a~n" expr))
  (match expr
    [(node/int/op/add info children)
      (format "(+ ~a)" (string-join (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds #t) " "))]
    [(node/int/op/subtract info children)
      (format "(- ~a)" (string-join (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds #t) " "))]
    [(node/int/op/multiply info children)
      (format "(* ~a)" (string-join (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds #t) " "))]
    [(node/int/op/divide info children)
      (format "(div ~a)" (string-join (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds #t) " "))]
    [(node/int/op/sum info children)
     ; Since we are following the CRS/cvc4 work for integer-handling, the child must always be a singleton.
     ; This holds whether or not the "sum" was added automatically, but manual/auto affects the nature of the error.
     ;(printf "qv-types: ~a; qvs: ~a~n" quantvar-types quantvars)
     (define inferred-multiplicity (expression-type-multiplicity
                                    (checkExpression run-or-state (first children) (map pair->list quantvar-types) (make-hash))))
     ; (printf "***** inferred-multiplicity: ~a ~a~n" (first children) inferred-multiplicity)
     ; TODO: pass back something better than #t or #f
     (unless (member inferred-multiplicity '(one lone no #t))
       (if (get-annotation info 'automatic-int-conversion)
           (raise-forge-error #:msg "SMT backend requires that this expression evaluates to a singleton integer, but could not infer this." #:context expr)
           (raise-forge-error #:msg "SMT backend does not currently support `sum` over multiple integer values, but could not infer safety." #:context expr)))
    (format "(IntAtom-to-Int (reconcile-int_atom ~a))" (string-join (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds) " "))]
    [(node/int/op/card info children)
    (let ([processed-form (string-join (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds) " ")])
    processed-form)]
    [(node/int/op/remainder info children)
     (format "(mod ~a)" (string-join (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds) " "))]
    [(node/int/op/abs info children)
     (format "(abs ~a)" (string-join (process-children-ambiguous run-or-state args relations atom-names quantvars quantvar-types bounds) " "))]
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