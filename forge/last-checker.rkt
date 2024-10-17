#lang racket/base

(require 
  forge/sigs-structs
  forge/lang/ast
  forge/lang/bounds
  forge/shared
  racket/syntax
  syntax/srcloc
  (prefix-in @ (only-in racket -> >= - > ->*))
  racket/list
  racket/match
  (only-in racket/string string-join)
  (except-in racket/set set)
  (only-in racket/contract define/contract or/c listof any))

(require forge/utils/collector)

(provide checkFormula checkExpression checkInt primify infer-atom-type dfs-sigs deprimify)

; This function only exists for code-reuse - it's so that we don't
; need to use begin and hash-ref in every single branch of the match
; Given a struct to handle, a hashtable to search for a handler in,
; and an output to return, executes the check in the handler
; then returns the output.
; In the case of an expression, the output is an expression-type struct, which
; the language-specific checking might use to decide whether there is an error.
(define (check-and-output ast-node to-handle checker-hash result [child-types #f])
  ;(printf "check-and-output: ~a; ~a; ~a; ~a~n" ast-node to-handle result child-types)
  (begin
    (when (hash-has-key? checker-hash to-handle)
      ((hash-ref checker-hash to-handle) ast-node result child-types))
    result))

; Fail unless the run is in temporal mode (last-resort check to prevent use of
; temporal operators if temporal engine is not engaged)
; TODO: are expressions to LHS and RHS of integer comparison not recurred on?
(define/contract (check-temporal-mode run-or-state a-node)
  (@-> (or/c Run? State? Run-spec?) 
       node?
       void?)
  (unless (equal? 'temporal 
                  (get-option run-or-state 'problem_type))
          (raise-syntax-error #f (format "Error: use of LTL operator without temporal problem_type declared")
                                 (datum->syntax #f (deparse a-node) (build-source-location-syntax (nodeinfo-loc (node-info a-node)))))))


; Recursive descent for last-minute consistency checking
; Catch potential issues not detected by AST construction + generate warnings
(define/contract (checkFormula run-or-state formula quantvars checker-hash)
  (@-> (or/c Run? State? Run-spec?)
       (or/c node/formula? node/expr?)
       list?
       hash?
       (or/c boolean? void? (listof (listof symbol?))))

  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "last-checker: checkFormula: ~a; quantvars: ~a~n" formula quantvars))
  
  (match formula    
    [(node/formula/constant info type)
     (check-and-output formula node/formula/constant checker-hash #f)]

    [(node/fmla/pred-spacer info name args expanded)
    (define domain-types (for/list ([arg args]) 
                         (expression-type-type (checkExpression run-or-state (mexpr-expr (apply-record-domain arg)) quantvars checker-hash))))
    
    (define arg-types (for/list ([arg args]  [acc (range 0 (length args))])
                      (list (expression-type-type (checkExpression run-or-state (apply-record-arg arg) quantvars checker-hash)) acc)))
    ;(printf "pred spacer: ~a~n~a~n~a~n" name domain-types arg-types)
    (for-each 
        (lambda (type) (if (not (member (car (car type)) (list-ref domain-types (cadr type))))
            (raise-forge-error
            #:msg (format "Argument ~a of ~a given to predicate ~a is of incorrect type. Expected type ~a, given type ~a" 
                  (add1 (cadr type)) (length args) name (deprimify run-or-state 
                                                        (if (equal? (map Sig-name (get-sigs run-or-state)) (flatten domain-types))
                                                        (map Sig-name (get-sigs run-or-state)) ; if the domain is univ then just pass in univ
                                                        (list-ref domain-types (cadr type)))) ; else pass in the sig one by one
                                                        (deprimify run-or-state 
                                                        (if (equal? (map Sig-name (get-sigs run-or-state)) (flatten (car (car arg-types))))
                                                        (map Sig-name (get-sigs run-or-state)) ; if the argument is univ then just pass in univ
                                                        (car (car type))))) ; else pass in the sig one by one
            #:context (apply-record-arg (list-ref args (cadr type))))
            (void)))
            arg-types)
    (checkFormula run-or-state expanded quantvars checker-hash)]
    
    [(node/formula/op info args)
     (check-and-output formula
                       node/formula/op
                       checker-hash
                       (checkFormulaOp run-or-state formula quantvars args checker-hash))]
        
    [(node/formula/multiplicity info mult expr)
     (define expr-type (checkExpression run-or-state expr quantvars checker-hash))
     (check-and-output formula
                       node/formula/multiplicity
                       checker-hash
                       (void)
                       (list expr-type))]
    
    [(node/formula/quantified info quantifier decls subform)
     (check-and-output formula
                       node/formula/quantified
                       checker-hash
                       (begin
                         ; The new set of quantified variables will be extended by the variables declared here.
                         ; When checking the domains of each, need to make the _prior_ decls available.
                         
                         (let ([new-quantvars
                                (foldl 
                                 (lambda (decl sofar)
                                   (let ([var (car decl)]
                                         [domain (cdr decl)])
                                     ; CHECK: shadowed variables
                                     ; We look only at identity (taking the gensym suffix into account), rather than
                                     ; looking at names. See tests/forge/ast-errors.frg. 
                                     
                                     (when (assoc var quantvars)
                                       (raise-forge-error
                                        #:msg (format "Nested re-use of variable ~a detected. Check for something like \"some x: A | some x : B | ...\"." var)
                                        #:context var))
                                     ; CHECK: recur into domain(s), carrying decls from prior quantifiers
                                     ;   (but not this one!)
                                     (expression-type-type (checkExpression run-or-state domain sofar checker-hash))
                                     ; Add to _end_ of the list, to maintain ordering
                                     (reverse (cons (assocify decl) (reverse sofar)))))
                                 quantvars decls)])
                           ; CHECK: recur into subformula (with extended variable environment)
                           (checkFormula run-or-state subform new-quantvars checker-hash))))]
    [(node/formula/sealed info)
     (checkFormula run-or-state info quantvars)]
    [else (error (format "no matching case in checkFormula for ~a" (deparse formula)))]))

; This should probably be called "decl->list"; it's really _de_-associfying since
; an association list is a list of (car cdr) pairs. But "assoc" works with "car", so
; it seems to function to retrieve the entry even if the inner elements are lists,
; not pairs. 
(define (assocify a-pair)  
  (list (car a-pair) (cdr a-pair)))

; This function isn't technically needed at the moment: every branch just recurs
; However, keeping the structure here for easy addition of new checks
(define/contract (checkFormulaOp run-or-state formula quantvars args checker-hash)
  (@-> (or/c Run? State? Run-spec?)
       (or/c node/formula? node/expr?)
       list?
       (listof (or/c node/expr? node/int? node/formula?))
       hash?
       (or/c void? (listof (listof symbol?))))

  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "last-checker: checkFormulaOp: ~a~n" formula))

  (match formula
    
    ; TEMPORAL OPERATORS
    [(? node/formula/op/always?)
     (check-temporal-mode run-or-state formula)
     (check-and-output formula
                       node/formula/op/always
                       checker-hash
                       (for-each (lambda (x) (checkFormula run-or-state x quantvars checker-hash)) args))]
    [(? node/formula/op/eventually?)
     (check-temporal-mode run-or-state formula)
     (check-and-output formula
                       node/formula/op/eventually
                       checker-hash
                       (for-each (lambda (x) (checkFormula run-or-state x quantvars checker-hash)) args))]
    [(? node/formula/op/until?)
     (check-temporal-mode run-or-state formula)
     (check-and-output formula
                       node/formula/op/until
                       checker-hash
                       (for-each (lambda (x) (checkFormula run-or-state x quantvars checker-hash)) args))]
    [(? node/formula/op/releases?)
     (check-temporal-mode run-or-state formula)
     (check-and-output formula
                       node/formula/op/releases
                       checker-hash
                       (for-each (lambda (x) (checkFormula run-or-state x quantvars checker-hash)) args))]
    [(? node/formula/op/next_state?)
     (check-temporal-mode run-or-state formula)
     (check-and-output formula
                       node/formula/op/next_state
                       checker-hash
                       (for-each (lambda (x) (checkFormula run-or-state x quantvars checker-hash)) args))]
    
    [(? node/formula/op/historically?)
     (check-temporal-mode run-or-state formula)
     (check-and-output formula
                       node/formula/op/historically
                       checker-hash
                       (for-each (lambda (x) (checkFormula run-or-state x quantvars checker-hash)) args))]
    [(? node/formula/op/once?)
     (check-temporal-mode run-or-state formula)
     (check-and-output formula
                       node/formula/op/once
                       checker-hash
                       (for-each (lambda (x) (checkFormula run-or-state x quantvars checker-hash)) args))]
    [(? node/formula/op/prev_state?)
     (check-temporal-mode run-or-state formula)
     (check-and-output formula
                       node/formula/op/prev_state
                       checker-hash
                       (for-each (lambda (x) (checkFormula run-or-state x quantvars checker-hash)) args))]
    [(? node/formula/op/since?)
     (check-temporal-mode run-or-state formula)
     (check-and-output formula
                       node/formula/op/since
                       checker-hash
                       (for-each (lambda (x) (checkFormula run-or-state x quantvars checker-hash)) args))]
    [(? node/formula/op/triggered?)
     (check-temporal-mode run-or-state formula)
     (check-and-output formula
                       node/formula/op/triggered
                       checker-hash
                       (for-each (lambda (x) (checkFormula run-or-state x quantvars checker-hash)) args))]
    
    ; AND 
     [(? node/formula/op/&&?)
      (check-and-output formula
                        node/formula/op/&&
                        checker-hash
                        (for-each (lambda (x) (checkFormula run-or-state x quantvars checker-hash)) args))]

    ; OR
    [(? node/formula/op/||?)
     (check-and-output formula
                       node/formula/op/||
                       checker-hash
                       (for-each (lambda (x) (checkFormula run-or-state x quantvars checker-hash)) args))]
    
    ; IMPLIES
    [(? node/formula/op/=>?)
     (check-and-output formula
                       node/formula/op/=>
                       checker-hash
                       (for-each (lambda (x) (checkFormula run-or-state x quantvars checker-hash)) args))]
    
    ; IN (atomic fmla)
    [(? node/formula/op/in?)
     (check-and-output formula
                       node/formula/op/in
                       checker-hash
                       (for-each (lambda (x) (expression-type-type (checkExpression run-or-state x quantvars checker-hash))) args))]
    
    ; EQUALS 
    [(? node/formula/op/=?)
     (define child-types (map (lambda (x) (checkExpression run-or-state x quantvars checker-hash)) args))
     (check-and-output formula
                       node/formula/op/=
                       checker-hash
                       (void)
                       child-types)]

    ; NEGATION
    [(? node/formula/op/!?)
     (check-and-output formula
                       node/formula/op/!
                       checker-hash
                       (for-each (lambda (x) (checkFormula run-or-state x quantvars checker-hash)) args))]
    
    ; INTEGER >, <, =
    [(or (? node/formula/op/int>?)
         (? node/formula/op/int<?)
         (? node/formula/op/int=?))
      ; descend into the integer-expression within and confirm no literals are unsafe
     (checkInt run-or-state (first (node/formula/op-children formula)) quantvars checker-hash)
     (checkInt run-or-state (second (node/formula/op-children formula)) quantvars checker-hash)]))

; Turn signame into list of all primsigs it contains
; Note we use Alloy-style "_remainder" names here; these aren't necessarily embodied in Forge
(define/contract (primify run-or-state raw-signame)
  (@-> (or/c Run? State? Run-spec?) (or/c symbol? string?) (listof symbol?))  
  (let ([signame (cond [(string? raw-signame) (string->symbol raw-signame)]
                       [(Sig? raw-signame) (Sig-name raw-signame)]
                       [else raw-signame])])
    (cond [(equal? 'Int signame)           
           '(Int)]
          [(equal? 'univ signame)
           (if (member (get-option (get-run-spec run-or-state) 'backend) UNBOUNDED_INT_BACKENDS)       
           (remove-duplicates (flatten (map (lambda (n) (primify run-or-state n)) (remove 'Int (map Sig-name (get-sigs run-or-state))))))  
           (remove-duplicates (flatten (map (lambda (n) (primify run-or-state n)) (cons 'Int (map Sig-name (get-sigs run-or-state)))))))]
          [else           
           (define the-sig (get-sig run-or-state signame))
           (define all-primitive-descendants
             (remove-duplicates
              (flatten
               (map (lambda (n) (primify run-or-state n))
                    (get-children run-or-state signame)))))
           (cond
             [(Sig-abstract the-sig)
              
             (if (empty? (get-children run-or-state signame))
                 (raise-forge-error
                  #:msg (format "The abstract sig ~a is not extended by any children" (symbol->string signame))
                  #:context the-sig)
                 all-primitive-descendants)]
             [else (cons 
                        (string->symbol (string-append (symbol->string signame) 
                            (if (empty? (get-children run-or-state signame))
                                ""
                                "_remainder")))
                         all-primitive-descendants)])])))


; Infer a type for an atom node. 
;   Best data source: the last Sterling instance of this run.
;   Not ideal data source (e.g., sterling hasn't been run): the Kodkod bounds generated for this run,
;     from which we can at least get the top-level sig to use.
;   Fallback data source (e.g., run hasn't been generated): none, just use univ.
(define (infer-atom-type run atom [in-instance #f])

  (define (infer-from-instance)
    (define tlsigs (if (Run? run) (get-top-level-sigs run) '()))
    (define last-matched
      (dfs-sigs run (lambda (s acc)
                      (if (member (list (atom-name atom)) (hash-ref in-instance (Sig-name s)))
                          (values s #f) ; try to find something more specific, don't stop
                          (values acc #t))) ; nothing to find, since this atom isn't in the parent
                (remove Int tlsigs)
                #f))
    (cond
      ; If most-specific sig has no children, just use that sig name
      [(and last-matched (empty? (get-children run last-matched)))
       (list (list (Sig-name last-matched)))]
      ; If most-specific sig has children, just the remainder
      [last-matched
       (list (list (string->symbol (string-append (symbol->string (Sig-name last-matched)) "_remainder"))))]
      ; Otherwise, we can infer nothing
      [else 
        (map list (primify run 'univ))]))
    
  ; Consider top-level sigs only
  (define (infer-from-bounds)
    (define kodkod-bounds (if (Run? run) (Run-kodkod-bounds run) '()))
    (define tlsigs-with-atom-in-bounds
      (filter-map (lambda (tlsig)
                    (define b (findf (lambda (b) (equal? tlsig (bound-relation b))) kodkod-bounds))
                    (if (and b (bound? b) (member (list (atom-name atom)) (bound-upper b)))
                        tlsig
                        #f))
                  (get-top-level-sigs run)))
    (if (empty? tlsigs-with-atom-in-bounds)
        (map list (primify run 'univ)) ; no match, cannot infer anything
        (map list (primify run (Sig-name (first tlsigs-with-atom-in-bounds)))))) ; we have a match
  
  (cond [(and in-instance (hash? in-instance)) (infer-from-instance)]
        [(Run? run) (infer-from-bounds)]
        [else (map list (primify run 'univ))]))
  
  
; Runs a DFS over the sigs tree, starting from sigs in <sigs>.
; On each visited sig, <func> is called to obtain a new accumulated value
; and whether the search should continue to that sig's children.
(define (dfs-sigs run-or-state func sigs init-acc)
    (define (dfs-sigs-helper todo acc)
      (cond [(equal? (length todo) 0) acc]
      [else (define next (first todo))
      (define-values (new-acc stop)
        (func next acc)) ; use define-values with the return of func
        (cond [stop (dfs-sigs-helper (rest todo) new-acc)]
              [else (define next-list (if (empty? (get-children run-or-state next)) ; empty?
                                      (rest todo)
                                      (append (get-children run-or-state next) (rest todo)))) ; append instead
              (dfs-sigs-helper next-list new-acc)])]))
    (dfs-sigs-helper sigs init-acc)) ; maybe take in initial accumulator as well for more flexibility

; Be robust to callers who pass quantifier-vars as either (var . domain) or as '(var domain).
(define (second/safe list-or-pair)
  (cond [(list? list-or-pair) (second list-or-pair)]
        [else (cdr list-or-pair)]))

(define (deprimify run-or-state primsigs)
  (let ([all-sigs (map Sig-name (get-sigs run-or-state))])
    (cond
      [(equal? primsigs '(Int))
       'Int]
      [(equal? primsigs (remove-duplicates (flatten (map (lambda (n) (primify run-or-state n)) (cons 'Int all-sigs)))))
       'univ]
      [else (define top-level (get-top-level-sigs run-or-state))
            (define pseudo-fold-lambda (lambda (sig acc) (if (or (subset? (primify run-or-state (Sig-name sig)) (flatten primsigs))
                                                                 (equal? (list (car (primify run-or-state (Sig-name sig)))) (flatten primsigs)))
                                                                 ; the above check is added for when you have the parent sig, but are expecting the child
                                      (values (append acc (list (Sig-name sig))) #t) ; replace cons with values
                                      (values acc #f))))
            (define final-list (dfs-sigs run-or-state pseudo-fold-lambda top-level '()))
            final-list])))

; wrap around checkExpression-mult to provide check for multiplicity, 
; while throwing the multiplicity away in output; DO NOT CALL THIS AS PASSTHROUGH!
(define (checkExpression run-or-state expr quantvars checker-hash)
;(printf "expr: ~a~n" expr)
  (match expr 
    ; extra work done on int - adding 1 to a var
    [(? node/int?) (expression-type (list (list 'Int)) 'one #f (list 'Int))]
    [(? integer?) (expression-type (list (list 'Int)) 'one #f (list 'Int))]
    ; [_ (let ([output (checkExpression-mult run-or-state expr quantvars checker-hash)])
    ;      (expression-type-type output))]))
    [_ (checkExpression-mult run-or-state expr quantvars checker-hash)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Gets the top levels; i.e. ('Int / 'univ ... arity times)
(define/contract (get-top-levels type-list run-or-state)
  (@-> list? (or/c Run? State? Run-spec?) list?)
  (let ([smt-backend (member (get-option (get-run-spec run-or-state) 'backend) UNBOUNDED_INT_BACKENDS)]) 
  (if (empty? type-list) (list 'univ)
  ; should be passing in a list of lists
  (for/list ([type (car type-list)])
    (if smt-backend (if (equal? type 'Int) 'Int 'univ) 'univ)
  )))
)

; For expressions, this descent does two things:
;   - collects possible typings of expressions
;   - throws syntax errors as needed (using typing info from children)
(define/contract (checkExpression-mult run-or-state expr quantvars checker-hash)
  (@-> (or/c Run? State? Run-spec?)
       node/expr?
       list?
       hash?
       any)

  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "last-checker: checkExpression: ~a~n" expr))
  
  (define (primifyThis n)
    (primify run-or-state n))
  
  (match expr
    
    ; relation name (base case)
    [(node/expr/relation info arity name typelist-thunk parent isvar)
     (check-and-output expr
                       node/expr/relation
                       checker-hash
                       (expression-type (apply cartesian-product (map primifyThis (typelist-thunk))) 
                                        ; TODO refine
                                        ; should only hit the 'else' case if it is a constant skolem relation I think
                                        (cond [(Relation-is? expr '(pfunc)) 'pfunc]
                                              [(Relation-is? expr '(func))  'func]
                                              [(and (equal? 1 (node/expr-arity expr))
                                                    (if (Sig? expr) (Sig-one expr) 1)) 'one]
                                              [else 'set])
                                        isvar
                                        (get-top-levels (apply cartesian-product (map primifyThis (typelist-thunk))) run-or-state))
                       '())]

    ; atom (base case)
    [(node/expr/atom info arity name)
     (check-and-output expr
                       node/expr/atom
                       checker-hash   
                       (expression-type (infer-atom-type run-or-state expr)
                             'one #f (get-top-levels (infer-atom-type run-or-state expr) run-or-state)) '())]

    [(node/expr/fun-spacer info arity name args codomain expanded)
     ; be certain to call the -mult version at the end, or the multiplicity will be thrown away.
     ; additionally, there is no lang-specific check for a predicate spacer
      (define domain-types (for/list ([arg args]) 
                         (expression-type-type (checkExpression run-or-state (mexpr-expr (apply-record-domain arg)) quantvars checker-hash))))
      (define arg-types (for/list ([arg args]  [acc (range 0 (length args))])
                      (list (expression-type-type (checkExpression run-or-state (apply-record-arg arg) quantvars checker-hash)) acc)))
      (for-each 
        (lambda (type) (if (not (member (car (car type)) (list-ref domain-types (cadr type))))
            (raise-forge-error
            #:msg (format "Argument ~a of ~a given to function ~a is of incorrect type. Expected type ~a, given type ~a" 
                  (add1 (cadr type)) (length args) name (deprimify run-or-state 
                                                        (if (equal? (map Sig-name (get-sigs run-or-state)) (flatten domain-types))
                                                        (map Sig-name (get-sigs run-or-state)) ; if the domain is univ then just pass in univ
                                                        (list-ref domain-types (cadr type)))) ; else pass in the sig one by one
                                                        (deprimify run-or-state 
                                                        (if (equal? (map Sig-name (get-sigs run-or-state)) (flatten (car (car arg-types))))
                                                        (map Sig-name (get-sigs run-or-state)) ; if the argument is univ then just pass in univ
                                                        (car (car type))))) ; else pass in the sig one by one
            #:context (apply-record-arg (list-ref args (cadr type))))
            (void)))
            arg-types)
      (define output-type (expression-type-type (checkExpression run-or-state expanded quantvars checker-hash)))
      (if (not (member (car output-type) (expression-type-type (checkExpression run-or-state (mexpr-expr codomain) quantvars checker-hash))))
          (raise-forge-error
          #:msg (format "The output of function ~a is of incorrect type" name)
          #:context expanded)
          (void))
       (checkExpression-mult run-or-state expanded quantvars checker-hash)]
    
    [(node/expr/ite info arity a b c)
     (let ([b-potentials (checkExpression-mult run-or-state b quantvars checker-hash)]
           [c-potentials (checkExpression-mult run-or-state c quantvars checker-hash)])
       (check-and-output expr
                         node/expr/ite
                         checker-hash
                         (begin (checkFormula run-or-state a quantvars checker-hash) ; Check condition formula
                                ; Might be a or b, we don't know
                                (expression-type (remove-duplicates (append (expression-type-type b-potentials)
                                                                            (expression-type-type c-potentials)))
                                                 ;; TODO: we can improve this multiplicity inference
                                                 (if (and (member (expression-type-multiplicity b-potentials) '(one lone))
                                                          (member (expression-type-multiplicity c-potentials) '(one lone)))
                                                     'lone
                                                     'set)
                                                 (get-temporal-variance run-or-state expr quantvars (list a b c) checker-hash)
                                                 (get-top-levels (remove-duplicates (append (expression-type-type b-potentials)
                                                                                           (expression-type-type c-potentials)))
                                                                                           run-or-state)
                                                 ))
                         (list b-potentials c-potentials)))]
    
    ; The INT Constant
    [(node/expr/constant info 1 'Int)
     (check-and-output expr
                       node/expr/constant
                       checker-hash
                       (expression-type (list (list 'Int))
                             'set #f (list 'Int)) '())]

    ; other expression constants (e.g., univ)
    [(node/expr/constant info arity type)
     (check-and-output expr
                       node/expr/constant
                       checker-hash
                       (expression-type (let ([prims (primify run-or-state 'univ)])
                              (cond [(equal? arity 1) (map list prims)]
                                    [else (cartesian-product prims prims)]))
                              'set
                              ; If any of the sigs are "var", then univ and iden may vary.
                              (if (and (member type '(univ iden))
                                       (not (empty? (andmap (lambda (s) (node/expr/relation-is-variable s)) (get-sigs run-or-state)))))
                                  #t #f) 
                              (let ([prims (primify run-or-state 'univ)])
                              (cond [(equal? arity 1) (get-top-levels (map list prims) run-or-state)]
                                    [else (get-top-levels (cartesian-product prims prims) run-or-state)]))) '())]
          
    ; expression w/ operator (union, intersect, ~, etc...)
    [(node/expr/op info arity args)
     ; We don't call check-and-output here. That is called for each individual case within checkExpressionOp.
     (checkExpressionOp run-or-state expr quantvars args checker-hash)]
 
    ; quantifier variable
    [(node/expr/quantifier-var info arity sym name)
     ;(printf "quantifier-var case. expr: ~a; quantvars: ~a~n" expr quantvars)z
     (check-and-output expr
                       node/expr/quantifier-var
                       checker-hash
                       ; Look up in quantvars association list, type is type of domain
                       (expression-type (if (assoc expr quantvars) ; expr, not sym (decls are over var nodes)
                                 (expression-type-type (checkExpression run-or-state (second/safe (assoc expr quantvars)) quantvars checker-hash))
                                 (raise-forge-error
                                  #:msg (format "Variable ~a used, but it was unbound at this point.\
 Often, this means that a sig, field, or helper name is being used as a quantifier variable.\
 It might also mean that the variable domain references the variable itself.\
 For help debugging, the bound variables at this point were: ~a" sym (map car quantvars) )
                                  #:context info))
                             'one #f
                             (expression-type-top-level-types (checkExpression run-or-state (second/safe (assoc expr quantvars)) quantvars checker-hash))
                             )
                       ; TODO: technically, this should pass the expression-types of the domains
                       '())]

    ; set comprehension e.g. {n : Node | some n.edges}
    [(node/expr/comprehension info arity decls subform)
     (check-and-output expr
                       node/expr/comprehension
                       checker-hash
                       ; For rationale here, see the quantified-formula case. This differs slightly
                       ; because a comprehension is an expression, and thus needs to report its type.

                       (begin
                         (let ([new-decls-and-child-values
                                (foldl (lambda (decl acc)
                                         (let ([var (car decl)]
                                               [domain (cdr decl)]
                                               [expanded-quantvars (first acc)]
                                               [child-types (second acc)])
                                           ; CHECK: shadowed variables (by identity, not by name)
                                           (when (assoc var quantvars)
                                             (raise-forge-error
                                              #:msg (format "Nested re-use of variable ~a detected. Check for something like \"some x: A | some x : B | ...\"." var)
                                              #:context info))
                                           
                                           ; CHECK: recur into domain(s), aware of prior variables
                                           (let ([next-child-type
                                                  (expression-type-type (checkExpression run-or-state domain expanded-quantvars checker-hash))])
                                             ; Accumulator: add current decl, add type (to end)
                                             (list (cons (assocify decl) expanded-quantvars)
                                                   (reverse (cons next-child-type (reverse child-types)))))))
                                       ; Acc has the form: (quantvars-so-far child-values-in-order)
                                       (list quantvars '())
                                       decls)])
                           ; CHECK: recur into subformula
                           (define new-quantvars (first new-decls-and-child-values))
                           (define child-values (second new-decls-and-child-values))
                           (checkFormula run-or-state subform new-quantvars checker-hash)
                           ; Return type constructed from decls above
                           (expression-type
                            (map flatten (map append (apply cartesian-product child-values)))
                            'set
                            ; need to check temporal variance for both all domains _AND_ the inner formula
                            (get-temporal-variance run-or-state expr quantvars (cons subform (map cdr decls)) checker-hash)
                            (get-top-levels (map flatten (map append (apply cartesian-product child-values))) run-or-state))))
                       ; TODO: technically, this should pass the expression types of the domains
                       '())]

    [else (error (format "no matching case in checkExpression for ~a" (deparse expr)))]))


(define (keep-only keepers pool)
  (filter (lambda (ele) (member ele keepers)) pool))

; can this expression vary with time? I.e., does it involve any var-declared relations?
; Because different expressions might involve different kinds of nodes to check (e.g., comprehensions)
; this function takes the "args" to be checked, rather than using the node itself.
(define (get-temporal-variance run-or-state node quantvars args checker-hash)
  (define var-relations
    ; TODO: this will perform poorly on VERY large formulas, because we aren't caching results and 
    ; thus the linear last-check ends up being quadratic in the worst case. 
    (apply append (map (lambda (arg)
                         (collect arg (lambda (n ctxt) (and (node/expr/relation? n)
                                                            (node/expr/relation-is-variable n))) #:order 'pre-order))
                       args)))
  (not (empty? var-relations)))
  
(define (intersection-multiplicity m1 m2 context)
  (cond
    [(equal? m1 'set) m2]
    [(equal? m2 'set) m1]
    [(or (equal? m1 'pfunc) (equal? m2 'pfunc)) 'set]
    [(or (equal? m1 'func)  (equal? m2 'func)) 'set]
    [(or (equal? m1 'no) (equal? m2 'no)) 'no]
    [(or (equal? m1 'lone) (equal? m2 'lone)) 'lone]
    [(and (equal? m1 'one) (equal? m2 'one)) 'lone]
    [else (raise-forge-error #:msg (format "Inferring multiplicity, unrecognized tags: ~a, ~a" m1 m2)
                             #:context context)]))

(define (join-multiplicity args child-values join-result context)
  ;(printf "join-multiplicity: ~a; child-values: ~a~n" context child-values)
  ;; TODO: very coarse
  (cond
    ; Do not try to infer anything beyond "set" if the LHS is not a singleton. This is a
    ; "Froglet-centric" perspective and likely could be improved.
    [(not (member (expression-type-multiplicity (first child-values)) '(lone one))) 'set]
    ; If the join is necessarily empty, infer that in the type. 
    [(empty? join-result) 'no]
    ; Otherwise, look at the RHS of the join. 
    [(equal? (expression-type-multiplicity (second child-values)) 'one) 'one]
    [(equal? (expression-type-multiplicity (second child-values)) 'lone) 'lone]
    ; If the RHS is non-nullary functional, and the result is arity 1, the function has been applied.
    [(and (equal? 1 (length (first join-result)))
          (equal? (expression-type-multiplicity (second child-values)) 'func))
     'one]
    [(and (equal? 1 (length (first join-result)))
          (equal? (expression-type-multiplicity (second child-values)) 'pfunc))
     'lone]
    ; If the RHS is non-nullary functional, and the result is arity >1, the singleton result exists in
    ; potential, but is not yet realized. The join result remains functional.
    [(equal? (expression-type-multiplicity (second child-values)) 'func)
     'func]
    [(equal? (expression-type-multiplicity (second child-values)) 'pfunc)
     'pfunc]
    ; Infer "set" as a last resort. 
    [else 'set]))

(define/contract (checkExpressionOp run-or-state expr quantvars args checker-hash)
  (@-> (or/c Run? State? Run-spec?) node/expr/op? list? (listof (or/c node/expr? node/int?)) hash?   
       any)

  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "last-checker: checkExpressionOp: ~a~n" expr))

  (define RESULT
  (match expr

    ; prime (temporal mode only)
    [(? node/expr/op/prime?)
     (check-temporal-mode run-or-state expr)
     (let [(expression (checkExpression run-or-state (first args) quantvars checker-hash))]
     (if (expression-type-temporal-variance expression)
         (check-and-output expr
              node/expr/op/prime
              checker-hash
              (expression-type (expression-type-type expression)
                               'set ; TODO: can we re-use the subexpression safely, even if in a different state?
                               (expression-type-temporal-variance expression)
                               (expression-type-top-level-types expression)))
         (raise-forge-error
          #:msg (format "Prime operator used in non-temporal context")
          #:context expr)))]

    ; UNION
    [(? node/expr/op/+?)
     (define child-types (map (lambda (x) (checkExpression run-or-state x quantvars checker-hash)) args))
     (check-and-output expr
                       node/expr/op/+
                       checker-hash
                       (expression-type
                        (remove-duplicates (apply append
                                                  (map (lambda (x) (expression-type-type x))
                                                       child-types)))
                              'set ; union may produce non-singleton from two singletons
                              (get-temporal-variance run-or-state expr quantvars args checker-hash)
                              (get-top-levels (remove-duplicates (apply append
                                                  (map (lambda (x) (expression-type-type x))
                                                       child-types))) run-or-state))
                       child-types)]

    
    ; SETMINUS 
    [(? node/expr/op/-?)
     (define child-types (map (lambda (x) (checkExpression run-or-state x quantvars checker-hash)) args))
     (begin
       ; A-B should have only 2 children. B may not exist; use A as the bound returned regardless. 
       ; However, if B is present, we must /check/ it anyway (and discard non-error results).
       (when (@> (length args) 1)
         (expression-type-type (checkExpression run-or-state (second args) quantvars checker-hash)))
       (check-and-output expr
                         node/expr/op/-
                         checker-hash
                         ; A-B should have only 2 children. B may be empty.
                         (let ([a-type (first child-types)])
                           (expression-type (expression-type-type a-type)
                                            (expression-type-multiplicity a-type) ; minus can't make a singleton non-singleton
                                            (get-temporal-variance run-or-state expr quantvars args checker-hash)
                                            (expression-type-top-level-types a-type)))
                         child-types))]
    
    ; INTERSECTION
    [(? node/expr/op/&?)
     (define sub-results (map (lambda (x) (checkExpression run-or-state x quantvars checker-hash)) args))
     (check-and-output expr
                       node/expr/op/&
                       checker-hash
                       (expression-type
                        (foldl (lambda (x acc) (keep-only (expression-type-type x) acc))
                               (expression-type-type (first sub-results))
                               (rest sub-results))
                        (foldl (lambda (x acc) (intersection-multiplicity acc (expression-type-multiplicity x) expr))
                               (expression-type-multiplicity (first sub-results))
                               (rest sub-results))
                        (get-temporal-variance run-or-state expr quantvars args checker-hash)
                        (foldl (lambda (x acc) (keep-only (expression-type-top-level-types x) acc))
                               (expression-type-top-level-types (first sub-results))
                               (rest sub-results)))
                       sub-results)]
    
    ; PRODUCT
    [(? node/expr/op/->?)
     (define child-types (map (lambda (x) (checkExpression run-or-state x quantvars checker-hash)) args))
     (check-and-output expr
                       node/expr/op/->
                       checker-hash
                       (let* ([child-values (map (lambda (x) (expression-type-type x)) child-types)]
                              [all-singleton (andmap (lambda (x) (equal? 'one (expression-type-multiplicity x))) child-types)]
                              [result (map flatten (map append (apply cartesian-product child-values)))]) 
                         (expression-type  
                          result
                          (if all-singleton 'one 'set) ; TODO: very coarse
                          (get-temporal-variance run-or-state expr quantvars args checker-hash)
                          (get-top-levels result run-or-state)))
                       child-types)]
   
    ; JOIN
    [(? node/expr/op/join?)
     (define child-types (map (lambda (x) (checkExpression run-or-state x quantvars checker-hash)) args))
     (check-and-output expr
                       node/expr/op/join
                       checker-hash
                       (let* ([join-result (check-join (map expression-type-type child-types))]
                              [join-top-level (check-join (map (lambda (x) (list (expression-type-top-level-types x))) child-types))])
                         (when (@>= (get-verbosity) VERBOSITY_LASTCHECK) 
                           (when (empty? join-result)
                            (if (eq? (nodeinfo-lang (node-info expr)) 'bsl)
                                ((hash-ref checker-hash 'empty-join) expr)
                              (raise-forge-error
                               #:msg (format "Join always results in an empty relation:\
 Left argument of join \"~a\" is in ~a.\
 Right argument of join \"~a\" is in ~a" 
                                             (deparse (first (node/expr/op-children expr)))  
                                             (map (lambda (lst) (string-join (map (lambda (c) (symbol->string c)) lst) " -> " #:before-first "(" #:after-last ")")) (expression-type-type (first child-types)))
                                             (deparse (second (node/expr/op-children expr)))
                                             (map (lambda (lst) (string-join (map (lambda (c) (symbol->string c)) lst) " -> " #:before-first "(" #:after-last ")")) (expression-type-type (second child-types))))
                               #:context expr))))
                         
                         ; If we are in Froglet ("bsl"), and the LHS expression is not a singleton or empty, lang-specific check
                         ; TODO: disentangle this; it should be handled by the join case of the checker hash
                         ;(when (and (not (member (expression-type-multiplicity (first child-types)) '(one lone)))
                         ;           (eq? (nodeinfo-lang (node-info expr)) 'bsl)
                         ;           (hash-has-key? checker-hash 'relation-join))
                         ;  ((hash-ref checker-hash 'relation-join) expr args))
                         
                         (define join-multip (join-multiplicity args child-types join-result expr))
                         ;(printf "***** ~a~n     ~a     ~a~n" expr args join-multip)
                         (expression-type
                            join-result
                            join-multip
                            (get-temporal-variance run-or-state expr quantvars args checker-hash)
                            (get-top-levels join-top-level run-or-state)))
                       child-types)]
    
    ; TRANSITIVE CLOSURE
    [(? node/expr/op/^?)
     (define child-types (map (lambda (x) (checkExpression run-or-state x quantvars checker-hash)) args))
     (check-and-output expr
                       node/expr/op/^
                       checker-hash
                       (expression-type (let* ([child-values (map (lambda (x) (expression-type-type x)) child-types)])     
                               (check-closure (first child-values)))
                              'set ; TODO
                              (get-temporal-variance run-or-state expr quantvars args checker-hash)
                              (let* ([child-values (map (lambda (x) (expression-type-type x)) child-types)])    
                              (get-top-levels (check-closure (first child-values)) run-or-state))))]

    ; REFLEXIVE-TRANSITIVE CLOSURE
    [(? node/expr/op/*?)
     (define child-types (map (lambda (x) (checkExpression run-or-state x quantvars checker-hash)) args))
     (check-and-output expr
                       node/expr/op/*
                       checker-hash
                       ; includes iden, so might contain any arity-2 tuple
                       (expression-type (let ([prims (primify run-or-state 'univ)])
                              (cartesian-product prims prims))
                              'set ; TODO
                              (get-temporal-variance run-or-state expr quantvars args checker-hash)
                              (let ([prims (primify run-or-state 'univ)])
                              (get-top-levels (cartesian-product prims prims) run-or-state)))
                       child-types)]

    ; TRANSPOSE: ~(r); r must be arity 2. reverse all types of r
    [(? node/expr/op/~?)
     (define child-types (map (lambda (x) (checkExpression run-or-state x quantvars checker-hash)) args))
     (check-and-output expr
                       node/expr/op/~
                       checker-hash
                       (expression-type (map reverse (expression-type-type (first child-types)))
                              'set ; TODO
                              (get-temporal-variance run-or-state expr quantvars args checker-hash)
                              (get-top-levels (map reverse (expression-type-type (first child-types))) run-or-state)
                              )
                       child-types)]
    

    ; RELATIONAL OVERRIDE
    ; AST already checks that both arguments have the same arity
    ; Need to check that the left sub-expression has arity at least 2
    ; Need to check the the left and right sub-expressions have the same types
    ; (which is much easier thanks to the AST checking that they have the same arity)
    [(? node/expr/op/++?)
     ;; TODO: check-and-output for this case
     (let ([left-arity (node/expr-arity (first args))]
           [left-tuples (expression-type-type (checkExpression run-or-state (first args) quantvars checker-hash))]
           [right-tuples (expression-type-type (checkExpression run-or-state (second args) quantvars checker-hash))]
           [syn-loc (nodeinfo-loc (node-info expr))])
       (let ([src-line (source-location-line syn-loc)]
             [src-col (source-location-column syn-loc)]
             [src-span (source-location-span syn-loc)])
         
         (unless (@>= left-arity 2)
           (raise-forge-error #:msg (format "++: arguments must have arity at least 2: got arity 1")
                              #:context (first args)))
         
         (when (set-empty? (set-intersect (list->set left-tuples)
                                          (list->set right-tuples)))
           (raise-forge-error #:msg (format "++: right argument will never override anything in left argument")
                              #:context (first args)))
         
         ; ++ has a maximum of two arguments so this should get everything
         (expression-type (remove-duplicates (append left-tuples right-tuples))
                'set ; TODO
                (get-temporal-variance run-or-state expr quantvars args checker-hash)
                (get-top-levels (remove-duplicates (append left-tuples right-tuples)) run-or-state))))]

    ; SINGLETON (typecast number to 1x1 relation with that number in it)
    [(? node/expr/op/sing?)
     ; descend into the integer-expression within and confirm no literals are unsafe
     (checkInt run-or-state (first (node/expr/op-children expr)) quantvars checker-hash)
     (check-and-output expr
                       node/expr/op/sing
                       checker-hash
                       (expression-type (list (list 'Int)) 'one
                        (get-temporal-variance run-or-state expr quantvars args checker-hash) (list 'Int))
                       #f)])) 
  RESULT)

(define/contract (check-closure lst)
  (@-> (listof (listof symbol?)) (listof (listof symbol?)))
  (define one-more (check-join (list lst lst)))
  (define new-candidate (remove-duplicates (append lst one-more)))  
  (if (equal? (list->set new-candidate) (list->set lst))
      new-candidate
      (check-closure new-candidate)))  

; since join is n-ary, pass *list* of typelist
; a typelist is a (listof (listof symbol?)), the outer list denoting a set of options
; a type is a (listof symbol?) denoting a sequence of primsigs
(define/contract (check-join child-values)
  (@-> (listof (listof (listof symbol?))) (listof (listof symbol?)))
  
  (let ([product-of-children (apply cartesian-product child-values)])
    (remove-duplicates    
     (filter-map
      (lambda (l) (foldl
                   (lambda (nxt sofar)
                     (if (and sofar (equal? (first nxt) (last sofar)))
                         (append (drop-right sofar 1) (drop nxt 1))
                         #f))
                   (first l)
                   (rest l)))
      product-of-children))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Integer-expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Walk any integer-expressions, seeking bad literal values etc.
(define/contract (checkInt run-or-state expr quantvars checker-hash)
  (@-> (or/c Run? State? Run-spec?)
       node/int?
       list?
       hash?
       any)

  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "last-checker: checkInt: ~a~n" expr))

  (match expr
    [(? node/int/constant?)
     ; Literal: check it!
     (check-int-literal run-or-state expr)]
    [(? node/int/op?)
     ; Integer operator: descend into children (which may be relational) to check them.
     (for ([child (node/int/op-children expr)])
       (cond [(node/int? child)
              (checkInt run-or-state child quantvars checker-hash)]
             [(node/expr? child)
              (expression-type-type (checkExpression run-or-state child quantvars checker-hash))]
             [else (void)]))]
    [(? node/int/sum-quant?)
     ; Sum "quantifier": descend into children (one int-expr, multiple decl domains)
     (define decls (node/int/sum-quant-decls expr))
     (let ([new-quantvars (append (map assocify decls) quantvars)])
       (checkInt run-or-state (node/int/sum-quant-int-expr expr) new-quantvars checker-hash))
     (for/fold ([new-quantvars quantvars])
               ([decl decls])
       (define var (car decl))
       (define domain (cdr decl))
       ; Check and throw away result
       (expression-type-type (checkExpression run-or-state domain new-quantvars checker-hash))
       (cons (list var domain) quantvars))
     (void)]))

; Do not check integer literals with respect to bitwidth for these backends
(define UNBOUNDED_INT_BACKENDS '(smtlibtor))

; Is this integer literal safe under the current bitwidth?
(define/contract (check-int-literal run-or-state expr)
  (@-> (or/c Run? State? Run-spec?)
       node/int/constant?
       any)

  (define run-spec (get-run-spec run-or-state))
  (define backend (get-option run-spec 'backend))
  (unless (member backend UNBOUNDED_INT_BACKENDS)
    (define val (node/int/constant-value expr))
    ; Note: get-scope will return number of int atoms, not the range we want. Hence, compute it ourselves.
    (define max-int (sub1 (expt 2 (sub1 (get-bitwidth run-spec)))))
    (define min-int (@- (expt 2 (sub1 (get-bitwidth run-spec)))))
    (when (or (@> val max-int) (@> min-int val))
      (raise-forge-error
       #:msg (format "Integer literal (~a) could not be represented in the current bitwidth (~a through ~a)"
                   val min-int max-int)
       #:context expr))
    (void)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Rackunit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(module+ test
  (require rackunit)
  ;(printf "Testing last-checker~n")
  (check-equal?
   (list->set (check-join '( ((n1 a) (n2 b) (n2 c) (n3 c) (n4 d))
                             ((a x) (k x) (c y) (c z) (e k))
                             ((y n100) (z n200)))))
   (list->set '((n2 n100) (n2 n200) (n3 n100) (n3 n200))))
  
  )
