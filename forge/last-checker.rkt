#lang racket/base

(require 
  forge/sigs-structs
  forge/lang/ast
  forge/shared
  racket/syntax
  syntax/srcloc
  (prefix-in @ (only-in racket -> >= - >))
  racket/list
  racket/match
  (only-in racket/string string-join)
  (except-in racket/set set)
  (only-in racket/contract define/contract or/c listof any))

(provide checkFormula checkExpression primify)

; This function only exists for code-reuse - it's so that we don't
; need to use begin and hash-ref in every single branch of the match
; Given a struct to handle, a hashtable to search for a handler in,
; and an output to return, executes the check in the handler
; then returns the output
(define (check-and-output ast-node to-handle checker-hash output)
  (begin (when (hash-has-key? checker-hash to-handle) ((hash-ref checker-hash to-handle) ast-node))
         output))

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
    (printf "last-checker: checkFormula: ~a~n" formula))
  
  (match formula    
    [(node/formula/constant info type)
     (check-and-output formula node/formula/constant checker-hash #f)]

    [(node/fmla/pred-spacer info name args expanded)
    (define domain-types (for/list ([arg args]) 
                         (checkExpression run-or-state (mexpr-expr (apply-record-domain arg)) quantvars checker-hash)))
    (define arg-types (for/list ([arg args]  [acc (range 0 (length args))])
                      (list (checkExpression run-or-state (apply-record-arg arg) quantvars checker-hash) acc)))
    (for-each 
        (lambda (type) (if (not (member (car (car type)) (list-ref domain-types (cadr type))))
            (raise-forge-error
            #:msg (format "The sig(s) given as an argument to predicate ~a are of incorrect type" name)
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
     (check-and-output formula
                       node/formula/multiplicity
                       checker-hash
                       (checkExpression-top run-or-state expr quantvars checker-hash formula))]
    
    [(node/formula/quantified info quantifier decls subform)
     (check-and-output formula
                       node/formula/quantified
                       checker-hash
                       (begin (for-each
                                (lambda (decl)
                                  (let ([var (car decl)]
                                        [domain (cdr decl)])
                                    ; CHECK: shadowed variables
                                    (when (assoc var quantvars)
                                      (raise-forge-error
                                       #:msg (format "Shadowing of variable ~a detected. Check for something like \"some x: A | some x : B | ...\"." var)
                                       #:context info))
                                    ; CHECK: recur into domain(s)
                                    (checkExpression run-or-state domain quantvars checker-hash)))
                                decls)
                              ; Extend domain environment
                              (let ([new-quantvars (append (map assocify decls) quantvars)])
                                ; CHECK: recur into subformula
                                (checkFormula run-or-state subform new-quantvars checker-hash))))]
    [(node/formula/sealed info)
     (checkFormula run-or-state info quantvars)]
    [else (error (format "no matching case in checkFormula for ~a" (deparse formula)))]))

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
                       (for-each (lambda (x) (checkExpression run-or-state x quantvars checker-hash)) args))]
    
    ; EQUALS 
    [(? node/formula/op/=?)
     (check-and-output formula
                       node/formula/op/=
                       checker-hash
                       (for-each (lambda (x) (checkExpression-top run-or-state x quantvars checker-hash formula)) args))]

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
           (remove-duplicates (flatten (map (lambda (n) (primify run-or-state n)) (cons 'Int (map Sig-name (get-sigs run-or-state))))))]
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

; wrap around checkExpression-mult to provide check for multiplicity, 
; while throwing the multiplicity away in output; DO NOT CALL THIS AS PASSTHROUGH!
(define (checkExpression run-or-state expr quantvars checker-hash)
;(printf "expr: ~a~n" expr)
  (match expr 
    [(? node/int?) (list (list 'Int))]
    [(? integer?) (list (list 'Int))]
    [_ (let ([output (checkExpression-mult run-or-state expr quantvars checker-hash)])
         (car output))]))

; similar except that this is called from a formula, so in bsl 
; it should check that the multiplicity of the overall expr is 1 
(define (checkExpression-top run-or-state expr quantvars checker-hash parent)
  (let ([output (checkExpression-mult run-or-state expr quantvars checker-hash)])
      ; insert check for (first here)
    ;(printf "checkExpression-top; node: ~a  ; mult: ~a" expr (cdr output))
    (when (hash-has-key? checker-hash 'expr-mult) ((hash-ref checker-hash 'expr-mult) expr (cdr output) parent))
    (car output)))

; For expressions, this descent does two things:
;   - collects possible typings of expressions
;   - throws syntax errors as needed (using typing info from children)
(define/contract (checkExpression-mult run-or-state expr quantvars checker-hash)
  (@-> (or/c Run? State? Run-spec?)
       node/expr?
       list?
       hash?
       any)
       ;(listof (listof (listof symbol?)))

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
                       (cons (apply cartesian-product (map primifyThis (typelist-thunk))) 
                             ; TODO refine
                             (and (equal? 1 (node/expr-arity expr)) (Sig-one expr))))]

    ; atom (base case)
    [(node/expr/atom info arity name)
     (check-and-output expr
                       node/expr/atom
                       checker-hash
                       ; overapproximate for now (TODO: get atom's sig)     
                       (cons (map list (primify run-or-state 'univ))
                             #t))]

    [(node/expr/fun-spacer info arity name args result expanded)
       ; be certain to call the -mult version, or the multiplicity will be thrown away.
       (checkExpression-mult run-or-state expanded quantvars checker-hash)]
    
    [(node/expr/ite info arity a b c)
     (check-and-output expr
                       node/expr/ite
                       checker-hash
                       (begin (checkFormula run-or-state a quantvars checker-hash) ; Check condition formula
                              (let ([b-potentials (checkExpression-mult run-or-state b quantvars checker-hash)]
                                    [c-potentials (checkExpression-mult run-or-state c quantvars checker-hash)])
                                ; Might be a or b, we don't know
                                (cons (remove-duplicates (append (car b-potentials) (car c-potentials)))
                                      (and (cdr b-potentials) (cdr c-potentials))))))]
    
    ; The INT Constant
    [(node/expr/constant info 1 'Int)
     (check-and-output expr
                       node/expr/constant
                       checker-hash
                       (cons (list (list 'Int))
                             #t))]

    ; other expression constants
    [(node/expr/constant info arity type)
     (check-and-output expr
                       node/expr/constant
                       checker-hash
                       (cons (let ([prims (primify run-or-state 'univ)])
                              (cond [(equal? arity 1) (map list prims)]
                                    [else (cartesian-product prims prims)]))
                              #t))]
          
    ; expression w/ operator (union, intersect, ~, etc...)
    [(node/expr/op info arity args)
     (check-and-output expr
                       node/expr/op
                       checker-hash
                       (checkExpressionOp run-or-state expr quantvars args checker-hash))]
 
    ; quantifier variable
    [(node/expr/quantifier-var info arity sym name)
     (check-and-output expr
                       node/expr/quantifier-var
                       checker-hash
                       ; Look up in quantvars association list, type is type of domain
                       (cons (if (assoc expr quantvars) ; expr, not sym (decls are over var nodes)
                                 (checkExpression run-or-state (second (assoc expr quantvars)) quantvars checker-hash)
                                 (raise-forge-error
                                  #:msg (format "Variable ~a used, but it was unbound at this point.\
 Often, this means that a sig, field, or helper name is being used as a quantifier variable.\
 It might also mean that the variable domain references the variable itself.\
 For help debugging, the bound variables at this point were: ~a" sym (map car quantvars) )
                                  #:context info))
                             #t))]

    ; set comprehension e.g. {n : Node | some n.edges}
    [(node/expr/comprehension info arity decls subform)
     (check-and-output expr
                       node/expr/comprehension
                       checker-hash
                       (cons (let ([child-values
                                    (map (lambda (decl)
                                          (let ([var (car decl)]
                                                [domain (cdr decl)])
                                            (printf "!!!!! comprehension case; var: ~a; domain: ~a; quantvars: ~a~n" var domain quantvars)
                                            ; CHECK: shadowed variables
                                            (when (assoc var quantvars)
                                              (raise-forge-error
                                               #:msg (format "Shadowing of variable ~a detected. Check for something like \"some x: A | some x : B | ...\"." var)
                                               #:context info))
                                            ; CHECK: recur into domain(s)
                                            (checkExpression run-or-state domain quantvars checker-hash)))
                                        decls)]
                                  ; Extend domain environment
                                  [new-quantvars (append (map assocify decls) quantvars)])
                              ; CHECK: recur into subformula
                              (checkFormula run-or-state subform new-quantvars checker-hash)
                              ; Return type constructed from decls above
                              (map flatten (map append (apply cartesian-product child-values))))
                              #f))]

    [else (error (format "no matching case in checkExpression for ~a" (deparse expr)))]))


(define (keep-only keepers pool)
  (filter (lambda (ele) (member ele keepers)) pool))

(define/contract (checkExpressionOp run-or-state expr quantvars args checker-hash)
  (@-> (or/c Run? State? Run-spec?) node/expr/op? list? (listof (or/c node/expr? node/int?)) hash?   
       any)
       ;(listof (listof symbol?)))

  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "last-checker: checkExpressionOp: ~a~n" expr))

  (define RESULT
  (match expr

    ; prime (temporal mode only)
    [(? node/expr/op/prime?)
     (check-temporal-mode run-or-state expr)
     (check-and-output expr
                       node/expr/op/prime
                       checker-hash
                       (cons (checkExpression run-or-state (first args) quantvars checker-hash) 
                             #t))]

    ; UNION
    [(? node/expr/op/+?)
     (check-and-output expr
                       node/expr/op/+
                       checker-hash
                       (cons (remove-duplicates (apply append
                                                 (map (lambda (x)
                                                        (checkExpression run-or-state x quantvars checker-hash))
                                                      args)))
                              #t))]

    
    ; SETMINUS 
    [(? node/expr/op/-?)
     (begin
       ; A-B should have only 2 children. B may not exist; use A as the bound returned regardless. 
       ; However, if B is present, we must /check/ it anyway (and discard non-error results).
       (when (@> (length args) 1)
         (checkExpression run-or-state (second args) quantvars checker-hash))
       (check-and-output expr
                         node/expr/op/-
                         checker-hash
                         ; A-B should have only 2 children. B may be empty.
                         (cons (checkExpression run-or-state (first args) quantvars checker-hash)
                               #t)))]
    
    ; INTERSECTION
    [(? node/expr/op/&?)
     (check-and-output expr
                       node/expr/op/&
                       checker-hash
                       (cons (foldl
                              (lambda (x acc)
                                (keep-only (checkExpression run-or-state x quantvars checker-hash) acc))
                              (checkExpression run-or-state (first args) quantvars checker-hash)
                              (rest args))
                              #t))]
    
    ; PRODUCT
    [(? node/expr/op/->?)
     (check-and-output expr
                       node/expr/op/->
                       checker-hash
                       (cons (let* ([child-values (map (lambda (x) (checkExpression run-or-state x quantvars checker-hash)) args)]
                                  [result (map flatten (map append (apply cartesian-product child-values)))])       
                            result)
                              #t))]
   
    ; JOIN
    [(? node/expr/op/join?)
     (check-and-output expr
                       node/expr/op/join
                       checker-hash
                       (let* ([child-values (map (lambda (x) (checkExpression-mult run-or-state x quantvars checker-hash)) args)]
                              [join-result (check-join (map car child-values))])
                         (when (@>= (get-verbosity) VERBOSITY_LASTCHECK) 
                           (when (empty? join-result)
                            (if (eq? (nodeinfo-lang (node-info expr)) 'bsl)
                                ((hash-ref checker-hash 'empty-join) expr)
                              (raise-forge-error
                               #:msg (format "Join always results in an empty relation:\
 Left argument of join \"~a\" is in ~a.\
 Right argument of join \"~a\" is in ~a" 
                                             (deparse (first (node/expr/op-children expr)))  
                                             (map (lambda (lst) (string-join (map (lambda (c) (symbol->string c)) lst) " -> " #:before-first "(" #:after-last ")")) (car (first child-values)))
                                             (deparse (second (node/expr/op-children expr)))
                                             (map (lambda (lst) (string-join (map (lambda (c) (symbol->string c)) lst) " -> " #:before-first "(" #:after-last ")")) (car (second child-values))))
                               #:context expr))))
                           (when (and (not (cdr (first child-values))) (eq? (nodeinfo-lang (node-info expr)) 'bsl))
                                ((hash-ref checker-hash 'relation-join) expr args))
                           (cons join-result
                                   (and (cdr (first child-values)) (not (empty? join-result))(equal? 1 (length (first join-result)))))))]
    
    ; TRANSITIVE CLOSURE
    [(? node/expr/op/^?)
     (check-and-output expr
                       node/expr/op/^
                       checker-hash
                       (cons (let* ([child-values (map (lambda (x) (checkExpression run-or-state x quantvars checker-hash)) args)])     
                               (check-closure (first child-values)))
                              #t))]

    ; REFLEXIVE-TRANSITIVE CLOSURE
    [(? node/expr/op/*?)
     (check-and-output expr
                       node/expr/op/*
                       checker-hash
                       ; includes iden, so might contain any arity-2 tuple
                       (cons (let ([prims (primify run-or-state 'univ)])
                              (cartesian-product prims prims))
                              #t))]

    ; TRANSPOSE: ~(r); r must be arity 2. reverse all types of r
    [(? node/expr/op/~?)
     (check-and-output expr
                       node/expr/op/~
                       checker-hash
                       (cons (map reverse (checkExpression run-or-state (first args) quantvars checker-hash))
                              #t))]
    

    ; RELATIONAL OVERRIDE
    ; AST already checks that both arguments have the same arity
    ; Need to check that the left sub-expression has arity at least 2
    ; Need to check the the left and right sub-expressions have the same types
    ; (which is much easier thanks to the AST checking that they have the same arity)
    [(? node/expr/op/++?)
     (let ([left-arity (node/expr-arity (first args))]
           [left-tuples (checkExpression run-or-state (first args) quantvars checker-hash)]
           [right-tuples (checkExpression run-or-state (second args) quantvars checker-hash)]
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
         (cons (remove-duplicates (append left-tuples right-tuples))
                #t)))]

    ; SINGLETON (typecast number to 1x1 relation with that number in it)
    [(? node/expr/op/sing?)
     ; descend into the integer-expression within and confirm no literals are unsafe
     (checkInt run-or-state (first (node/expr/op-children expr)) quantvars checker-hash)
     (check-and-output expr
                       node/expr/op/sing
                       checker-hash
                       (cons (list (list 'Int)) #t))])) 
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
              (checkExpression run-or-state child quantvars checker-hash)]
             [else (void)]))]
    [(? node/int/sum-quant?)
     ; Sum "quantifier": descend into children (one int-expr, multiple decl domains)
     (define decls (node/int/sum-quant-decls expr))
     (let ([new-quantvars (append (map assocify decls) quantvars)])
       (checkInt run-or-state (node/int/sum-quant-int-expr expr) new-quantvars checker-hash))
     (for ([decl decls])
       (define var (car decl))
       (define domain (cdr decl))
       (checkExpression run-or-state domain quantvars checker-hash))]))

; Is this integer literal safe under the current bitwidth?
(define/contract (check-int-literal run-or-state expr)
  (@-> (or/c Run? State? Run-spec?)
       node/int/constant?
       any)
  
  (cond [(not (Run-spec? run-or-state))
         (printf "Warning: integer literals not checked.~n")]
        [else 
         (define val (node/int/constant-value expr))
         ; Note: get-scope will return number of int atoms, not the range we want. Hence, compute it ourselves.
         (define max-int (sub1 (expt 2 (sub1 (get-bitwidth run-or-state)))))
         (define min-int (@- (expt 2 (sub1 (get-bitwidth run-or-state)))))
         (when (or (@> val max-int) (@> min-int val))
           (raise-forge-error
            #:msg (format "Integer literal (~a) could not be represented in the current bitwidth (~a through ~a)"
                          val min-int max-int)
            #:context expr))
         
         (void)]))


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
