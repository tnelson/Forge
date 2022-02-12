#lang racket

(require forge/lang/deparser)
(require 
  "sigs-structs.rkt"
  "lang/ast.rkt"
  "shared.rkt"
  racket/syntax
  syntax/srcloc
  (prefix-in @ (only-in racket -> >=)))

(provide checkFormula checkExpression primify)
; This function only exists for code-reuse - it's so that we don't
; need to use begin and hash-ref in every single branch of the match
; Given a struct to handle, a hashtable to search for a handler in,
; and an output to return, executes the check in the handler
; then returns the output
(define (check-and-output ast-node to-handle checker-hash output)
  (begin (when (hash-has-key? checker-hash to-handle) ((hash-ref checker-hash to-handle) ast-node))
         output))


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
    
    [(node/formula/op info args)
     (check-and-output formula
                       node/formula/op
                       checker-hash
                       (checkFormulaOp run-or-state formula quantvars args checker-hash))]
        
    [(node/formula/multiplicity info mult expr)
     (check-and-output formula
                       node/formula/multiplicity
                       checker-hash
                       (checkExpression-top run-or-state expr quantvars checker-hash expr))]
    
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
                                      (raise-syntax-error #f
                                                          (format "Shadowing of variable ~a detected. Check for something like \"some x: A | some x : B | ...\"." var)
                                                          (datum->syntax #f var (build-source-location-syntax (nodeinfo-loc info)))))
                                    ; CHECK: recur into domain(s)
                                    (checkExpression run-or-state domain quantvars checker-hash)))
                                decls)
                              ; Extend domain environment
                              (let ([new-quantvars (append (map assocify decls) quantvars)])       
                                ; CHECK: recur into subformula
                                (checkFormula run-or-state subform new-quantvars checker-hash))))]
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
     (check-and-output formula
                       node/formula/op/always
                       checker-hash
                       (for-each (lambda (x) (checkFormula run-or-state x quantvars checker-hash)) args))]
    [(? node/formula/op/eventually?)
     (check-and-output formula
                       node/formula/op/eventually
                       checker-hash
                       (for-each (lambda (x) (checkFormula run-or-state x quantvars checker-hash)) args))]
    [(? node/formula/op/until?)
     (check-and-output formula
                       node/formula/op/until
                       checker-hash
                       (for-each (lambda (x) (checkFormula run-or-state x quantvars checker-hash)) args))]
    [(? node/formula/op/releases?)
     (check-and-output formula
                       node/formula/op/releases
                       checker-hash
                       (for-each (lambda (x) (checkFormula run-or-state x quantvars checker-hash)) args))]
    [(? node/formula/op/next_state?)
     (check-and-output formula
                       node/formula/op/next_state
                       checker-hash
                       (for-each (lambda (x) (checkFormula run-or-state x quantvars checker-hash)) args))]
    
    [(? node/formula/op/historically?)
     (check-and-output formula
                       node/formula/op/historically
                       checker-hash
                       (for-each (lambda (x) (checkFormula run-or-state x quantvars checker-hash)) args))]
    [(? node/formula/op/once?)
     (check-and-output formula
                       node/formula/op/once
                       checker-hash
                       (for-each (lambda (x) (checkFormula run-or-state x quantvars checker-hash)) args))]
    [(? node/formula/op/prev_state?)
     (check-and-output formula
                       node/formula/op/prev_state
                       checker-hash
                       (for-each (lambda (x) (checkFormula run-or-state x quantvars checker-hash)) args))]
    [(? node/formula/op/since?)
     (check-and-output formula
                       node/formula/op/since
                       checker-hash
                       (for-each (lambda (x) (checkFormula run-or-state x quantvars checker-hash)) args))]
    [(? node/formula/op/triggered?)
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
    
    ; INTEGER >
    [(? node/formula/op/int>?)
     (void)]
    ; INTEGER <
    [(? node/formula/op/int<?)
     (void)]
    ; INTEGER =
    [(? node/formula/op/int=?)
     (void)]))

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
              all-primitive-descendants]
             [else (cons (string->symbol (string-append (symbol->string signame) "_remainder"))
                         all-primitive-descendants)])])))

; wrap around checkExpression-mult to provide check for multiplicity, 
; while throwing the multiplicity away in output;
(define (checkExpression run-or-state expr quantvars checker-hash)
  (let ([output (checkExpression-mult run-or-state expr quantvars checker-hash)])
    (car output)))

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
                              (raise-syntax-error #f (format "Variable ~a used but was unbound in overall formula being checked. Bound variables: ~a" sym (map car quantvars) )
                                (datum->syntax #f name (build-source-location-syntax (nodeinfo-loc info)))))
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
                                            ; CHECK: shadowed variables
                                            (when (assoc var quantvars)
                                              (raise-syntax-error #f (format "Shadowing of variable ~a detected. Check for something like \"some x: A | some x : B | ...\"." var)
                                                (datum->syntax #f var (build-source-location-syntax (nodeinfo-loc info)))))
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

    ; prime
    [(? node/expr/op/prime?)
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
     (check-and-output expr
                       node/expr/op/-
                       checker-hash
                       ; A-B should have only 2 children. B may be empty.
                       (cons (checkExpression run-or-state (first args) quantvars checker-hash)
                              #t))]
    
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
                               (raise-syntax-error #f (format "join always results in an empty relation")
                                                 (datum->syntax #f expr (build-source-location-syntax (nodeinfo-loc (node-info expr))))))))
                           (cons join-result
                                 (cdr (first child-values)))))]
    
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
         ; FIX ERROR MESSAGES
         (unless (@>= left-arity 2)
           (raise-user-error (format "++: arguments must have arity at least 2: got arity 1 on line ~a, column ~a, span ~a."
                                     src-line src-col src-span)))
         (when (set-empty? (set-intersect (list->set left-tuples)
                                          (list->set right-tuples)))
           (raise-user-error (format "++: right argument will never override anything in left argument on line ~a, column ~a, span ~a."
                                     src-line src-col src-span)))
         ; ++ has a maximum of two arguments so this should get everything
         (cons (remove-duplicates (append left-tuples right-tuples))
                #t)))]

    ; SINGLETON (typecast number to 1x1 relation with that number in it)
    [(? node/expr/op/sing?)
     (check-and-output expr
                       node/expr/op/sing
                       checker-hash
                       (cons (list (list 'Int)) #t))]))
;  (printf "result for ~a was ~a~n" expr RESULT)
  RESULT)

(define/contract (check-closure lst)
  (@-> (listof (listof symbol?)) (listof (listof symbol?)))
  (define one-more (check-join (list lst lst)))
  (define new-candidate (remove-duplicates (append lst one-more)))  
  (if (equal? (list->set new-candidate) (list->set lst))
      new-candidate
      (check-closure new-candidate)))  

; since join is n-ary, pass *list* of typelist
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

(module+ test
  (require rackunit)
  ;(printf "Testing last-checker~n")
  (check-equal?
   (list->set (check-join '( ((n1 a) (n2 b) (n2 c) (n3 c) (n4 d))
                             ((a x) (k x) (c y) (c z) (e k))
                             ((y n100) (z n200)))))
   (list->set '((n2 n100) (n2 n200) (n3 n100) (n3 n200))))
  
  )
