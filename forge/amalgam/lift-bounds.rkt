;#lang racket
#lang forge/core

; Bounds lifting functions for Amalgam
; expression x bounds -> bounds

; Every Kodkod problem has (upper, lower) bounds for every relation.
; Amalgam needs to have safe bounds estimates for every *expression*
;  since I might write some x : A+B, I need to know the upper-bound for
;   A+B in order to turn the quantified formula into a big "or".
; One thing we might do is just use univ (of appropriate arity), but
;  that gets very large and unwieldy quite quickly. E.g.,
;    suppose UB(A) = {A0, A1}, UB(B) = {B0, B1},
;            UB(C) = {C0, C1, C2}, UB(Int) = [-8, ..., 7].
;   Then if we convert the above quantified formula using "univ", we'll build
;    a big "or" with *23* disjuncts, rather than the *4* needed (note C wasn't included).
;  Also, the desugaring algorithm uses upper-bounds in a lot of other places,
;    e.g., "R in Q" becomes a big "and" saying that all possible members of R
;    are in Q (if they are in R).
;
;  We therefore need this function to "lift" the notion of bounds on a relation
;   to arbitrary expressions.

; Adapted from original Amalgam UpperBoundVisitor.java at:
; https://github.com/transclosure/amalgam/blob/master/src/edu/mit/csail/sdg/alloy4compiler/translator/AmalgamUpperBoundVisitor.java

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Warning: ast.rkt exports (e.g.) "and".
; This is the macro that produces an "and" formula!
; To use real Racket and, use @and.
;(require "../lang/ast.rkt" (prefix-in @ racket))
;(require "../sigs.rkt")
(provide lift-bounds-expr)
(require "desugar_helpers.rkt")
(require debug/repl)
;;;;;;;;;;;;;;;;;;;;;;;;;

; Only Expression and IntExpression cases needed
; (we never try to lift bounds of a formula, because that makes no sense.)
;  ... -> list<tuple> i.e., list<list<atom>>
(define (lift-bounds-expr expr quantvars runContext)

  (match expr
    ; relation name (base case)
    [(node/expr/relation info arity name typelist parent)
     
     (define all-bounds (forge:Run-kodkod-bounds runContext)) ; list of bounds objects     
     (define filtered-bounds (filter (lambda (b) (equal? name (forge:relation-name (forge:bound-relation b)))) all-bounds))
     ; return a list-of-lists
     (cond [(equal? (length filtered-bounds) 1) (forge:bound-upper (first filtered-bounds))]
           [else (error (format "lift-bounds-expr on ~a: didn't have a bound for ~a in ~a" expr name all-bounds))])]

    ; The Int constant
    ; this needs to return e.g. '((-4) (-3) (-2) (-1) (0) (1) (2) (3)) for bitwidth=3
    [(node/expr/constant info 1 'Int)
     (define bitwidth (forge:Scope-bitwidth (forge:Run-spec-scope (forge:Run-run-spec runContext))))
     (create-bitwidth-list (- (* bitwidth -1) 1) bitwidth)
     ]

    ; other expression constants
    [(node/expr/constant info arity type)
     (cond
       [(equal? type univ) (map (lambda (x) (list x x)) (forge:Run-atoms runContext))]
       [(equal? type none) '()])]
    
    ; expression w/ operator (union, intersect, ~, etc...)
    [(node/expr/op info arity args)
     (lift-bounds-expr-op expr quantvars args runContext)]
    
    ; quantified variable
    [(node/expr/quantifier-var info arity sym)     
     (printf "  ~a~n" sym)]
    
    ; set comprehension e.g. {n : Node | some n.edges}
    [(node/expr/comprehension info len decls form)     
     (define vars (map car decls)) ; account for multiple variables  
     (let ([quantvars (append vars quantvars)])             
       ; {x: e1, y: e2 | ...}
       ; then UB(e1)->UB(e2) is the UB of the whole comprehension
       ;  where -> is product in Alloy notation
       (define uppers
         (map (lambda (d) ; each declaration                                      
                   ; Decl is (varname . domain-expr), so only need second thing
                   ; *pair*, not *list* 
                   (define ub (lift-bounds-expr (cdr d) quantvars runContext))
                   (printf "    decl: ~a had UB =~a~n" d ub) ub)
                 decls))
     ; Return a list of lists with all of the bounds with the cartesian product
     (map (lambda (ub) (apply append ub)) (apply cartesian-product uppers)))]))


(define (lift-bounds-expr-op expr quantvars args runContext)
  (match expr

    ; SET UNION 
    [(? node/expr/op/+?)
	(printf "+~n")
	; The upper bound of the LHS and RHS is just the addition between both bounds  
	(define uppers 
          (map (lambda (arg)
                 ; we are assuming that lift-bounds-expr returns a list 
                 (define ub (lift-bounds-expr arg quantvars runContext))
                 (printf "    arg: ~a had UB =~a~n" arg ub)
                 ub) args))
        ; We are assuming that uppers is a list of list of list of atoms 
        ; therefore, by calling 'apply', we can convert this into a list of list of atoms. 
        (remove-duplicates (apply append uppers))
     ]
    
    ; SET MINUS 
    [(? node/expr/op/-?)
     (printf "-~n")
     ; Upper bound of A-B is A's upper bound (in case B empty).
     (define ub (lift-bounds-expr (first args) quantvars runContext))
     (printf "    arg: ~a had UB =~a~n" (first args) ub)
     ; return a list-of-lists containing A's upper bound 
     ub]

    ; SET INTERSECTION
    [(? node/expr/op/&?)
     ; map to get the upper bounds
     (define upper-bounds
       (map (lambda (arg)
              (define ub (lift-bounds-expr arg quantvars runContext))
              ub) args))
     ; filter to filter out the LHS only if they are also in upper bounds of RHS
     ; implemented list-member? to check whether x (a list) is a member of (first upper-bound)
     ; member wasn't working because x is a list, not a value. Now we return a list-of-lists,
     ; which is the appropiate return value. 
     (filter (lambda (x) (member x (first upper-bounds))) (apply append (rest upper-bounds)))
     ]

    ; PRODUCT
    [(? node/expr/op/->?)
     (printf "->~n")
     ; the bounds of A->B are Bounds(A) x Bounds(B)
     ; right now uppers contains ((bounds a) (bounds B))
     (define uppers 
        (map (lambda (arg)
              (define ub (lift-bounds-expr arg quantvars runContext))
              (printf "    arg: ~a had UB =~a~n" arg ub)
               ub) args))
     ; Return a list of lists with all of the bounds with the cartesian product
     (map (lambda (ub) (apply append ub)) (apply cartesian-product uppers))
     ]

    ; JOIN
    [(? node/expr/op/join?)
     (printf ".~n")
     ; In order to approach a join with n arguments, we will first do a
     ; binary join and procede with a foldl doing a join on the previous
     ; result of the function
     (cond
       [(< node/expr-arity 2) (error (format ("Join was given expr ~a with arity less than 1") expr))]
       [else
        (define uppers 
        (map (lambda (arg)
              (define ub (lift-bounds-expr arg quantvars runContext))
              (printf "    arg: ~a had UB =~a~n" arg ub)
               ub) args))
        (define currBinaryJoin (zip (first args) (second args)))
        ; we need to remove the first two things from args since we already joined there 
        (foldl (lambda (curr acc) (zip acc curr)) currBinaryJoin (rest (rest args)))
        ]
       )
     ]

    ; TRANSITIVE CLOSURE
    [(? node/expr/op/^?)
     (printf "^~n")
     (map (lambda (x) (lift-bounds-expr x quantvars runContext)) args)
     ]

    ; REFLEXIVE-TRANSITIVE CLOSURE 
    [(? node/expr/op/*?)
     (printf "*~n")
     (map (lambda (x) (lift-bounds-expr x quantvars runContext)) args)
     ]

    ; TRANSPOSE 
    [(? node/expr/op/~?)
     (printf "~~~n")
     (define upper-bounds (map (lambda (x) (lift-bounds-expr x quantvars runContext)) args))
     ; the call to lift-bounds-expr returns a list of lists, so then we just go through the list
     ; and flip the tuples themselves.
     (define transposedBounds (map (lambda (x) (map transposeTup x)) upper-bounds))
     transposedBounds]

    ; SINGLETON (typecast number to 1x1 relation with that number in it)
    [(? node/expr/op/sing?)
     (printf "sing~n")
     (lift-bounds-expr (first expr) quantvars runContext)]))

(define (lift-bounds-int expr quantvars runContext)
  (match expr
    ; constant int
    [(node/int/constant info value)
     (define bitwidth (forge:Scope-bitwidth (forge:Run-spec-scope (forge:Run-run-spec runContext))))
     (create-bitwidth-list (- (* bitwidth -1) 1) bitwidth)]
    
    ; apply an operator to some integer expressions
    [(node/int/op info args)   
     (lift-bounds-int-op expr quantvars args runContext)]
    
    ; sum "quantifier"
    ; e.g. sum p : Person | p.age  
    [(node/int/sum-quant info decls int-expr)
     (printf "sumQ~n")
     (define var (car (car decls)))
     (let ([quantvars (cons var quantvars)])
       ;( print-cmd-cont (format "(sum ([~a : ~a " 
       ;                         (v (get-var-idx var quantvars))
       ;                         (if (@> (node/expr-arity var) 1) "set" "one")))
       (lift-bounds-expr (cdr (car decls)) quantvars runContext)
       
       (lift-bounds-int int-expr quantvars runContext)
       )]))

(define (lift-bounds-int-op expr quantvars args runContext)
  (match expr
    ; int addition
    [(? node/int/op/add?)
     (printf "int+~n")
     (error "amalgam: int + not supported")
     ]
    
    ; int subtraction
    [(? node/int/op/subtract?)
     (printf "int-~n")
     (error "amalgam: int - not supported")
     ]
    
    ; int multiplication
    [(? node/int/op/multiply?)
     (printf "int*~n")
     (error "amalgam: int * not supported")
     ]
    
    ; int division
    [(? node/int/op/divide?)
     (printf "int/~n")
     (error "amalgam: int / not supported")
     ]
    
    ; int sum (also used as typecasting from relation to int)
    ; e.g. {1} --> 1 or {1, 2} --> 3
    [(? node/int/op/sum?)
     (printf "intsum~n")
     (error "amalgam: int sum not supported")
     ]
    
    ; cardinality (e.g., #Node)
    [(? node/int/op/card?)
     (printf "cardinality~n")
     (define bitwidth (forge:Scope-bitwidth (forge:Run-spec-scope (forge:Run-run-spec runContext))))
     (create-bitwidth-list (- (* bitwidth -1) 1) bitwidth)
     ]
    
    ; remainder/modulo
    [(? node/int/op/remainder?)
     (printf "remainder~n")
     (error "amalgam: int % (modulo) not supported")
     ]
    
    ; absolute value
    [(? node/int/op/abs?)
     (printf "abs~n")
     (error "amalgam: int abs not supported")
     ]
    
    ; sign-of 
    [(? node/int/op/sign?)
     (printf "sign~n")
     (error "amalgam: int sign not supported")
     ]  
    ))


