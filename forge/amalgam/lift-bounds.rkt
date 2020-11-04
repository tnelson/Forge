#lang racket

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
(require "../lang/ast.rkt" (prefix-in @ racket))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Only Expression and IntExpression cases needed
; (we never try to lift bounds of a formula, because that makes no sense.)

(define (lift-bounds-expr expr quantvars)  
  (match expr
    ; relation name (base case)
    [(node/expr/relation arity name typelist parent)
     expr]
    ; The Int constant
    [(node/expr/constant 1 'Int)
     expr]
    ; other expression constants
    [(node/expr/constant arity type)
     expr]
    
    ; expression w/ operator (union, intersect, ~, etc...)
    [(node/expr/op arity args)
     (lift-bounds-expr-op expr quantvars args)]
    
    ; quantified variable (depends on scope! which quantifier is this var for?)
    [(node/expr/quantifier-var arity sym)     
     ;;(print-cmd-cont (symbol->string (v (get-var-idx expr quantvars))))
     (printf "  ~a~n" sym)]
    
    ; set comprehension e.g. {n : Node | some n.edges}
    [(node/expr/comprehension len decls form)     
     (define vars (map car decls)) ; account for multiple variables  
     (let ([quantvars (append vars quantvars)])             
       ; {x: e1, y: e2 | ...}
       ; then UB(e1)->UB(e2) is the UB of the whole comprehension
       ;  where -> is product in Alloy notation
       (define uppers
         (map (lambda (d) ; each declaration                                      
                   ; Decl is (varname . domain-expr), so only need second thing
                   ; *pair*, not *list* 
                   (define ub (lift-bounds-expr (cdr d) quantvars))
                   (printf "    decl: ~a had UB =~a~n" d ub))
                 decls))
       ; TN: unsure if this works to create n-ary products or if we need to chain
       (if (equal? (length uppers) 1)
           (first uppers)
           (node/expr/op/-> (length uppers) uppers)))]))

(define (lift-bounds-expr-op expr quantvars args)
  (match expr

    ; SET UNION 
    [(? node/expr/op/+?)
	(printf "+~n")
	; The upper bound of the LHS and RHS is just the addition between both bounds  
	(define uppers 
          (map (lambda (arg)
              (define ub (lift-bounds-expr arg quantvars))
              (printf "    arg: ~a had UB =~a~n" arg ub))
            args))
	  (if (equal? (length uppers) 1)
           (first uppers)
           (node/expr/op/+ (length uppers) uppers))
     ]
    
    ; SET MINUS 
    [(? node/expr/op/-?)
     (printf "-~n")
     ; Don't confuse semantics with upper bounds. 
     ; Upper bound of A-B is A's upper bound (in case B empty).
     (define ub (lift-bounds-expr (first args) quantvars))
     (printf "    arg: ~a had UB =~a~n" (first args) ub)
     (node/expr/op/- 1 ub))
     ]

    ; SET INTERSECTION
    [(? node/expr/op/&?)
     ;(printf "& ~a~n" expr)
     (define children (map (lambda (x) (lift-bounds-expr x quantvars)) args))
     ; first argument of & struct is the arity, second is the child expressions
     (node/expr/op/& (length children) children)
     ]

    ; PRODUCT
    [(? node/expr/op/->?)
     (printf "->~n")
     (map (lambda (x) (lift-bounds-expr x quantvars)) args)
     ]

    ; JOIN
    [(? node/expr/op/join?)
     (printf ".~n")
     (map (lambda (x) (lift-bounds-expr x quantvars)) args)
     ]

    ; TRANSITIVE CLOSURE
    [(? node/expr/op/^?)
     (printf "^~n")
     (map (lambda (x) (lift-bounds-expr x quantvars)) args)
     ]

    ; REFLEXIVE-TRANSITIVE CLOSURE 
    [(? node/expr/op/*?)
     (printf "*~n")
     (map (lambda (x) (lift-bounds-expr x quantvars)) args)
     ]

    ; TRANSPOSE 
    [(? node/expr/op/~?)
     (printf "~~~n")
     (map (lambda (x) (lift-bounds-expr x quantvars)) args)
     ]

    ; SINGLETON (typecast number to 1x1 relation with that number in it)
    [(? node/expr/op/sing?)
     (printf "sing~n")
     (map (lambda (x) (lift-bounds-int x quantvars)) args)
     ]))

(define (lift-bounds-int expr quantvars)
  (match expr
    ; constant int
    [(node/int/constant value)
     (printf "~a~n" value)]
    
    ; apply an operator to some integer expressions
    [(node/int/op args)   
     (lift-bounds-int-op expr quantvars args)]
    
    ; sum "quantifier"
    ; e.g. sum p : Person | p.age  
    [(node/int/sum-quant decls int-expr)
     (printf "sumQ~n")
     (define var (car (car decls)))
     (let ([quantvars (cons var quantvars)])
       ;( print-cmd-cont (format "(sum ([~a : ~a " 
       ;                         (v (get-var-idx var quantvars))
       ;                         (if (@> (node/expr-arity var) 1) "set" "one")))
       (lift-bounds-expr (cdr (car decls)) quantvars)
       
       (lift-bounds-int int-expr quantvars)
       )]))

(define (lift-bounds-int-op expr quantvars args)
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
     (map (lambda (x) (lift-bounds-expr x quantvars)) args)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Node  (declare-relation '(univ) 'univ "Node"))
(define edges (declare-relation '(Node Node) 'Node "edges"))
(define f-symmetric (= edges (~ edges)))
(define f-irreflexive (no (& edges iden)))
(define f-some-reaches-all (some ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))




