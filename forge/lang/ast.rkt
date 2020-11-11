#lang racket

(require (for-syntax racket/syntax syntax/srcloc)
         syntax/srcloc (prefix-in @ racket) (prefix-in $ racket))

(provide (except-out (all-defined-out) next-name @@and @@or @@not int< int>)
         (rename-out [@@and and] [@@or or] [@@not not] [int< <] [int> >]))

; Forge's AST is based on Ocelot's AST, with modifications.

; Ocelot ASTs are made up of expressions (which evaluate to relations) and
; formulas (which evaluate to booleans).
; All AST structs start with node/. The hierarchy is:
;  * node -- holds basic info like source location (added for Forge)
;   * node/expr  -- expressions
;     * node/expr/op  -- simple operators
;       * node/expr/+
;       * node/expr/-
;       * ...
;     * node/expr/comprehension  -- set comprehension
;     * node/expr/relation  -- leaf relation
;     * node/expr/constant  -- relational constant
;   * node/formula  -- formulas
;     * node/formula/op  -- simple operators
;       * node/formula/and
;       * node/formula/or
;       * ...
;     * node/formula/quantified  -- quantified formula
;     * node/formula/multiplicity  -- multiplicity formula

;; -----------------------------------------------------------------------------

; Group information in one struct to make change easier
(struct nodeinfo (loc))

; Base node struct, should be ancestor of all AST node types
(struct node (info) #:transparent)

(define empty-nodeinfo (nodeinfo (build-source-location #f)))

;; ARGUMENT CHECKS -------------------------------------------------------------

(define (check-args loc op args type?
                    #:same-arity? [same-arity? #f] #:arity [arity #f]
                    #:min-length [min-length 2] #:max-length [max-length #f]
                    #:join? [join? #f] #:domain? [domain? #f] #:range? [range? #f])
  (when (< (length args) min-length)
    (raise-arguments-error op "not enough arguments" "required" min-length "got" args))
  (unless (false? max-length)
    (when (> (length args) max-length)
      (raise-arguments-error op "too many arguments" "maximum" max-length "got" args)))
  (for ([a (in-list args)])
    (unless (type? a)
      (raise-argument-error op (~v type?) a))
    (unless (false? arity)
      (unless (equal? (node/expr-arity a) arity)
        (raise-argument-error op (format "expression with arity ~v" arity) a))))
  (when same-arity?
    (let ([arity (node/expr-arity (car args))])
      (for ([a (in-list args)])
        (unless (equal? (node/expr-arity a) arity)      
          (raise-arguments-error op "arguments must have same arity"
                                 "got" arity "and" (node/expr-arity a) ":" args)))))
  (when join?
    (when (<= (apply join-arity (for/list ([a (in-list args)]) (node/expr-arity a))) 0)
      (printf "Error loc: ~a Args: ~a~n" loc args)
      (printf "Arg locs ~a~n" (map (lambda (a) (nodeinfo-loc (node-info a))) args))
      (printf "Merged arg locs ~a~n" (apply build-source-location (map (lambda (a) (nodeinfo-loc (node-info a))) args)))
      (raise-arguments-error op (format "join would create a relation of arity 0: ~a" args))))
  (when range?
    (unless (equal? (node/expr-arity (cadr args)) 1)      
      (raise-arguments-error op "second argument must have arity 1")))
  (when domain?
    (unless (equal? (node/expr-arity (car args)) 1)      
      (raise-arguments-error op "first argument must have arity 1"))))


;; EXPRESSIONS -----------------------------------------------------------------

; Previously, this would be: 
;(foldl @join rel vars))   ; rel[a][b]...  (taken from breaks.rkt)
; or (foldl join r sigs) (taken from below)
(define (build-box-join sofar todo)
  (cond [(empty? todo) sofar]
        [else
         (build-box-join (join (first todo) sofar) (rest todo))]))

(struct node/expr node (arity) #:transparent
  #:property prop:procedure (λ (r . sigs) (build-box-join r sigs))
)

;; -- operators ----------------------------------------------------------------

(struct node/expr/op node/expr (children) #:transparent)

; lifted operators are defaults, for when the types aren't as expected
(define-syntax (define-node-op stx)
  (syntax-case stx ()
    [(_ id parent arity checks ... #:lift @op #:type childtype)
     ;(printf "defining: ~a~n" stx)
     (with-syntax ([name (format-id #'id "~a/~a" #'parent #'id)]
                   [ellip '...]) ; otherwise ... is interpreted as belonging to the outer macro
       (syntax/loc stx
         (begin
           (struct name parent () #:transparent #:reflection-name 'id)
           (define-syntax (id stx2)                     
             (syntax-case stx2 ()                              
               [(_ e ellip)
                (quasisyntax/loc stx2
                  ;(printf "in created macro, arg location: ~a~n" (build-source-location stx2))
                  (begin
                    ; Keeping this comment in case these macros need to be maintained:
                    ; This line will cause a bad syntax error without any real error-message help.
                    ; The problem is that "id" is the thing defined by this define-stx
                    ;   so it'l try to expand (e.g.) "~" by itself. Instead of "id" use " 'id ".
                    ;(printf "debug2, in ~a: ~a~n" id (list e ellip))
                    
                    ; allow to work with a list of args or a spliced list e.g. (+ 'univ 'univ) or (+ (list univ univ)).                   
                    (let* ([args-raw (list e ellip)]
                           [args (cond
                                   [(or (not (equal? 1 (length args-raw)))
                                        (not (list? (first args-raw)))) args-raw]
                                   [else (first args-raw)])])
                      (if ($and @op (for/and ([a (in-list args)]) ($not (childtype a))))
                          (apply @op args)
                          (begin
                            (check-args #,(build-source-location stx2) 'id args childtype checks ...)
                            (if arity
                                ; expression
                                (if (andmap node/expr? args)
                                    ; expression with expression children (common case)
                                    (let ([arities (for/list ([a (in-list args)]) (node/expr-arity a))])
                                      (name (nodeinfo #,(build-source-location stx2)) (apply arity arities) args))
                                    ; expression with non-expression children or const arity (e.g., sing)
                                    (name (nodeinfo #,(build-source-location stx2)) (arity) args))
                                ; intexpression or formula
                                (name (nodeinfo #,(build-source-location stx2)) args)))))))])))))] 
    [(_ id parent arity checks ... #:lift @op)
     (printf "Warning: ~a was defined without a child type; defaulting to node/expr?~n" (syntax->datum #'id))
     (syntax/loc stx
       (define-node-op id parent arity checks ... #:lift @op #:type node/expr?))]
    [(_ id parent arity checks ... #:type childtype)    
     (syntax/loc stx
       (define-node-op id parent arity checks ... #:lift #f #:type childtype))]
    [(_ id parent arity checks ...)     
     (syntax/loc stx
       (define-node-op id parent arity checks ... #:lift #f))]))

(define get-first
  (lambda e (car e)))
(define get-second
  (lambda e (cadr e)))
(define-syntax-rule (define-op/combine id @op)
  (define-node-op id node/expr/op get-first #:same-arity? #t #:lift @op #:type node/expr?))

(define-op/combine + @+)
(define-op/combine - @-)
(define-op/combine & #f)

(define-syntax-rule (define-op/cross id)
  (define-node-op id node/expr/op @+ #:type node/expr?))
(define-op/cross ->)

(define-node-op ~ node/expr/op get-first #:min-length 1 #:max-length 1 #:arity 2 #:type node/expr?)

(define join-arity
  (lambda e (@- (apply @+ e) (@* 2 (@- (length e) 1)))))
(define-syntax-rule (define-op/join id)
  (define-node-op id node/expr/op join-arity #:join? #t #:type node/expr?))
(define-op/join join)

(define-node-op <: node/expr/op get-second #:max-length 2 #:domain? #t #:type node/expr?)
(define-node-op :> node/expr/op get-first  #:max-length 2 #:range? #t #:type node/expr?)
(define-node-op sing node/expr/op (const 1) #:min-length 1 #:max-length 1 #:type node/int?)

(define-syntax-rule (define-op/closure id @op)
  (define-node-op id node/expr/op (const 2) #:min-length 1 #:max-length 1 #:arity 2 #:lift @op #:type node/expr?))
(define-op/closure ^ #f)
(define-op/closure * @*)

;; -- quantifier vars ----------------------------------------------------------

(struct node/expr/quantifier-var node/expr (sym) #:transparent)

;; -- comprehensions -----------------------------------------------------------

(struct node/expr/comprehension node/expr (decls formula)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "(comprehension ~a ~a ~a)" 
              (node/expr-arity self) 
              (node/expr/comprehension-decls self)
              (node/expr/comprehension-formula self)))])
(define (comprehension info decls formula)
  (for ([e (map cdr decls)])
    (unless (node/expr? e)
      (raise-argument-error 'set "expr?" e))
    (unless (equal? (node/expr-arity e) 1)
      (raise-argument-error 'set "decl of arity 1" e)))
  (unless (node/formula? formula)
    (raise-argument-error 'set "formula?" formula))
  (node/expr/comprehension info (length decls) decls formula))

(define-syntax (set stx)
  (syntax-case stx ()
    [(_ ([r0 e0] ...) pred)
     (quasisyntax/loc stx
       (let* ([r0 (node/expr/quantifier-var (nodeinfo #,(build-source-location stx)) (node/expr-arity e0) 'r0)] ... )
         (comprehension (nodeinfo #,(build-source-location stx)) (list (cons r0 e0) ...) pred)))]))

;; -- relations ----------------------------------------------------------------

(struct node/expr/relation node/expr (name typelist parent) #:transparent #:mutable
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (match-define (node/expr/relation info arity name typelist parent) self)
     (fprintf port "(relation ~a ~v ~a ~a)" arity name typelist parent))])
(define next-name 0)
(define (declare-relation typelist parent [name #f])
  (let ([name (if (false? name) 
                  (begin0 (format "r~v" next-name) (set! next-name (add1 next-name)))
                  name)])
    ; TODO TN
    (node/expr/relation empty-nodeinfo (length typelist) name typelist parent)))
(define (relation-arity rel)
  (node/expr-arity rel))
(define (relation-name rel)
  (node/expr/relation-name rel))
(define (relation-typelist rel)
  (node/expr/relation-typelist rel))
(define (relation-parent rel)
  (node/expr/relation-parent rel))

;; -- relations ----------------------------------------------------------------

(struct node/expr/atom node/expr (name) #:transparent #:mutable
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (match-define (node/expr/atom info arity name) self)
     (fprintf port "(atom ~a)" name))])
(define (atom name)
  (node/expr/atom 1 name))
(define (atom-name rel)
  (node/expr/atom-name rel))

;; -- constants ----------------------------------------------------------------

(struct node/expr/constant node/expr (type) #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "~v" (node/expr/constant-type self)))])

; Macros in order to capture source location
; constants
(define-syntax none (lambda (stx) (syntax-case stx ()    
    [val (identifier? (syntax val)) (quasisyntax/loc stx (node/expr/constant (nodeinfo #,(build-source-location stx)) 1 'none))])))
(define-syntax univ (lambda (stx) (syntax-case stx ()    
    [val (identifier? (syntax val)) (quasisyntax/loc stx (node/expr/constant (nodeinfo #,(build-source-location stx)) 1 'univ))])))
(define-syntax iden (lambda (stx) (syntax-case stx ()    
    [val (identifier? (syntax val)) (quasisyntax/loc stx (node/expr/constant (nodeinfo #,(build-source-location stx)) 2 'iden))])))
; relations, not constants
(define-syntax Int (lambda (stx) (syntax-case stx ()    
    [val (identifier? (syntax val)) (quasisyntax/loc stx (node/expr/relation (nodeinfo #,(build-source-location stx)) 1 "Int" '(Int) "univ"))])))
(define-syntax succ (lambda (stx) (syntax-case stx ()    
    [val (identifier? (syntax val)) (quasisyntax/loc stx (node/expr/relation (nodeinfo #,(build-source-location stx)) 2 "succ" '(Int Int) "Int"))])))

;; INTS ------------------------------------------------------------------------

(struct node/int node () #:transparent)

;; -- operators ----------------------------------------------------------------

(struct node/int/op node/int (children) #:transparent)

(define-node-op add node/int/op #f #:min-length 2 #:type node/int?)
(define-node-op subtract node/int/op #f #:min-length 2 #:type node/int?)
(define-node-op multiply node/int/op #f #:min-length 2 #:type node/int?)
(define-node-op divide node/int/op #f #:min-length 2 #:type node/int?)

; id, parent, arity, checks
(define-node-op card node/int/op #f #:min-length 1 #:max-length 1 #:type node/expr?)
(define-node-op sum node/int/op #f #:min-length 1 #:max-length 1 #:type node/expr?)

(define-node-op remainder node/int/op #f #:min-length 2 #:max-length 2 #:type node/int?)
(define-node-op abs node/int/op #f #:min-length 1 #:max-length 1 #:type node/int?)
(define-node-op sign node/int/op #f #:min-length 1 #:max-length 1 #:type node/int?)

(define (max s-int)
  (sum (- s-int (join (^ succ) s-int))))
(define (min s-int)
  (sum (- s-int (join s-int (^ succ)))))

;(define-node-op max node/int/op #:min-length 1 #:max-length 1 #:type node/int?)
;(define-node-op min node/int/op #:min-length 1 #:max-length 1 #:type node/int?)

;; -- constants ----------------------------------------------------------------

(struct node/int/constant node/int (value) #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "~v" (node/int/constant-value self)))])


(define-syntax (int stx)
  (syntax-case stx ()
    [(_ n)
     (quasisyntax/loc stx       
         (node/int/constant (nodeinfo #,(build-source-location stx)) n))]))

;; -- sum quantifier -----------------------------------------------------------
(struct node/int/sum-quant node/int (decls int-expr)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (match-define (node/int/sum-quant info decls int-expr) self)
     (fprintf port "(sum [~a] ~a)"
                   decls
                   int-expr))])

(define (sum-quant-expr info decls int-expr)
  (for ([e (map cdr decls)])
    (unless (node/expr? e)
      (raise-argument-error 'set "expr?" e))
    (unless (equal? (node/expr-arity e) 1)
      (raise-argument-error 'set "decl of arity 1" e)))
  (unless (node/int? int-expr)
    (raise-argument-error 'set "int-expr?" int-expr))
  (node/int/sum-quant info decls int-expr))

;; FORMULAS --------------------------------------------------------------------

(struct node/formula node () #:transparent)

;; -- constants ----------------------------------------------------------------

(struct node/formula/constant node/formula (type) #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "~v" (node/formula/constant-type self)))])

(define-syntax true (lambda (stx) (syntax-case stx ()    
    [val (identifier? (syntax val)) (quasisyntax/loc stx (node/formula/constant (nodeinfo #,(build-source-location stx)) 'true))])))
(define-syntax false (lambda (stx) (syntax-case stx ()    
    [val (identifier? (syntax val)) (quasisyntax/loc stx (node/formula/constant (nodeinfo #,(build-source-location stx)) 'false))])))



;; -- operators ----------------------------------------------------------------

(struct node/formula/op node/formula (children) #:transparent)

(define-node-op in node/formula/op #f  #:same-arity? #t #:max-length 2 #:type node/expr?)

; TODO: what is this for?
(define-node-op ordered node/formula/op #f #:max-length 2 #:type node/expr?)

(define-node-op && node/formula/op #f #:min-length 1 #:lift #f #:type node/formula?)
(define-node-op || node/formula/op #f #:min-length 1 #:lift #f #:type node/formula?)
(define-node-op => node/formula/op #f #:min-length 2 #:max-length 2 #:lift #f #:type node/formula?)
(define-node-op ! node/formula/op #f #:min-length 1 #:max-length 1 #:lift #f #:type node/formula?)
(define-node-op int> node/formula/op #f #:min-length 2 #:max-length 2 #:type node/int?)
(define-node-op int< node/formula/op #f #:min-length 2 #:max-length 2 #:type node/int?)
(define-node-op int= node/formula/op #f #:min-length 2 #:max-length 2 #:type node/int?)

(define (int=-lifter i1 i2)
  (int= i1 i2))

(define-node-op = node/formula/op #f #:same-arity? #t #:max-length 2 #:lift int=-lifter #:type node/expr?) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Lift the "and", "or" and "not" procedures

(define-syntax (@@and stx)
  (syntax-case stx ()
    [(_) (syntax/loc stx true)] ;#t?
    [(_ a0 a ...)
     (syntax/loc stx
       (let ([a0* a0])         
         (if (node/formula? a0*)
             (&& a0* a ...)
             (and a0* a ...))))]))
(define-syntax (@@not stx)
  (syntax-case stx ()    
    [(_ a0)
     (syntax/loc stx
       (let ([a0* a0])
         (if (node/formula? a0*)
             (! a0*)
             (not a0*))))]))
(define-syntax (@@or stx)
  (syntax-case stx ()
    [(_) (syntax/loc stx false)]
    [(_ a0 a ...)
     (syntax/loc stx
       (let ([a0* a0])
         (if (node/formula? a0*)
             (|| a0* a ...)
             (or a0* a ...))))]))

; *** WARNING ***
; Because of the way this is set up, if you're running tests in the REPL on the ast.rkt tab,
; you need to use @@and (etc.) not and (etc.) to construct formulas. If you try to use and, etc.
; you'll get the boolean behavior, which short-circuits. E.g.
; (and (= iden iden) (= univ univ))
; evaluates to 
; (= #<nodeinfo> '('univ 'univ))
; which is the last thing in the list.

;; -- quantifiers --------------------------------------------------------------

(struct node/formula/quantified node/formula (quantifier decls formula)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (match-define (node/formula/quantified info quantifier decls formula) self)
     (fprintf port "(~a [~a] ~a)" quantifier decls formula))])

(define (quantified-formula info quantifier decls formula)  
  (for ([e (in-list (map cdr decls))])
    (unless (node/expr? e)
      (raise-argument-error quantifier "expr?" e))
    #'(unless (equal? (node/expr-arity e) 1)
      (raise-argument-error quantifier "decl of arity 1" e)))
  (unless (or (node/formula? formula) (equal? #t formula))
    (raise-argument-error quantifier "formula?" formula))
  (node/formula/quantified info quantifier decls formula))


;(struct node/formula/higher-quantified node/formula (quantifier decls formula))

;; -- multiplicities -----------------------------------------------------------

(struct node/formula/multiplicity node/formula (mult expr)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (match-define (node/formula/multiplicity info mult expr) self)
     (fprintf port "(~a ~a)" mult expr))])
(define (multiplicity-formula info mult expr)
  (unless (node/expr? expr)
    (raise-argument-error mult "expr?" expr))
  (node/formula/multiplicity info mult expr))


#|(define-syntax (all stx)
  (syntax-case stx ()
    [(_ ([x1 r1] ...) pred)
     (with-syntax ([(rel ...) (generate-temporaries #'(r1 ...))])
       (syntax/loc stx
         (let* ([x1 (declare-relation 1)] ...
                [decls (list (cons x1 r1) ...)])
           (quantified-formula 'all decls pred))))]))|#



; ok something of this form works!
; now to make it actually work.
; ok, search through pred for instances of v0, replace with just the datum.
; then, in kodkod-translate, we keep a list of quantified variables, and if we ever hit one we know what to do.
; and that can only happen in the context of interpret-expr, right? yeah, it always appears in an expression context.
; and in that scenario, we can always just straight print it!

; OK new plan: reader goes through all 
(define-syntax (all stx) ;#'(quantified-formula 'all (list 'v0 'e0) true)
  (syntax-case stx ()
    [(_ ([v0 e0] ...) pred)
     ; need a with syntax???? 
     (quasisyntax/loc stx
       (let* ([v0 (node/expr/quantifier-var (nodeinfo #,(build-source-location stx)) (node/expr-arity e0) 'v0)] ...)
         (quantified-formula (nodeinfo #,(build-source-location stx)) 'all (list (cons v0 e0) ...) pred)))]))

#|(define-syntax (one stx) ;#'(quantified-formula 'all (list 'v0 'e0) true)
  (syntax-case stx ()
    [(_ ([v0 e0]) pred)
     ; need a with syntax????
     (syntax/loc stx
       (let ([v0 (node/expr/quantifier-var 1 'v0)])
         (quantified-formula 'one (list (cons v0 e0)) pred)))]))|#



(define-syntax (some stx)
  (syntax-case stx ()
    [(_ ([v0 e0] ...) pred)     
     (quasisyntax/loc stx
       (let* ([v0 (node/expr/quantifier-var (nodeinfo #,(build-source-location stx)) (node/expr-arity e0) 'v0)] ...)
         (quantified-formula (nodeinfo #,(build-source-location stx)) 'some (list (cons v0 e0) ...) pred)))]
    [(_ expr)
     (quasisyntax/loc stx
       (multiplicity-formula (nodeinfo #,(build-source-location stx)) 'some expr))]))

(define-syntax (no stx)
  (syntax-case stx ()
    [(_ ([v0 e0] ...) pred)
     (quasisyntax/loc stx
       (let* ([v0 (node/expr/quantifier-var (nodeinfo #,(build-source-location stx)) (node/expr-arity e0) 'v0)] ...)
         (! (quantified-formula (nodeinfo #,(build-source-location stx)) 'some (list (cons v0 e0) ...) pred))))]
    [(_ expr)
     (quasisyntax/loc stx
       (multiplicity-formula (nodeinfo #,(build-source-location stx)) 'no expr))]))

(define-syntax (one stx)
  (syntax-case stx ()
    [(_ ([x1 r1] ...) pred)
     (quasisyntax/loc stx
       ; TODO TN: is this right? shouldn't it be a quantified-formula?
       (multiplicity-formula (nodeinfo #,(build-source-location stx)) 'one (set ([x1 r1] ...) pred)))]
    [(_ expr)
     (quasisyntax/loc stx
       (multiplicity-formula (nodeinfo #,(build-source-location stx)) 'one expr))]))

(define-syntax (lone stx)
  (syntax-case stx ()
    [(_ ([x1 r1] ...) pred)
     (quasisyntax/loc stx
       ; TODO TN: is this right? 
       (multiplicity-formula (nodeinfo #,(build-source-location stx)) 'lone (set ([x1 r1] ...) pred)))]
    [(_ expr)
     (quasisyntax/loc stx
       (multiplicity-formula (nodeinfo #,(build-source-location stx)) 'lone expr))]))


; sum quantifier macro
(define-syntax (sum-quant stx)
  (syntax-case stx ()
    [(_ ([x1 r1] ...) int-expr)
     (quasisyntax/loc stx
       (let* ([x1 (node/expr/quantifier-var (nodeinfo #,(build-source-location stx)) (node/expr-arity r1) 'x1)] ...)
         (sum-quant-expr (nodeinfo #,(build-source-location stx)) (list (cons x1 r1) ...) int-expr)))]))