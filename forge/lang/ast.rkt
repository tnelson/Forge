#lang racket

(require (for-syntax racket/syntax)
         (prefix-in @ racket)
         racket/generic
         "../shared.rkt")

; NOTE: removed @@and, @@or, due to doubt in implementation.
; Does this cause problems?

(provide (except-out (all-defined-out) int< int>)
         (rename-out [int< <] [int> >]))

; Ocelot ASTs are made up of expressions (which evaluate to relations) and
; formulas (which evaluate to booleans).
; All AST structs start with node/. The hierarchy is:
;
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

(define-generics kkcli-str
  [to-kkcli-str kkcli-str])

(define-generics replace
  [replace replace A B])

;; ARGUMENT CHECKS -------------------------------------------------------------

(define (ast-options #:same-arity? [same-arity? #f] #:arity [arity #f]
                    #:min-length [min-length 2] #:max-length [max-length #f]
                    #:join? [join? #f] #:domain? [domain? #f] #:range? [range? #f])
  (list same-arity? arity min-length max-length join? domain? range?))

; options must have been created by ast-options above
(define (check-args op args type? options)
  (define-values
    (same-arity? arity min-length max-length join? domain? range?)
    (apply values options))
  (when (< (length args) min-length)
    (raise-arguments-error op "not enough arguments" "required" min-length "got" (length args)))
  (unless (false? max-length)
    (when (> (length args) max-length)
      (raise-arguments-error op "too many arguments" "maximum" max-length "got" (length args))))
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
          (printf "~a ~n" args)
          (raise-arguments-error op "arguments must have same arity"
                                 "got" arity "and" (node/expr-arity a) ":" args)))))
  (when join?
    (when (<= (apply join-arity (for/list ([a (in-list args)]) (node/expr-arity a))) 0)
      (raise-arguments-error op (format "join would create a relation of arity 0: ~a" args))))
  (when range?
    (unless (equal? (node/expr-arity (cadr args)) 1)
      (raise-arguments-error op "second argument must have arity 1")))
  (when domain?
    (unless (equal? (node/expr-arity (car args)) 1)
      (raise-arguments-error op "first argument must have arity 1"))))

;; EXPRESSIONS -----------------------------------------------------------------

(struct node/expr (arity) #:transparent)

;; PRE-DECLARE NODE/INT --------------------------------------------------------
; Have to do this so I can use the node/int? predicate in the node/expr operators.

(struct node/int () #:transparent)

;; -- EXPRESSION OPERATIONS ----------------------------------------------------

; sym describes what type of operation this is, e.g. '* for multiplication.
; operands is a list of the arguments to the operation.
; and remember, this inherits the arity field from node/expr
(struct node/expr/op node/expr (operator operands) #:transparent
  #:methods gen:kkcli-str
  [(define/generic recur to-kkcli-str)
   (define (to-kkcli-str op)
     (string-append-immutable
      "(" (symbol->string (node/expr/op-operator op))
      (string-join (map recur (node/expr/op-operands op)) " " #:before-first " ")
      ")"))]
  #:methods gen:replace
  [(define (replace op A B)
     (cond
       [(equal? op A) B]
       [else (node/expr/op
              (node/expr-arity op)
              (node/expr/op-operator op)
              (map (curryr replace A B) (node/expr/op-operands op)))]))])

; Creates a function to wrap up some args in an operation of type given by sym.
; Arity-combinator is a function that tells us what the arity of the resulting expression should be, given the arities of the operands.
; type? is used to type-check arguments. Used in check-args, and to determine whether fallback operation should be used.
; options is passed to check-args so it knows what other properties of the arguments to check
(define (make-expr-op operator arity-combinator type? options #:fallback [fallback #f])
  (λ args
    (if (and fallback (not (ormap #|coercible-|# type? args)))
        (fallback args)
        (begin (check-args operator args type? options)
               (let ([arities (for/list ([arg (in-list args)]) (node/expr-arity arg))])
                 (node/expr/op (arity-combinator arities) operator args))))))

(define + (make-expr-op '+ first node/expr? (ast-options #:same-arity? #t) #:fallback @+))
(define - (make-expr-op '- first node/expr? (ast-options #:same-arity? #t) #:fallback @-))
(define & (make-expr-op '& first node/expr? (ast-options #:same-arity? #t)))

(define -> (make-expr-op '-> @+ node/expr? empty))

(define ~ (make-expr-op '~ first node/expr? (ast-options #:min-length 1 #:max-length 1 #:arity 2)))

(define join-arity
  (lambda (args) (@- (apply @+ args)
                     (@* 2 (@- (length args) 1)))))
(define join (make-expr-op 'join join-arity node/expr? (ast-options #:join? #t)))

(define sing (make-expr-op 'sing (const 1) node/int? (ast-options #:min-length 1 #:max-length 1)))

(define ^ (make-expr-op '^ (const 2) node/expr? (ast-options #:min-length 1 #:max-length 1 #:arity 2)))
(define * (make-expr-op '* (const 2) node/expr? (ast-options #:min-length 1 #:max-length 1 #:arity 2) #:fallback @*))

;; -- QUANTIFIER VARS ----------------------------------------------------------

; mult is one of 'one, 'some, 'lone, 'set, default 'one
(struct node/expr/var node/expr (mult origin name)
  #:methods gen:kkcli-str
  [(define (to-kkcli-str var)
     (string-append-immutable
      "v_"
      (symbol->string (node/expr/var-name var))))]
  #:methods gen:replace
  [(define (replace var A B)
     (if (equal? var A) B
         (node/expr/var (node/expr-arity var)
                        (node/expr/var-mult var)
                        (replace (node/expr/var-origin var) A B)
                        (node/expr/var-name var))))])

; Variable declarations contain no more information than the variables themselves, they just have to be printed different.
; So this is a helper function to be used in to-kkcli-str methods.
(define (decl-to-kkcli-str var)
  (unless (node/expr/var? var)
    (raise-argument-error 'decl-to-kkcli-str "node/expr/var?" var))
  (string-append-immutable
   "["
   (symbol->string (node/expr/var-mult var))
   " "
   (to-kkcli-str var)
   ":"
   (to-kkcli-str (node/expr/var-origin var))
   "]"))

;; -- COMPREHENSIONS -----------------------------------------------------------

; decls is just a list of node/expr/var
(struct node/expr/comprehension node/expr (decls formula)
  #:methods gen:kkcli-str
  [(define/generic recur to-kkcli-str)
   (define (to-kkcli-str comp)
     (string-append-immutable
      "{("
      (string-join (map decl-to-kkcli-str (node/expr/comprehension-decls comp)) " ")
      ") "
      (recur node/expr/comprehension-formula comp)
      "}"))]
  #:methods gen:replace
  [(define (replace comp A B)
     (if (equal? comp A) B
         (node/expr/comprehension
          (node/expr-arity comp)
          (map (curryr replace A B) (node/expr/comprehension-decls comp))
          (replace (node/expr/comprehension-formula comp) A B))))])

(define (comprehension decls formula)
  (for ([decl decls])
    (unless (node/expr/var? decl)
      (raise-argument-error 'set "node/expr/var?" decl))
    (unless (equal? (node/expr-arity decl) 1)
      (raise-argument-error 'set "decl of arity 1" decl)))
  (unless (node/formula? formula)
    (raise-argument-error 'set "formula?" formula))
  (node/expr/comprehension (length decls) decls formula))

(define-syntax (set stx)
  (syntax-case stx ()
    [(_ ([var-name origin] ...) pred)
     (syntax/loc stx
       (let* ([var-name (node/expr/var (node/expr-arity origin) origin 'var-name)] ... )
         (comprehension (list var-name ...) pred)))]))

;; -- RELATIONS ----------------------------------------------------------------

(struct node/expr/relation node/expr (name) #:transparent
  #:methods gen:kkcli-str
  [(define (to-kkcli-str rel)
     (string-append-immutable "r_" (symbol->string (node/expr/relation-name rel))))]
  #:methods gen:replace
  [(define (replace rel A B)
     (if (equal? rel A) B
         rel))])

;; -- CONSTANTS ----------------------------------------------------------------

(struct node/expr/constant node/expr (name) #:transparent
  #:methods gen:kkcli-str
  [(define (to-kkcli-str const)
     (symbol->string node/expr/constant-name const))]
  #:methods gen:replace
  [(define (replace const A B)
     (if (equal? const A) B A))])

; SHOULD THESE BE MACROS?
(define none (node/expr/constant 1 'none))
(define univ (node/expr/constant 1 'univ))
(define iden (node/expr/constant 2 'iden))

; Outdated - "parents" must be replaced by typesets
;(define Int (node/expr/relation 1 "Int" '(Int) "univ"))
;(define succ (node/expr/relation 2 "succ" '(Int Int) "CharlieSaysWhatever"))

;; INTS ------------------------------------------------------------------------

; Actually defined above, with node/expr
; (struct node/int () #:transparent)

;; -- INT OPERATORS ------------------------------------------------------------

(struct node/int/op node/int (operator operands) #:transparent
  #:methods gen:kkcli-str
  [(define/generic recur to-kkcli-str)
   (define (to-kkcli-str op)
     (string-append-immutable
      "(" (symbol->string (node/int/op-operator op))
      (string-join (map recur (node/int/op-operands op)) " " #:before-first " ")
      ")"))]
  #:methods gen:replace
  [(define (replace op A B)
     (if (equal? op A) B
         (node/int/op (node/int/op-operator op)
                      (map (curryr replace A B) (node/int/op-operands op)))))])

(define (make-int-op operator type? options #:fallback [fallback #f])
  (λ args
    (if (and fallback (not (ormap type? args)))
        (fallback args)
        (begin (check-args operator args type? options)
               (node/int/op operator args)))))

(define plus (make-int-op '+ node/int? (ast-options #:min-length 2) #:fallback @+))
(define minus (make-int-op '- node/int? (ast-options #:min-length 2) #:fallback @-))
(define times (make-int-op '* node/int? (ast-options #:min-length 2) #:fallback @*))
(define divide (make-int-op '/ node/int? (ast-options #:min-length 2) #:fallback @/))

(define card (make-int-op '|#| node/expr? (ast-options #:min-length 1 #:max-length 1)))
(define sum (make-int-op 'sum node/expr? (ast-options #:min-length 1 #:max-length 1)))

(define remainder (make-int-op '% node/int? (ast-options #:min-length 2 #:max-length 2)))

(define abs (make-int-op 'abs node/int? (ast-options #:min-length 1 #:max-length 1)))
(define sign (make-int-op 'sgn node/int? (ast-options #:min-length 1 #:max-length 1)))

#| (define (max s-int)
  (sum (- s-int (join (^ succ) s-int))))
(define (min s-int)
  (sum (- s-int (join s-int (^ succ))))) |#

;(define-int-op max node/expr? #:min-length 1 #:max-length 1)
;(define-int-op min node/expr? #:min-length 1 #:max-length 1)

;; -- INT CONSTANTS ------------------------------------------------------------

(struct node/int/constant node/int (value) #:transparent
  #:methods gen:kkcli-str
  [(define (to-kkcli-str const)
     (number->string node/int/constant-value const))]
  #:methods gen:replace
  [(define (replace const A B)
     (if (equal? const A) B A))])

;; -- sum quantifier -----------------------------------------------------------

(struct node/int/quant_sum node/int (decls int-expr)
  #:methods gen:kkcli-str
  [(define/generic recur to-kkcli-str)
   (define (to-kkcli-str quant)
     (string-append-immutable
      "{("
      (string-join (map decl-to-kkcli-str (node/int/sum-quant-decls quant)) " ")
      ") "
      (recur node/int/sum-quant-int-expr quant)
      "}"))]
  #:methods gen:replace
  [(define (replace quant A B)
     (if (equal? quant A) B
         (node/int/sum-quant (map (curryr replace A B) (node/int/sum-quant-decls quant))
                             (replace (node/int/sum-quant-int-expr quant) A B))))])

(define (sum-quant-expr decls int-expr)
  (for ([decl decls])
    (unless (node/expr/var? decl)
      (raise-argument-error 'sum "node/expr/var?" decl))
    (unless (equal? (node/expr-arity decl) 1)
      (raise-argument-error 'sum "decl of arity 1" decl)))
  (unless (node/int? int-expr)
    (raise-argument-error 'sum "int-expr?" formula))
  (node/int/sum-quant decls int-expr))

(define-syntax (sum stx)
  (syntax-case stx ()
    [(_ ([var-name origin] ...) pred)
     (syntax/loc stx
       (let* ([var-name (node/expr/var (node/expr-arity origin) origin 'var-name)] ... )
         (comprehension (list var-name ...) pred)))]))

;; FORMULAS --------------------------------------------------------------------

(struct node/formula () #:transparent)

;; -- constants ----------------------------------------------------------------

(struct node/formula/constant node/formula (name) #:transparent
  #:methods gen:kkcli-str
  [(define (to-kkcli-str const)
     (symbol->string (node/formula/constant-name const)))]
  #:methods gen:replace
  [(define (replace const A B)
     (if (equal? const A) B A))])

(define true (node/formula/constant 'true))
(define false (node/formula/constant 'false))

;; -- operators ----------------------------------------------------------------

(struct node/formula/op node/formula (operator operands) #:transparent
  #:methods gen:kkcli-str
  [(define/generic recur to-kkcli-str)
   (define (to-kkcli-str op)
     (string-append-immutable
      "(" (symbol->string (node/formula/op-operator op))
      (string-join (map recur (node/formula/op-operands op)) " " #:before-first " ")
      ")"))])

(define (make-formula-op operator type? options #:fallback [fallback #f])
  (λ args
    (if (and fallback (not (ormap type? args)))
        (fallback args)
        (begin (check-args operator args type? options)
               (node/formula/op operator args)))))

(define in (make-formula-op 'in node/expr? (ast-options #:min-length 2)))
(define = (make-formula-op '= node/expr? (ast-options #:same-arity? #t #:max-length 2)))

(define ordered (make-formula-op 'ordered node/expr? (ast-options #:max-length 2)))

(define && (make-formula-op '&& node/formula? (ast-options #:min-length 1)))
(define || (make-formula-op '|| node/formula? (ast-options #:min-length 1)))

(define ! (make-formula-op '! node/formula? (ast-options #:min-length 1 #:max-length 1)))
(define not !)

(define => (make-formula-op '=> node/formula? (ast-options #:min-length 2 #:max-length 2)))
(define int> (make-formula-op 'int> node/int? (ast-options #:min-length 2 #:max-length 2)))
(define int< (make-formula-op 'int< node/int? (ast-options #:min-length 2 #:max-length 2)))
(define int= (make-formula-op 'int= node/int? (ast-options #:min-length 2 #:max-length 2)))

; Are these necessary? Can they be replaced / rephrased?
; Yeah i don't like these. they privilege the first argument in a weird way.
#| (define-syntax (@@and stx)
  (syntax-case stx ()
    [(_) (syntax/loc stx true)] ;#t?
    [(_ a0 a ...)
     (syntax/loc stx
       (let ([a0* a0])
         (if (node/formula? a0*)
             (&& a0* a ...)
             (and a0* a ...))))]))

(define-syntax (@@or stx)
  (syntax-case stx ()
    [(_) (syntax/loc stx false)]
    [(_ a0 a ...)
     (syntax/loc stx
       (let ([a0* a0])
         (if (node/formula? a0*)
             (|| a0* a ...)
             (or a0* a ...))))])) |#

;; -- QUANTIFIED FORMULAS ------------------------------------------------------

;symbol table not working
; why is it using the wrong version of to-kkcli-str?

(struct node/formula/quantified node/formula (quantifier decls formula)
  #:methods gen:kkcli-str
  [(define/generic recur to-kkcli-str)
   (define (to-kkcli-str quant)
     (string-append-immutable
      "("
      (symbol->string (node/formula/quantified-quantifier quant))
      " ("
      (string-join (map decl-to-kkcli-str (node/formula/quantified-decls quant)) " ")
      ") "
      (recur (node/formula/quantified-formula quant))
      ")"))]
  #:methods gen:replace
  [(define (replace quant A B)
     (if (equal? quant A) B
         (node/formula/quantified
          (node/formula/quantified-quantifier quant)
          (map (curryr replace A B) (node/formula/quantified-decls quant))
          (replace (node/formula/quantified-formula quant) A B))))])

(define (quantified-formula quantifier decls formula)
  (for ([e (in-list (map cdr decls))])
    (unless (node/expr? e)
      (raise-argument-error quantifier "expr?" e))
    #'(unless (equal? (node/expr-arity e) 1)
        (raise-argument-error quantifier "decl of arity 1" e)))
  (unless (or (node/formula? formula) (equal? #t formula))
    (raise-argument-error quantifier "formula?" formula))
  (node/formula/quantified quantifier decls formula))

;(struct node/formula/higher-quantified node/formula (quantifier decls formula))
; I think doing the macro -> define quant var decl, then proceed with quantified expression, is the best way forward.
; don't ask me why, just intuition.

; Ah shit wait I sorta need this decl information in two places? where and why. ok, I need it up front in the wrapping ast object, so that it can be printed out.
; I also want it deeper in the formula, so that it can be arity- and type-checked. And that requires that, deeper in the formula,
; there's this actual variable with an arity and type, so that rules out just doing symbolic replacement.
; Should decls be different from vars? not really, I don't think.
; So, in general, they're just vars. And I have to state up front some information about the vars contained within, so they can be printed properly. That's the "decls"
;; -- multiplicities -----------------------------------------------------------

; mult is a symbol like 'one or 'some
(struct node/formula/multiplicity node/formula (mult expr)
  #:methods gen:kkcli-str
  [(define/generic recur to-kkcli-str)
   (define (to-kkcli-str mult)
     (string-append-immutable
      "("
      (symbol->string (node/formula/multiplicity-mult mult))
      " "
      (recur (node/formula/multiplicity-expr mult))
      ")"))]
  #:methods gen:replace
  [(define (replace mult A B)
     (if (equal? mult A) B
         (node/formula/multiplicity
          (node/formula/multiplicity-mult mult)
          (replace (node/formula/multiplicity-expr) A B))))])

(define (multiplicity-formula mult expr)
  (unless (node/expr? expr)
    (raise-argument-error mult "expr?" expr))
  (node/formula/multiplicity mult expr))

(define-syntax (all stx) ;#'(quantified-formula 'all (list 'v0 'e0) true)
  (syntax-case stx ()
    [(_ ([v0 e0] ...) pred)
     ; need a with syntax????
     (syntax/loc stx
       (let* ([v0 (node/expr/quantifier-var (node/expr-arity e0) 'v0)] ...)
         (quantified-formula 'all (list (cons v0 e0) ...) pred)))]))

(define-syntax (some stx)
  (syntax-case stx ()
    [(_ ([v0 e0] ...) pred)
     (syntax/loc stx
       (let* ([v0 (node/expr/quantifier-var (node/expr-arity e0) 'v0)] ...)
         (quantified-formula 'some (list (cons v0 e0) ...) pred)))]
    [(_ expr)
     (syntax/loc stx
       (multiplicity-formula 'some expr))]))

(define-syntax (no stx)
  (syntax-case stx ()
    [(_ ([v0 e0] ...) pred)
     (syntax/loc stx
       (let* ([v0 (node/expr/quantifier-var (node/expr-arity e0) 'v0)] ...)
         (! (quantified-formula 'some (list (cons v0 e0) ...) pred))))]
    [(_ expr)
     (syntax/loc stx
       (multiplicity-formula 'no expr))]))

(define-syntax (one stx)
  (syntax-case stx ()
    [(_ ([x1 r1] ...) pred)
     (syntax/loc stx
       (multiplicity-formula 'one (set ([x1 r1] ...) pred)))]
    [(_ expr)
     (syntax/loc stx
       (multiplicity-formula 'one expr))]))

(define-syntax (lone stx)
  (syntax-case stx ()
    [(_ ([x1 r1] ...) pred)
     (syntax/loc stx
       (multiplicity-formula 'lone (set ([x1 r1] ...) pred)))]
    [(_ expr)
     (syntax/loc stx
       (multiplicity-formula 'lone expr))]))

; sum quantifier macro
(define-syntax (sum-quant stx)
  (syntax-case stx ()
    [(_ ([x1 r1] ...) int-expr)
     (syntax/loc stx
       (let* ([x1 (node/expr/quantifier-var (node/expr-arity r1) 'x1)] ...)
         (sum-quant-expr (list (cons x1 r1) ...) int-expr)))]))

;; PREDICATES ------------------------------------------------------------------

; Operators that take only a single argument
(define (unary-op? op)
  (@member op (list ~ ^ * sing ! not sum card abs sign)))
(define (binary-op? op)
  (@member op (list in = => int= int> int< remainder)))
(define (nary-op? op)
  (@member op (list + - & -> join && || plus minus times divide)))
