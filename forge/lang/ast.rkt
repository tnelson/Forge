#lang racket/base

(require (for-syntax racket/base racket/syntax syntax/srcloc syntax/parse)
         syntax/srcloc 
         racket/contract
         racket/match
         forge/shared ; for verbosity level
         (only-in racket false? empty? first second rest drop const thunk)
         (prefix-in @ (only-in racket + - * < > or <=)))

(provide deparse
         (except-out (all-defined-out) next-name))

(require forge/choose-lang-specific)

; Avoid loading cycle
(require racket/lazy-require)
(lazy-require ["deparse.rkt" (deparse)])

; Forge's AST is based on Ocelot's AST, with modifications.

; Ocelot ASTs are made up of expressions (which evaluate to relations) and
; formulas (which evaluate to booleans).
; All AST structs start with node/. The hierarchy is:
;  * node (info) -- holds basic info like source location (added for Forge)
;   * node/expr (arity) -- expressions
;     * node/expr/fun-spacer -- no-op spacer to record location where a fun substitution was done
;     * node/expr/op (children) -- simple operators
;       * node/expr/op/+
;       * node/expr/op/-
;       * ...
;     * node/expr/comprehension (decls formula)  -- set comprehension
;     * node/expr/relation (name typelist-thunk parent is-variable)  -- leaf relation
;     * node/expr/constant (type) -- relational constant [type serves purpose of name?]
;     * node/expr/quantifier-var (sym name) -- variable for quantifying over
;   * node/formula  -- formulas
;     * node/expr/pred-spacer -- no-op spacer to record location where a pred substitution was done
;     * node/formula/op  -- simple operators
;       * node/formula/op/and
;       * node/formula/op/or
;       * ...
;     * node/formula/quantified   -- quantified formula
;     * node/formula/multiplicity -- multiplicity formula
;     * node/formula/sealed
;       * wheat
;   * node/int -- integer expression
;     * node/int/sum-quant -- sum "quantified" form
;     * node/int/op (children)
;       * node/int/op/add
;       * ...
;     * node/int/constant (value) -- int constant
;; -----------------------------------------------------------------------------

; Struct to hold metadata about an AST node (like source location)
; Group information in one struct to make change easier
(struct nodeinfo (loc lang) #:transparent  
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (match-define (nodeinfo loc lang) self)
     ; hide nodeinfo when printing; don't print anything or this will become overwhelming
     ;(fprintf port "")
     (void))])

(define empty-nodeinfo (nodeinfo (build-source-location #f) 'empty))
(define (just-location-info loc)
  (if loc 
      (nodeinfo loc 'empty)
      (build-source-location #f)))

; Base node struct, should be ancestor of all AST node types
; Should never be directly instantiated
(struct node (info) #:transparent
  ;#:guard (lambda (info name) (printf "creating a node; ~a; ~a~n" (nodeinfo? info) name))
  )

; Should never be directly instantiated
(struct node/expr node (arity) #:transparent
  #:property prop:procedure (λ (r . sigs) (build-box-join r sigs)))

; Should never be directly instantiated
(struct node/int node () #:transparent)

; Should never be directly instantiated
(struct node/formula node () #:transparent
  ; Give a friendlier error if Racket thinks we are asking it to treat a formula as a function
  #:property prop:procedure
  (λ (f . x)
    (cond [(node/fmla/pred-spacer? f)
           (raise-forge-error
            #:msg (format "Tried to give arguments to a predicate, but it takes none: ~a." (node/fmla/pred-spacer-name f))
            #:context f)]
          [else
           (raise-forge-error
            #:msg (format "Could not use formula as a function: ~a." (deparse f))
            #:context f)])))
  

;; ARGUMENT CHECKS -------------------------------------------------------------

(define/contract (intexpr->expr/maybe a-node #:op functionname #:info info)
  (@-> (or/c node? integer?) #:op symbol? #:info nodeinfo? node/expr?)  
  (cond [(node/int? a-node) (node/expr/op/sing (node-info a-node) 1 (list a-node))]
        [(integer? a-node) (intexpr->expr/maybe (int a-node) #:op functionname #:info info)]
        [(node/expr? a-node) a-node]
        [else 
          (define loc (nodeinfo-loc info))
          (define locstr (format "line ~a, col ~a, span: ~a" (source-location-line loc) (source-location-column loc) (source-location-span loc)))    
          (raise-syntax-error functionname 
                              (format "~a operator expected to be given an expression, but instead got ~a at loc: ~a" functionname (node-type a-node) locstr)
                              (datum->syntax #f (deparse a-node) (build-source-location-syntax loc)))]))

(define/contract (expr->intexpr/maybe a-node #:op functionname #:info info)  
  (@-> node? #:op symbol? #:info nodeinfo? node/int?)  
  (cond [(node/expr? a-node) (node/int/op/sum (node-info a-node) (list a-node))]
        [(node/int? a-node) a-node]
        [(integer? a-node) (int a-node)]
        [else         
          (define loc (nodeinfo-loc info))
          (define locstr (format "line ~a, col ~a, span: ~a" (source-location-line loc) (source-location-column loc) (source-location-span loc)))    
          (raise-syntax-error functionname 
                              (format "~a operator expected an expression, but instead got ~a at loc: ~a" functionname (node-type a-node) locstr)
                              (datum->syntax #f (deparse a-node) (build-source-location-syntax loc)))]))

(define (check-args info op args type?
                    #:same-arity? [same-arity? #f] #:arity [arity #f]
                    #:min-length [min-length 2] #:max-length [max-length #f]
                    #:join? [join? #f] #:domain? [domain? #f] #:range? [range? #f])
  (define loc (nodeinfo-loc info))
  (define locstr (format "line ~a, col ~a, span: ~a" (source-location-line loc) (source-location-column loc) (source-location-span loc)))
  
  (when (< (length args) min-length)
    (raise-syntax-error op (format "building ~a; not enough arguments: required ~a got ~a at loc: ~a"
                                   op min-length args locstr)
                        (datum->syntax #f (map deparse args) (build-source-location-syntax loc))))
  (unless (false? max-length)
    (when (> (length args) max-length)
      (raise-syntax-error op (format "too many arguments to ~a; maximum ~a, got ~a at loc: ~a" op max-length args locstr)
                          (datum->syntax #f (map deparse args) (build-source-location-syntax loc)))))
  (for ([a (in-list args)])
    (unless (type? a)
      (raise-syntax-error op (format "argument to ~a had unexpected type. expected ~a, got ~a. loc: ~a" op type? a locstr)
                          (datum->syntax #f (map deparse args) (build-source-location-syntax loc))))
    (unless (false? arity)
      (unless (equal? (node/expr-arity a) arity)
        (raise-syntax-error op (format "argument to ~a was not expression with arity ~v (got: ~a) at loc: ~a" op arity a locstr)
                            (datum->syntax #f (map deparse args) (build-source-location-syntax loc))))))
  (when same-arity?
    (let ([arity (node/expr-arity (car args))])
      (for ([a (in-list args)])
        (unless (equal? (node/expr-arity a) arity)
          (raise-syntax-error op (format "arguments to ~a must have same arity. got ~a and ~a at loc: ~a"
                                         op arity (node/expr-arity a) locstr)
                           (datum->syntax #f (map deparse args) (build-source-location-syntax loc)))))))
  (when join?
    (when (<= (apply join-arity (for/list ([a (in-list args)]) (node/expr-arity a))) 0)
       (raise-syntax-error op (format "join would create a relation of arity 0 at loc: ~a" locstr)
                           (datum->syntax #f (map deparse args) (build-source-location-syntax loc)))))
  
  (when range?
    (unless (equal? (node/expr-arity (cadr args)) 1)      
      (raise-syntax-error op (format "second argument to ~a must have arity 1 at loc: ~a" op locstr)
                          (datum->syntax #f (map deparse args) (build-source-location-syntax loc)))))
  (when domain?
    (unless (equal? (node/expr-arity (car args)) 1)      
      (raise-syntax-error op (format "first argument to ~a must have arity 1 at loc: ~a" op locstr)
                             (datum->syntax #f (map deparse args) (build-source-location-syntax loc))))))

;; EXPRESSIONS -----------------------------------------------------------------

; Previously, this would be: 
;(foldl @join rel vars))   ; rel[a][b]...  (taken from breaks.rkt)
; or (foldl join r sigs) (taken from below)
(define (build-box-join sofar todo)
  (cond [(empty? todo) sofar]
        [else
         (define loc1 (nodeinfo-loc (node-info sofar)))
         (define loc2 (nodeinfo-loc (node-info (first todo))))
         (build-box-join (join/info
                          (nodeinfo (build-source-location loc1 loc2) 'checklangplaceholder)
                                    (first todo) sofar)
                          (rest todo))]))

; Records an expression with multiplicity attached
(struct/contract mexpr ([expr node/expr?] [mult symbol?]) #:transparent)

; Records the substitution of a concrete argument into a formal parameter
; param: the variable substituted out; domain: the domain of the variable; arg: the actual argument
; For robustness, we'll allow the arg to be an "int expr" node or Racket int without forcing conversion.
(struct/contract apply-record ([param symbol?]
                               [domain mexpr?]
                               [arg (or/c node/expr? node/int? integer?)]) #:transparent)

; struct/contract does not support #:methods, but at least add a guard
(struct node/expr/fun-spacer node/expr (name args codomain expanded) #:transparent
  #:guard (lambda (info arity name args codomain expanded structure-type-name)
            (unless (nodeinfo? info)
              (error "node/expr/fun-spacer: info argument should be a nodeinfo structure"))
            (unless (number? arity)
              (error "node/expr/fun-spacer: arity argument should be a number"))
            (unless (symbol? name)
              (error "node/expr/fun-spacer: name argument should be a symbol"))
            (unless (and (list? args)
                         (andmap (lambda (arg) (apply-record? arg)) args))
              (error (format "node/expr/fun-spacer: args argument should be a list of apply-record structures (~a)" args)))
            (unless (mexpr? codomain)
              (error "node/expr/fun-spacer: codomain argument should be a mexpr structure"))
            (unless (node/expr? expanded)
              (error "node/expr/fun-spacer: expanded argument should be a node/expr structure"))
            (values info arity name args codomain expanded))
                   
  ; print invisibly unless verbosity is set to > LOW
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (if (<= (get-verbosity) VERBOSITY_LOW)
         (fprintf port "~a" (node/expr/fun-spacer-expanded self))
         (fprintf port "(node/expr/fun-spacer ~a ~a ~a ~a ~a)"                  
                  (node/expr-arity self)
                  (node/expr/fun-spacer-name self)
                  (node/expr/fun-spacer-args self)
                  (node/expr/fun-spacer-codomain self)
                  (node/expr/fun-spacer-expanded self))))])

;; -- operators ----------------------------------------------------------------

; Should never be directly instantiated
(struct node/expr/op node/expr (children) #:transparent)

;; if-then-else *expression*, which is different from an if-then-else formula
;   The formula version is just sugar, the expression version is a basic expr type
;   that Kodkod/Pardinus understand and is hard to emulate generically.
(struct node/expr/ite node/expr (condition thene elsee) #:transparent #:reflection-name 'ite             
             #:methods gen:equal+hash
             [(define equal-proc (make-robust-node-equal-syntax node/expr))
              (define hash-proc  (make-robust-node-hash-syntax node/expr 0))
              (define hash2-proc (make-robust-node-hash-syntax node/expr 3))]
             #:methods gen:custom-write
             [(define (write-proc self port mode)                
                (fprintf port "~a" (list 'ite (node/expr/ite-condition self)
                                         (node/expr/ite-thene self)
                                         (node/expr/ite-elsee self))))])

(define (ite/info-helper info a orig-b orig-c)  
  (define b (intexpr->expr/maybe orig-b #:op 'if-then-else #:info info))
  (define c (intexpr->expr/maybe orig-c #:op 'if-then-else #:info info))
  
  (unless (node/formula? a)
    (raise-syntax-error #f (format "If-then-else expression requires first argument to be a formula")
                        (datum->syntax #f (deparse a) (build-source-location-syntax (nodeinfo-loc info)))))
  (unless (node/expr? b)
    (raise-syntax-error #f (format "If-then-else expression requires second argument to be an expression")
                               (datum->syntax #f (deparse b) (build-source-location-syntax (nodeinfo-loc info)))))
  (unless (node/expr? c)
    (raise-syntax-error #f (format "If-then-else expression requires third argument to be an expression")
                        (datum->syntax #f (deparse c) (build-source-location-syntax (nodeinfo-loc info)))))
  (unless (equal? (node/expr-arity b) (node/expr-arity c))
    (raise-syntax-error #f (format "If-then-else expression requires expression arguments to have same arity")
                        (datum->syntax #f (deparse c) (build-source-location-syntax (nodeinfo-loc info)))))
  (node/expr/ite info (node/expr-arity b) a b c))

(define-syntax (ite/info stx)
  (syntax-case stx ()
    [(_ info a b c)
     (quasisyntax/loc stx
       (ite/info-helper info a b c))]))

(define-syntax (ite stx)
  (syntax-parse stx
    [(_ (~optional (#:lang check-lang) #:defaults ([check-lang #''checklangNoCheck])) a b c) 
      (quasisyntax/loc stx (ite/info (nodeinfo #,(build-source-location stx) check-lang) a b c))]))

(define (empty-check node) (lambda ()(void)))

(define (check-and-output ast-node to-handle checker-hash info)
  (when (not checker-hash)
    (printf (string-append "No checker-hash given. ast-node was: ~a~n"
                           "Ignoring language-specific checks for this node. (If this was not a formula or expression "
                           "created in the Racket/DrRacket REPL, please report this as a bug.)~n") ast-node))
  ;(printf "checking ast-node:~a  to-handle:~a  has-key? ~a ~n" ast-node to-handle (hash-has-key? checker-hash to-handle))
  (when (and checker-hash (hash-has-key? checker-hash to-handle))
    ((hash-ref checker-hash to-handle) ast-node info)))

; lifted operators are defaults, for when the types aren't as expected
; parent: the node struct type that is the parent of this new one
; arity: the arity of the new node, in terms of the arities of its children
(define-syntax (define-node-op stx)
  (syntax-case stx ()
    [(_ id parent arity checks ... #:lift @op #:type childtype)
     ;(printf "defining: ~a~n" stx)
     (with-syntax ([name (format-id #'id "~a/~a" #'parent #'id)]
                   [parentname (format-id #'id "~a" #'parent)]
                   [functionname (format-id #'id "~a/func" #'id)]
                   [macroname/info-help (format-id #'id "~a/info-help" #'id)]
                   [macroname/info (format-id #'id "~a/info" #'id)]
                   [child-accessor (format-id #'id "~a-children" #'parent)]
                   [key (format-id #'id "~a/~a" #'parent #'id)]
                   [display-id (if (equal? '|| (syntax->datum #'id)) "||" #'id)]
                   [ellip '...]) ; otherwise ... is interpreted as belonging to the outer macro
       (syntax/loc stx
         (begin
           (struct name parent () #:transparent #:reflection-name 'id  
             #:methods gen:equal+hash
             [(define equal-proc (make-robust-node-equal-syntax parentname))
              (define hash-proc  (make-robust-node-hash-syntax parentname 0))
              (define hash2-proc (make-robust-node-hash-syntax parentname 3))]
             #:methods gen:custom-write
             [(define (write-proc self port mode)
                ; all of the /op nodes have their children in a field named "children"
                (fprintf port "~a" (cons 'display-id (child-accessor self))))])
           ; Keep this commented-out line for use in emergencies to debug bad source locations:
           ;(fprintf port "~a" (cons 'display-id (cons (nodeinfo-loc (node-info self)) (child-accessor self))))
           
           (define (functionname #:info [info empty-nodeinfo] . raw-args)
             (define ast-checker-hash (get-ast-checker-hash))
             ;(printf "ast-checker-hash ~a~n" (get-ast-checker-hash))
             ;(printf "name: ~a args: ~a    key:~a ~n" 'name raw-args  key)

             ; Perform intexpr->expr and expr->intexpr coercion if needed:             
             (define args (cond [(equal? node/expr? childtype)
                                 (map (lambda (x) (intexpr->expr/maybe x #:op 'id #:info info)) raw-args)]
                                [(equal? node/int? childtype)
                                 (map (lambda (x) (expr->intexpr/maybe x #:op 'id #:info info)) raw-args)]
                                [else raw-args]))
             
             (check-and-output args key ast-checker-hash info)
             (check-args info 'id args childtype checks ...)
             (if arity
                 ; expression
                 (cond [(andmap node/expr? args)                       
                        ; expression with ~all~ expression children (common case)
                        (let ([arities (for/list ([a (in-list args)]) (node/expr-arity a))])
                          (name info (apply arity arities) args))]
                       ; expression with non-expression children or const arity (e.g., sing or expr form of ITE)                                       
                       [else             
                        (name info (arity) args)]) 
                 ; intexpression or formula
                 (name info args)))
           
           

           ; For expander to use check-lang on this macro, use the format
           ; (id (#:lang (get-check-lang)) args ...) for pattern matching

           ; a macro constructor that captures the syntax location of the call site
           ;  (good for, e.g., test cases + parser)
           (define-syntax (id stx2)
             (syntax-case stx2 ()
                [(_ (#:lang check-lang) e ellip)                
                  (quasisyntax/loc stx2
                    ;(begin
                    ;(printf "arguments:~a ; ~a ; ~a ~n" check-lang e ellip))]
                    (macroname/info (nodeinfo #,(build-source-location stx2) check-lang) e ellip))]
                [(_ e ellip)                
                  (quasisyntax/loc stx2
                    (macroname/info (nodeinfo #,(build-source-location stx2) 'checklangNoCheck) e ellip))]))


           (define (macroname/info-help info args-raw)
             (let* ([args (cond  ; support passing chain of args OR a list of them
                            [(or (not (equal? 1 (length args-raw)))
                                (not (list? (first args-raw)))) args-raw]
                            [else (first args-raw)])])
              (if (and @op (for/and ([a (in-list args)]) (not (childtype a))))
                  (apply @op args)
                  (apply functionname #:info info args))))

           ; a macro constructor that also takes a syntax object to use for location
           ;  (good for, e.g., creating a big && in sigs.rkt for predicates)
           (define-syntax (macroname/info stx2)                     
             (syntax-case stx2 ()                              
               [(_ info e ellip)
                (quasisyntax/loc stx2
                  ;(printf "all args ~a ;   ~a ;  ~a ~n" info e ellip))]))
                  ; (printf "in created macro, arg location: ~a~n" (build-source-location stx2))
                  (begin
           
                    ; Keeping this comment in case these macros need to be maintained:
                    ; This line will cause a bad syntax error without any real error-message help.
                    ; The problem is that "id" is the thing defined by this define-stx
                    ;   so it'l try to expand (e.g.) "~" by itself. Instead of "id" use " 'id ".
                    ;(printf "debug2, in ~a: ~a~n" id (list e ellip))
                    
                    ; allow to work with a list of args or a spliced list e.g. (+ 'univ 'univ) or (+ (list univ univ)).                   
                    (macroname/info-help info (list e ellip))))])))))]
                  ;)))]
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
(define-syntax-rule (define-op/combine id)
  (define-node-op id node/expr/op get-first #:same-arity? #t #:type node/expr?))

(define-op/combine +)
(define-op/combine -)
(define-op/combine &)

(define-syntax-rule (define-op/cross id)
  (define-node-op id node/expr/op @+ #:type node/expr?))
(define-op/cross ->)

(define-node-op ~ node/expr/op get-first #:min-length 1 #:max-length 1 #:arity 2 #:type node/expr?)

(define join-arity
  (lambda e (@- (apply @+ e) (@* 2 (@- (length e) 1)))))
(define-syntax-rule (define-op/join id)
  (define-node-op id node/expr/op join-arity #:join? #t #:type node/expr?))
(define-op/join join)

;(define-node-op <: node/expr/op get-second #:max-length 2 #:domain? #t #:type node/expr?)
;(define-node-op :> node/expr/op get-first  #:max-length 2 #:range? #t #:type node/expr?)
(define-node-op ++ node/expr/op get-first #:same-arity? #t #:min-length 2 #:max-length 2 #:type node/expr?)
(define-node-op sing node/expr/op (const 1) #:min-length 1 #:max-length 1 #:type node/int?)

(define-node-op prime node/expr/op get-first #:min-length 1 #:max-length 1 #:type node/expr?)

(define-syntax-rule (define-op/closure id)
  (define-node-op id node/expr/op (const 2) #:min-length 1 #:max-length 1 #:arity 2 #:type node/expr?))
(define-op/closure ^)
(define-op/closure *)




;; -- quantifier vars ----------------------------------------------------------

(struct node/expr/quantifier-var node/expr (sym name) #:transparent
  #:methods gen:equal+hash
  [(define equal-proc (make-robust-node-equal-syntax node/expr/quantifier-var))
   (define hash-proc  (make-robust-node-hash-syntax node/expr/quantifier-var 0))
   (define hash2-proc (make-robust-node-hash-syntax node/expr/quantifier-var 3))]
  #:methods gen:custom-write
  [(define (write-proc self port mode)     
     (fprintf port "~a" (node/expr/quantifier-var-name self)))])

(define (var [raw-sym #f] #:info [node-info empty-nodeinfo])
  (define sym (@or raw-sym (gensym 'var)))
  (node/expr/quantifier-var node-info
                            1 ; TODO: Allow arbitrary arity?
                            sym sym))

;; -- comprehensions -----------------------------------------------------------

(struct node/expr/comprehension node/expr (decls formula)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "(comprehension ~a ~a)"               
              (node/expr/comprehension-decls self)
              (node/expr/comprehension-formula self)))]
  #:methods gen:equal+hash
  [(define equal-proc (make-robust-node-equal-syntax node/expr/comprehension))
   (define hash-proc  (make-robust-node-hash-syntax node/expr/comprehension 0))
   (define hash2-proc (make-robust-node-hash-syntax node/expr/comprehension 3))])

; Helper to construct a set comprehension (and give useful errors, in case
; these are shown to an end-user)
(define (raise-set-comp-quantifier-error e)
  (raise-forge-error
   #:msg (format "Set-comprehension variable domain expected a singleton or relational expression: ~a" (deparse e))
   #:context e))
(define (comprehension info decls formula)
  (for ([e (map cdr decls)])
    (unless (node/expr? e)
      (raise-set-comp-quantifier-error e))
    (unless (equal? (node/expr-arity e) 1)
      (raise-forge-error
       #:msg (format "Set-comprehension variable domain needs arity = 1: ~a" (deparse e))
       #:context e))
    (unless (node/formula? formula)
      (raise-forge-error
       #:msg (format "Set-comprehension condition expected a formula: ~a" (deparse formula))
       #:context formula)))
  (node/expr/comprehension info (length decls) decls formula))
(define (set/func decls formula #:info [node-info empty-nodeinfo])
  (comprehension node-info decls formula))

(begin-for-syntax
  (define-splicing-syntax-class opt-check-lang-class
    (pattern (~optional ((~datum #:lang) check-lang) #:defaults ([check-lang #''checklangNoCheck]))))
  (define-splicing-syntax-class opt-mult-class
    (pattern (~optional mult #:defaults ([mult #'(if (> (node/expr-arity e0) 1) 'set 'one)])))))

; Macro for forge/core set comprehensions
(define-syntax (set stx)
  (syntax-parse stx
    [((~datum set) check-lang:opt-check-lang-class
                   ([r0 e0 m0:opt-mult-class] ...)
                   pred)
        (quasisyntax/loc stx
          (begin 
            (unless (node/expr? e0)
              (raise-set-comp-quantifier-error e0))
            ...
            (let* ([r0 (node/expr/quantifier-var (nodeinfo #,(build-source-location stx) check-lang.check-lang) (node/expr-arity e0) (gensym (format "~a_set" 'r0)) 'r0)] ... )
              (set/func #:info (nodeinfo #,(build-source-location stx) check-lang.check-lang) (list (cons r0 e0) ...) pred))))]))

;; -- relations ----------------------------------------------------------------

; Do not use this constructor directly, instead use the rel macro or the build-relation procedure.
; The is-variable field allows support for Electrum-style var fields in the core language
; typelist-thunk is a thunk so that relations can be defined before all of the
; sigs they relate are bound (so long as those sigs are bound later)
; this is necessary to allow for mutual references between sigs in surface
(struct node/expr/relation node/expr (name typelist-thunk parent is-variable) #:transparent #:mutable
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (match-define (node/expr/relation info arity name typelist-thunk parent is-variable) self)
     ;(fprintf port "(relation ~a ~v ~a ~a)" arity name typelist parent)
     (fprintf port "(rel ~a)" name))]
  #:methods gen:equal+hash
  [(define equal-proc (make-robust-node-equal-syntax node/expr/relation))
   (define hash-proc  (make-robust-node-hash-syntax node/expr/relation 0))
   (define hash2-proc (make-robust-node-hash-syntax node/expr/relation 3))])
(define next-name 0)

; Is this used anywhere?
; e.g.: (rel '(Node Node) 'Node "edges") to define the usual edges relation
(define-syntax (rel stx)
  (syntax-case stx ()
    [(_ (typelist ...) parent name)
     (quasisyntax/loc stx       
       (build-relation #,(build-source-location stx) (typelist ...) parent name #f))]
    [(_ (typelist ...) parent name isv)
     (quasisyntax/loc stx       
       (build-relation #,(build-source-location stx) (typelist ...) parent name isvar))]))

; Used by rel macro
; pre-functional: *also* used by Sig and relation macros in forge/core (sigs.rkt)
(define (build-relation loc typelist parent [name #f] [is-var #f])
  (let ([name (cond [(false? name) 
                     (begin0 (format "r~v" next-name) (set! next-name (add1 next-name)))]
                     [(symbol? name) (symbol->string name)]
                     [else name])]
        [types (map (lambda (t)
                      (cond                        
                        [(string? t) t]
                        [(symbol? t) (symbol->string t)]
                        [else (error (format "build-relation expected list of strings or symbols: ~a" typelist))])) typelist)]
        [scrubbed-parent (cond [(symbol? parent) (symbol->string parent)]
                               [(string? parent) parent]
                               [else (error (format "build-relation expected parent as either symbol or string"))])])    
    (node/expr/relation (nodeinfo loc 'checklangplaceholder) (length types) name
                        (thunk types) scrubbed-parent is-var)))

; Helpers to more cleanly talk about relation fields
(define (relation-arity rel)
  (node/expr-arity rel))
(define (relation-name rel)
  (node/expr/relation-name rel))
(define (relation-typelist-thunk rel)
  (node/expr/relation-typelist-thunk rel))
(define (relation-parent rel)
  (node/expr/relation-parent rel))

;; -- relations ----------------------------------------------------------------

(struct node/expr/atom node/expr (name) #:transparent #:mutable
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (match-define (node/expr/atom info arity name) self)
     (fprintf port "(atom ~a)" name))]
  #:methods gen:equal+hash
  [(define equal-proc (make-robust-node-equal-syntax node/expr/atom))
   (define hash-proc  (make-robust-node-hash-syntax node/expr/atom 0))
   (define hash2-proc (make-robust-node-hash-syntax node/expr/atom 3))])

(define (atom/func name #:info [node-info empty-nodeinfo])
  (node/expr/atom node-info 1 name))

(define-syntax (atom stx)
  (syntax-case stx ()    
    [(_ name)
     (quasisyntax/loc stx (atom/func #:info (nodeinfo #,(build-source-location stx) 'checklangplaceholder)
                                     name))]))

(define (atom-name rel)
  (node/expr/atom-name rel))

;; -- constants ----------------------------------------------------------------

(struct node/expr/constant node/expr (type) #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "~v" (node/expr/constant-type self)))]
  #:methods gen:equal+hash
  [(define equal-proc (make-robust-node-equal-syntax node/expr/constant))
   (define hash-proc  (make-robust-node-hash-syntax node/expr/constant 0))
   (define hash2-proc (make-robust-node-hash-syntax node/expr/constant 3))])

; Macros in order to capture source location

; constants
(define-syntax none (lambda (stx) (syntax-case stx ()    
    [val (identifier? (syntax val)) (quasisyntax/loc stx (node/expr/constant (nodeinfo #,(build-source-location stx) 'checklangplaceholder) 1 'none))])))
(define-syntax univ (lambda (stx) (syntax-case stx ()    
    [val (identifier? (syntax val)) (quasisyntax/loc stx (node/expr/constant (nodeinfo #,(build-source-location stx) 'checklangplaceholder) 1 'univ))])))
(define-syntax iden (lambda (stx) (syntax-case stx ()    
    [val (identifier? (syntax val)) (quasisyntax/loc stx (node/expr/constant (nodeinfo #,(build-source-location stx) 'checklangplaceholder) 2 'iden))])))
; relations, not constants
; (define-syntax Int (lambda (stx) (syntax-case stx ()
;   [val (identifier? (syntax val)) (quasisyntax/loc stx (node/expr/relation (nodeinfo #,(build-source-location stx)) 1 "Int" '(Int) "univ" #f))])))
; (define-syntax succ (lambda (stx) (syntax-case stx ()    
;   [val (identifier? (syntax val)) (quasisyntax/loc stx (node/expr/relation (nodeinfo #,(build-source-location stx)) 2 "succ" '(Int Int) "Int" #f))])))

;; INTS ------------------------------------------------------------------------

; node/int is defined near top of module so it is accessible by expr->intexpr/maybe

;; -- operators ----------------------------------------------------------------

(struct node/int/op node/int (children) #:transparent)

(define-node-op add node/int/op #f #:min-length 2 #:type node/int?)
(define-node-op subtract node/int/op #f #:min-length 2 #:type node/int?)
(define-node-op multiply node/int/op #f #:min-length 2 #:type node/int?)
(define-node-op divide node/int/op #f #:min-length 2 #:type node/int?)

; id, parent, arity, checks
; card and sum both accept a single node/expr as their argument
(define-node-op card node/int/op #f #:min-length 1 #:max-length 1 #:type node/expr?)
(define-node-op sum node/int/op #f #:min-length 1 #:max-length 1 #:type node/expr?)

(define-node-op remainder node/int/op #f #:min-length 2 #:max-length 2 #:type node/int?)
(define-node-op abs node/int/op #f #:min-length 1 #:max-length 1 #:type node/int?)
(define-node-op sign node/int/op #f #:min-length 1 #:max-length 1 #:type node/int?)

; min and max are now *defined*, not declared, and in sigs-structs.rkt:

;; -- constants ----------------------------------------------------------------

(struct node/int/constant node/int (value) #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "~v" (node/int/constant-value self)))]
  #:methods gen:equal+hash
  [(define equal-proc (make-robust-node-equal-syntax node/int/constant))
   (define hash-proc  (make-robust-node-hash-syntax node/int/constant 0))
   (define hash2-proc (make-robust-node-hash-syntax node/int/constant 3))])

(define (int/func n #:info [node-info empty-nodeinfo])
  (node/int/constant node-info n))

(define-syntax (int stx)
  (syntax-case stx ()
    [(_ n)
     (quasisyntax/loc stx       
         (int/func #:info (nodeinfo #,(build-source-location stx) 'checklangplaceholder) n))]))

;; -- sum quantifier -----------------------------------------------------------
(struct node/int/sum-quant node/int (decls int-expr)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (match-define (node/int/sum-quant info decls int-expr) self)
     (fprintf port "(sum [~a] ~a)"
                   decls
                   int-expr))]
    #:methods gen:equal+hash
  [(define equal-proc (make-robust-node-equal-syntax node/int/sum-quant))
   (define hash-proc  (make-robust-node-hash-syntax node/int/sum-quant 0))
   (define hash2-proc (make-robust-node-hash-syntax node/int/sum-quant 3))])

(define (sum-quant/func decls raw-int-expr #:info [node-info empty-nodeinfo])
  (for ([e (map cdr decls)])
    (unless (node/expr? e)
      (raise-argument-error 'set "expr?" e))
    (unless (equal? (node/expr-arity e) 1)
      (raise-argument-error 'set "decl of arity 1" e)))
  (define int-expr (cond [(node/expr? raw-int-expr) 
                          (expr->intexpr/maybe raw-int-expr #:op 'sum #:info node-info)]
                         [else 
                          raw-int-expr]))
  (unless (node/int? int-expr)
    (raise-argument-error 'set "int-expr?" int-expr))
  (node/int/sum-quant node-info decls int-expr))

(define (sum-quant-expr info decls int-expr)
  (sum-quant/func decls int-expr #:info info))

;; FORMULAS --------------------------------------------------------------------

(struct node/fmla/pred-spacer node/formula (name args expanded) #:transparent
  ; print invisibly unless verbosity is set to > LOW
  #:methods gen:custom-write
  [(define (write-proc self port mode)     
     (if (<= (get-verbosity) VERBOSITY_LOW)         
         (fprintf port "~a" (node/fmla/pred-spacer-expanded self))
         (fprintf port "(node/fmla/pred-spacer ~a ~a ~a)"                                    
                  (node/fmla/pred-spacer-name self)
                  (node/fmla/pred-spacer-args self)                  
                  (node/fmla/pred-spacer-expanded self))))])


;; -- constants ----------------------------------------------------------------

(struct node/formula/constant node/formula (type) #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "~v" (node/formula/constant-type self)))]
    #:methods gen:equal+hash
  [(define equal-proc (make-robust-node-equal-syntax node/formula/constant))
   (define hash-proc  (make-robust-node-hash-syntax node/formula/constant 0))
   (define hash2-proc (make-robust-node-hash-syntax node/formula/constant 3))])

(define-syntax true (lambda (stx) (syntax-case stx ()    
    [val (identifier? (syntax val)) (quasisyntax/loc stx (node/formula/constant (nodeinfo #,(build-source-location stx) 'checklangplaceholder) 'true))])))
(define-syntax false (lambda (stx) (syntax-case stx ()    
    [val (identifier? (syntax val)) (quasisyntax/loc stx (node/formula/constant (nodeinfo #,(build-source-location stx) 'checklangplaceholder) 'false))])))



;; -- operators ----------------------------------------------------------------

; Should never be directly instantiated
(struct node/formula/op node/formula (children) #:transparent)

(define-node-op in node/formula/op #f  #:same-arity? #t #:max-length 2 #:type node/expr?)

; TODO: what is this for?
(define-node-op ordered node/formula/op #f #:max-length 2 #:type node/expr?)

; allow empty && to facilitate  {} blocks
(define-node-op && node/formula/op #f #:min-length 0 #:lift #f #:type node/formula?)
(define-node-op || node/formula/op #f #:min-length 1 #:lift #f #:type node/formula?)
(define-node-op => node/formula/op #f #:min-length 2 #:max-length 2 #:lift #f #:type node/formula?)
(define-node-op ! node/formula/op #f #:min-length 1 #:max-length 1 #:lift #f #:type node/formula?)
(define-node-op int> node/formula/op #f #:min-length 2 #:max-length 2 #:type node/int?)
(define-node-op int< node/formula/op #f #:min-length 2 #:max-length 2 #:type node/int?)
(define-node-op int= node/formula/op #f #:min-length 2 #:max-length 2 #:type node/int?)

; Electrum temporal operators
(define-node-op always node/formula/op #f #:min-length 1 #:max-length 1 #:lift #f #:type node/formula?)
(define-node-op eventually node/formula/op #f #:min-length 1 #:max-length 1 #:lift #f #:type node/formula?)
(define-node-op next_state node/formula/op #f #:min-length 1 #:max-length 1 #:lift #f #:type node/formula?)
(define-node-op until node/formula/op #f #:min-length 1 #:max-length 2 #:lift #f #:type node/formula?)
(define-node-op releases node/formula/op #f #:min-length 1 #:max-length 2 #:lift #f #:type node/formula?)

; Electrum past-time temporal operators
(define-node-op historically node/formula/op #f #:min-length 1 #:max-length 1 #:lift #f #:type node/formula?)
(define-node-op once node/formula/op #f #:min-length 1 #:max-length 1 #:lift #f #:type node/formula?)
; Note that prev_state F is false in state 0 for any F
(define-node-op prev_state node/formula/op #f #:min-length 1 #:max-length 1 #:lift #f #:type node/formula?)
(define-node-op since node/formula/op #f #:min-length 1 #:max-length 2 #:lift #f #:type node/formula?)
(define-node-op triggered node/formula/op #f #:min-length 1 #:max-length 2 #:lift #f #:type node/formula?)

; --------------------------------------------------------

(define (int=-lifter i1 i2)
  (int= i1 i2))

(define-node-op = node/formula/op #f #:same-arity? #t #:max-length 2 #:lift int=-lifter #:type node/expr?) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Historical note: we used to lift the "and", "or" and "not" procedures.
; However, this was unsound. One reason is that (e.g.) "and" is short-circuiting
; in Racket. Also, what should empty-and denote? Etc. This caused us major issues
; in testing and even some performance problems. So we no longer lift Racket's 
; boolean operators. Use "&&" to build a conjunction, not "and".

(require (prefix-in @ (only-in racket ->)))
(define/contract (maybe-and->list fmla)
  (@-> node/formula? (listof node/formula?))
  (cond [(node/formula/op/&&? fmla)
         (apply append (map maybe-and->list (node/formula/op-children fmla)))]
        [else
         (list fmla)]))

;; -- quantifiers --------------------------------------------------------------

(struct node/formula/quantified node/formula (quantifier decls formula)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (match-define (node/formula/quantified info quantifier decls formula) self)
     (fprintf port "(~a ~a ~a)" quantifier decls formula))]
  #:methods gen:equal+hash
  [(define equal-proc (make-robust-node-equal-syntax node/formula/quantified))
   (define hash-proc  (make-robust-node-hash-syntax node/formula/quantified 0))
   (define hash2-proc (make-robust-node-hash-syntax node/formula/quantified 3))])

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
     (fprintf port "(~a ~a)" mult expr))]
  #:methods gen:equal+hash
  [(define equal-proc (make-robust-node-equal-syntax node/formula/multiplicity))
   (define hash-proc  (make-robust-node-hash-syntax node/formula/multiplicity 0))
   (define hash2-proc (make-robust-node-hash-syntax node/formula/multiplicity 3))])

(define (node-type n) 
  (cond [(node/expr? n) "expression"]
        [(node/int? n) "integer expression"]
        [(node/formula? n) "formula"]
        [else "unknown type"]))

(define (multiplicity-formula info mult expr)
  (unless (node/expr? expr)
    (define loc (nodeinfo-loc info))
    (define locstr (format "line ~a, col ~a, span: ~a" (source-location-line loc) (source-location-column loc) (source-location-span loc)))    
    (raise-syntax-error mult (format "~a operator expected to be given an expression, but instead got ~a at loc: ~a" mult (node-type expr) locstr)
                             (datum->syntax #f (deparse expr) (build-source-location-syntax loc)))) 
  (node/formula/multiplicity info mult expr))

(define (no-pairwise-intersect vars #:context [context #f])
  (apply &&/func (no-pairwise-intersect-recursive-helper vars #:context context)))

(define (no-pairwise-intersect-recursive-helper vars #:context [context #f])
  (cond [(empty? vars) (raise-forge-error
                        #:msg "Cannot take pairwise intersection of empty list"
                        #:context context)]
        [(empty? (rest vars)) (list true)]
        [else (append (map (lambda (elt) (no (& (first vars) elt))) (rest vars))
                      (no-pairwise-intersect-recursive-helper (rest vars) #:context context))]))

;;; ALL ;;;

(define (all-quant/func decls formula #:info [node-info empty-nodeinfo])
  (quantified-formula node-info 'all decls formula))

(define-syntax (all stx)
  (syntax-parse stx
    [(_ check-lang:opt-check-lang-class (~optional (~and #:disj disj)) ([v0 e0 m0:opt-mult-class] ...) pred)     
      (quasisyntax/loc stx (all/info (nodeinfo #,(build-source-location stx) check-lang.check-lang) (~? disj) ([v0 e0 m0] ...) pred))]))

(define-syntax (all/info stx)
  (syntax-parse stx
    ; quantifier case with disjointness flag; embed and repeat
    ; TODO: currently discarding the multiplicity info, unchecked
    [(_ info #:disj ([v0 e0 m0:opt-mult-class] ...) pred)
     (quasisyntax/loc stx
       (all/info info ([v0 e0 m0] ...)
                 #,(quasisyntax/loc stx
                     (=> (no-pairwise-intersect (list v0 ...) #:context #,(build-source-location stx)) pred))))]
    [(_ info ([v0 e0 m0:opt-mult-class] ...) pred)     
     (quasisyntax/loc stx
       (let* ([v0 (node/expr/quantifier-var info (node/expr-arity e0) (gensym (format "~a_all" 'v0)) 'v0)] ...)
         (quantified-formula info 'all (list (cons v0 e0) ...) pred)))]))

;;; SOME ;;;

(define (some-quant/func decls formula #:info [node-info empty-nodeinfo])
  (quantified-formula node-info 'some decls formula))

(define (some/func expr #:info [node-info empty-nodeinfo])
  (multiplicity-formula node-info 'some expr))

(define-syntax (some stx)
  (syntax-parse stx
    [(_ check-lang:opt-check-lang-class (~optional (~and #:disj disj)) ([v0 e0 m0:opt-mult-class] ...) pred) 
      (quasisyntax/loc stx (some/info (nodeinfo #,(build-source-location stx) check-lang.check-lang) (~? disj) ([v0 e0 m0] ...) pred))]
    [(_ check-lang:opt-check-lang-class expr)
      (quasisyntax/loc stx (some/info (nodeinfo #,(build-source-location stx) check-lang.check-lang) expr))]))

(define-syntax (some/info stx)
  (syntax-parse stx 
    ; ignore quantifier over no variables
    [(_ info (~optional #:disj) () pred) #'pred]
    ; quantifier case
    ; TODO: currently discarding the multiplicity info, unchecked (in this and the following cases)
    [(_ info ([v0 e0 m0:opt-mult-class] ...) pred)
     (quasisyntax/loc stx
       (let* ([v0 (node/expr/quantifier-var info (node/expr-arity e0) (gensym (format "~a_some" 'v0)) 'v0)] ...)
         (quantified-formula info 'some (list (cons v0 e0) ...) pred)))]
    ; quantifier case with disjointness flag; embed and repeat
    [(_ info #:disj ([v0 e0 m0:opt-mult-class] ...) pred)
     (quasisyntax/loc stx
       (some/info info ([v0 e0 m0] ...)
                  #,(quasisyntax/loc stx
                      (&& (no-pairwise-intersect (list v0 ...)
                                                 #:context #,(build-source-location stx)) pred))))]
    ; multiplicity case
    [(_ info expr)
     (quasisyntax/loc stx
       (multiplicity-formula info 'some expr))]))

;;; NO ;;;

(define (no-quant/func decls formula #:info [node-info empty-nodeinfo])
  (quantified-formula node-info 'no decls formula))

(define (no/func expr #:info [node-info empty-nodeinfo])
  (multiplicity-formula node-info 'no expr))

(define-syntax (no stx)
  (syntax-parse stx
    ; quantifier
    [(_ (~optional (#:lang check-lang) #:defaults ([check-lang #''checklangNoCheck])) (~optional (~and #:disj disj)) ([v0 e0 m0:opt-mult-class] ...) pred) 
      (quasisyntax/loc stx (no/info (nodeinfo #,(build-source-location stx) check-lang) (~? disj) ([v0 e0 m0] ...) pred))]
    ; multiplicity
    [(_ (~optional (#:lang check-lang) #:defaults ([check-lang #''checklangNoCheck])) expr)
      (quasisyntax/loc stx (no/info (nodeinfo #,(build-source-location stx) check-lang) expr))]))

(define-syntax (no/info stx)
  (syntax-parse stx
    ; quantifier with disj: rewrite as !some
    [(_ info #:disj ([v0 e0 m0:opt-mult-class] ...) pred)
     #'(! (some/info info #:disj ([v0 e0 m0] ...) pred))]
    ; quantifier without disj: rewrite as !some
    [(_ info ([v0 e0 m0:opt-mult-class] ...) pred)
     (quasisyntax/loc stx
       (let* ([v0 (node/expr/quantifier-var info (node/expr-arity e0) (gensym (format "~a_no" 'v0)) 'v0)] ...)
         (! (quantified-formula info 'some (list (cons v0 e0) ...) pred))))]
    [(_ info expr)
     (quasisyntax/loc stx
       (multiplicity-formula info 'no expr))]))

;;; ONE ;;;

(define (one/func expr #:info [node-info empty-nodeinfo])
  (multiplicity-formula node-info 'one expr))

(define (one-quant/func decls formula #:info [node-info empty-nodeinfo])
  (one/func #:info node-info (set/func #:info node-info decls formula)))

(define-syntax (one stx)
  (syntax-parse stx
    ; quantifier
    [(_ (~optional (#:lang check-lang) #:defaults ([check-lang #''checklangNoCheck])) (~optional (~and #:disj disj)) ([v0 e0 m0:opt-mult-class] ...) pred) 
      (quasisyntax/loc stx (one/info (nodeinfo #,(build-source-location stx) check-lang) (~? disj) ([v0 e0 m0] ...) pred))]
    ; multiplicity
    [(_ (~optional (#:lang check-lang) #:defaults ([check-lang #''checklangNoCheck])) expr)
      (quasisyntax/loc stx (one/info (nodeinfo #,(build-source-location stx) check-lang) expr))]))

(define-syntax (one/info stx)
  (syntax-parse stx
    [(_ info #:disj ([x1 r1 m0:opt-mult-class] ...) pred)
     (quasisyntax/loc stx
       ; Kodkod doesn't have a "one" quantifier natively.
       ; Instead, desugar as a multiplicity of a set comprehension
       (multiplicity-formula info 'one
                             (set ([x1 r1] ...)
                                  #,(quasisyntax/loc stx (&& (no-pairwise-intersect (list x1 ...)
                                                                                    #:context #,(build-source-location stx)) pred)))))]
    [(_ info ([x1 r1 m0:opt-mult-class] ...) pred)
     (quasisyntax/loc stx
       ; Kodkod doesn't have a "one" quantifier natively.
       ; Instead, desugar as a multiplicity of a set comprehension
       (multiplicity-formula info 'one (set ([x1 r1] ...) pred)))]
    ; multiplicity case
    [(_ info expr)
     (quasisyntax/loc stx
       (multiplicity-formula info 'one expr))]))

;;; LONE ;;;

(define (lone/func expr #:info [node-info empty-nodeinfo])
  (multiplicity-formula node-info 'lone expr))

(define (lone-quant/func decls formula #:info [node-info empty-nodeinfo])
  (lone/func #:info node-info (set/func #:info node-info decls formula)))

(define-syntax (lone stx)
  (syntax-parse stx    
    [(_ (~optional (#:lang check-lang) #:defaults ([check-lang #''checklangNoCheck])) (~optional (~and #:disj disj)) ([v0 e0 m0:opt-mult-class] ...) pred) 
      (quasisyntax/loc stx (lone/info (nodeinfo #,(build-source-location stx) check-lang) (~? disj) ([v0 e0 m0] ...) pred))]
    [(_ (~optional (#:lang check-lang) #:defaults ([check-lang #''checklangNoCheck])) expr)
      (quasisyntax/loc stx (lone/info (nodeinfo #,(build-source-location stx) check-lang) expr))]))

(define-syntax (lone/info stx)
  (syntax-parse stx
    [(_ info #:disj ([x1 r1 m0:opt-mult-class] ...) pred)
     (quasisyntax/loc stx
       ; Kodkod doesn't have a lone quantifier natively.
       ; Instead, desugar as a multiplicity of a set comprehension
       (multiplicity-formula info 'lone (set ([x1 r1] ...)
                                             #,(quasisyntax/loc stx
                                                 (&& (no-pairwise-intersect (list x1 ...)
                                                                            #:context #,(build-source-location stx)) pred)))))]
    [(_ info ([x1 r1 m0:opt-mult-class] ...) pred)
     (quasisyntax/loc stx
       ; Kodkod doesn't have a lone quantifier natively.
       ; Instead, desugar as a multiplicity of a set comprehension
       (multiplicity-formula info 'lone (set ([x1 r1] ...) pred)))]
    ; multiplicity case
    [(_ info expr)
     (quasisyntax/loc stx
       (multiplicity-formula info 'lone expr))]))

; sum-quant/func defined above

; sum quantifier macro
(define-syntax (sum-quant stx)
  (syntax-parse stx
    [(_ (~optional (#:lang check-lang) #:defaults ([check-lang #''checklangNoCheck])) ([x1 r1 m0:opt-mult-class] ...) int-expr)
     (quasisyntax/loc stx
       (let* ([x1 (node/expr/quantifier-var (nodeinfo #,(build-source-location stx) check-lang) (node/expr-arity r1) (gensym (format "~a_sum" 'x1)) 'x1)] ...)
         (sum-quant-expr (nodeinfo #,(build-source-location stx) check-lang) (list (cons x1 r1) ...) int-expr)))]))


;; -- sealing (for examplar) -----------------------------------------------------------

(define (simple-write-proc str)
  (lambda (v port mode)
    (fprintf port "#<~a>" str)))

(struct node/formula/sealed node/formula []
  #:methods gen:custom-write [(define write-proc (simple-write-proc "node/formula/sealed"))])
(struct wheat node/formula/sealed []
  #:methods gen:custom-write [(define write-proc (simple-write-proc "wheat"))]
  #:extra-constructor-name make-wheat)
(struct chaff node/formula/sealed []
  #:methods gen:custom-write [(define write-proc (simple-write-proc "chaff"))]
  #:extra-constructor-name make-chaff)

(define (unseal-node/formula x)
  (if (node/formula/sealed? x)
    (node-info x)
    x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Build syntax for a generic equals/hash that's robust to new node subtypes

; Issue with this approach: struct-accessors is re-called every time equals? is invoked.
; Leaving it like this for now until it becomes a performance issue.

; Might think to promote struct-accessors into phase 2 and invoke it from within
;  the builder macros, but since macros work outside-in, can't just do something like
; (printf "accessors: ~a~n" (struct-accessors #'structname))
;   ^ the macro gets #'structname, not the actual structure name.

; Macro to get all accessors
; Thanks to Alexis King:
; https://stackoverflow.com/questions/41311604/get-a-structs-field-names
; requires installing syntax-classes package
(require (for-meta 1 racket/base
                     syntax/parse/class/struct-id)
         syntax/parse/define)

  (define-simple-macro (struct-accessors id:struct-id)
    (begin ;(printf "~a~n" (list id.accessor-id ...))
           (list id.accessor-id ...)))

; Use this macro to produce syntax (for use in operator-registration macro)
;   that builds a comparator including all struct fields except for node-info
; Note that extenders, like Sig (which extends node/expr/relation) *MUST* 
; provide an equals method in the same manner, otherwise when comparing
; two Sigs, only the underlying relation fields will be considered.
(define-syntax (make-robust-node-equal-syntax stx)
  (syntax-case stx ()
    [(_ structname)
     (begin
       ; don't want to call struct-accessors every time this lambda is invoked,
       ; so call once at expansion time of make-robust...
       ;(printf "structname: ~a~n" (syntax->datum #'structname))       
       #`(lambda (a b equal-proc)           
           (andmap (lambda (access)           
                     ;(printf "checking equality for ~a, proc ~a~n" structname access)
                     (define vala (access a))
                     (define valb (access b))
                     ; Some AST fields may be thunkified
                     (cond                         
                       [(and (procedure? vala)
                             (procedure? valb)
                             (equal? (procedure-arity vala) 0)
                             (equal? (procedure-arity valb) 0))
                         (equal-proc (vala) (valb))]
                       [(and (or (node/expr? vala) (node/formula? vala) (not (procedure? vala)))
                             (or (node/expr? valb) (node/formula? vala) (not (procedure? valb))))
                         (equal-proc vala valb)]
                       [else (raise (format "Mismatched procedure fields when checking equality for ~a. Got: ~a and ~a"
                                            structname vala valb))]))                                              
                   (remove node-info (struct-accessors structname)))))]))

; And similarly for hash
(define multipliers '(3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71))
(define-simple-macro (make-robust-node-hash-syntax structname offset)
  (lambda (a hash-proc)
    (define multiplied
      (for/list ([access (remove node-info (struct-accessors structname))]
                 [multiplier (drop multipliers offset)])
        ; Some AST fields may be thunkified
        ; Also, any node/expr is a procedure? since that's how we impl. box join
        ; Also, in order to add better error messages for mis-used node/formulas,
        ;   any node/formula is also a procedure?
        ; ASSUME: node/expr will never be an exactly-arity-0 procedure.
        (define vala (access a))
        (cond
          ; zero-arity procedure (a "thunk"): invoke it and then hash the result
          [(and (procedure? vala)                
                (equal? (procedure-arity vala) 0))
           ;(printf "field was zero arity procedure: ~a~n" vala)
           (@* multiplier (hash-proc (vala)))]

          ; node/expr? (but not a zero-arity procedure): just hash it
          [(node/expr? vala)
           ;(printf "field was expr: ~a~n" vala)
           (@* multiplier (hash-proc vala))]

          ; node/formula? (but not a zero-arity procedure): just hash it
          [(node/formula? vala)
           ;(printf "field was formula: ~a~n" vala)
           (@* multiplier (hash-proc vala))]

          ; not a procedure at all: just hash it
          [(not (procedure? vala))
           ;(printf "field was other non-procedure: ~a~n" vala)
           (@* multiplier (hash-proc vala))]

          [else (raise (format "Non-thunk procedure field when hashing for ~a. Got: ~a"
                              structname vala))])))
    ;(printf "in mrnhs for ~a/~a: ~a, multiplied: ~a~n" structname offset a multiplied)    
    (apply @+ multiplied)))


;; BREAKERS --------------------------------------------------------------------

(struct node/breaking node () #:transparent)
(struct node/breaking/op node (children) #:transparent)
(define-node-op is node/breaking/op #f #:max-length 2 #:type (lambda (n) (@or (node/expr? n) (node/breaking/break? n))))
(struct node/breaking/break node/breaking (break) #:transparent)
(define-syntax (make-breaker stx)
  (syntax-case stx ()
    [(make-breaker id sym)
      #'(define-syntax id (lambda (stx) (syntax-case stx ()    
          [val (identifier? (syntax val)) (quasisyntax/loc stx (node/breaking/break (nodeinfo #,(build-source-location stx) 'checklangplaceholder) sym))])))]))

(make-breaker cotree 'cotree)
(make-breaker cofunc 'cofunc)
(make-breaker cosurj 'cosurj)
(make-breaker coinj 'coinj)

(make-breaker ireff 'ireff)
(make-breaker ref 'ref)
(make-breaker linear 'linear)
(make-breaker plinear 'plinear)
(make-breaker acyclic 'acyclic)
(make-breaker tree 'tree)
(make-breaker func 'func)
(make-breaker pfunc 'pfunc)
(make-breaker surj 'surj)
(make-breaker inj 'inj)
(make-breaker bij 'bij)
(make-breaker pbij 'pbij)

(make-breaker default 'default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; *** raise-forge-error ***
;
; Use this error-throwing function whenever possible in Forge.
; 
; Racket "user errors" avoid the (possibly confusing) stack trace that a Racket 
; "syntax error" produces, but only DrRacket will highlight source location from
; a user error. For the VSCode extension (and use from the terminal), produce a
; custom user error that contains the row and column information of the offending
; AST node.

(define (pretty-loc loc)
  (format "~a:~a:~a (span ~a)" (srcloc-source loc) (srcloc-line loc) (srcloc-column loc) (srcloc-span loc)))

(define (raise-forge-error #:msg [msg "error"] #:context [context #f])  
  (define loc (cond                
                [(nodeinfo? context) (pretty-loc (nodeinfo-loc context))]
                ; Wheats/chaffs have their inner formula in the info field
                [(and (node? context) (node? (node-info context)))
                 (pretty-loc (nodeinfo-loc (node-info (node-info context))))]
                [(node? context) (pretty-loc (nodeinfo-loc (node-info context)))]
                [(srcloc? context) (pretty-loc context)]
                [(syntax? context) (pretty-loc (build-source-location context))]                    
                [else "unknown:?:?"]))
  (raise-user-error (format "[~a] ~a" loc msg)))

