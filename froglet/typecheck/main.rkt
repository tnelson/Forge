#lang racket/base

;; TODO
;; - froglet: binop-check: (#<syntax:correspond.frg:38:46 "implies"> '#s((sigtype type 1) #<syntax Bool> #f #f ()) '#s((sigtype type 1) #<syntax Bool> #f #f ()))
;; - correspond, why running forge during compile? something wrong with dyn-require? what does bsl do?
;; - see checklist in error/main.rkt
;; - scope-check ... other not implemented (go back through f1 f2 f3)
;; - ...

;; TODO can we use turnstile?
;;  collect env and then use turnstile?

(provide
  typecheck
  env-serialize)

(require
  forge/lang/alloy-syntax/syntax-class
  (only-in froglet/util type-env-name log-froglet-info log-froglet-warning)
  froglet/typecheck/error
  froglet/typecheck/struct
  racket/list
  racket/match
  racket/pretty
  syntax/id-set
  syntax/parse
  (for-syntax racket/base syntax/parse))

(define-syntax (define-parser stx)
  (syntax-parse stx
   ((_ id clause ...)
    #`(define id #,(syntax/loc stx (syntax-parser clause ...))))))

;; -----------------------------------------------------------------------------

(define (env-init)
  (list the-int-type the-bool-type the-unknown-type))

(define current-type-env (make-parameter '()))

(define current-quant (make-parameter #f))
;; (or/c #f               ;; uninitialized
;;       symbol?          ;; quant
;;       (list symbol?    ;; quant
;;             boolean?)) ;; disj?

(define (quant=? x)
  (equal? (current-quant) x))

(define (one-mult? mval)
  (eq? 'one (mult->sym mval)))

(define (no-mult? mval)
  (eq? 'no (mult->sym mval)))

(define (some-mult? mval)
  (eq? 'some (mult->sym mval)))

(define (all-mult? mval)
  (eq? 'all (mult->sym mval)))

(define (singleton-mult? mval)
  (define sym (mult->sym mval))
  (or (eq? #f sym)
      (no-mult? sym)
      (one-mult? sym)
      (some-mult? sym)
      (all-mult? sym)))

(define (mult->sym mval)
  (cond
    ((pair? mval) (car mval))
    ((identifier? mval) (syntax-e mval))
    (else mval)))

(define (singleton-sigtype? x)
  (and (sigtype? x)
       (singleton-mult? (sigtype-mult x))))

(define forge:module 'module)
(define forge:expr 'expr)
(define forge:subexpr 'subexpr)
(define forge:example 'example)
(define forge:inst 'inst)
(define forge:bounds 'bounds)
(define current-forge-context (make-parameter '()))

(define (context=? sym)
  (define ctx (current-forge-context))
  (and (pair? ctx) (eq? sym (car ctx))))

(define (in-context? sym)
  (memq sym (current-forge-context)))

(define (context+ sym)
  (define ctx (current-forge-context))
  (if (and (pair? ctx) (eq? (car ctx) sym))
    ctx
    (cons sym ctx)))

(define-syntax-rule (with-forge-context sym e0 e1* ...)
  (parameterize ((current-forge-context (context+ sym))) e0 e1* ...))

(define the-type# (make-hash))

(define (get-type stx)
  (hash-ref the-type# stx
            (lambda () (log-froglet-warning "get-type not found ~e" stx) the-unknown-type)))

(define (set-type stx tt)
  (void (hash-set! the-type# stx tt))
  tt)

(define (typecheck mod)
  (define-values [env0 import*] (env-collect mod))
  (define env (env-extend env0 import* (env-init)))
  (log-froglet-info "type env~n ~a" (pretty-format env 120 #:mode 'write))
  (void (env-check env))
  (parameterize ([current-type-env env])
    (hash-clear! the-type#)
    (with-forge-context forge:module
      (mod-check mod))
    (void))
  env0)

(define-parser mod-check
  [mod:$AlloyModule
   (for ((par (in-list (syntax-e #'(mod.parag* ...)))))
     (parag-check par))
   (with-forge-context forge:expr
     (for ((exp (in-list (syntax-e #'(mod.expr* ...)))))
       (expr-check exp)))]
  [_
    (log-froglet-warning "mod-check: unknown stx ~a" (syntax->datum this-syntax))
    (void)])

(define-parser parag-check
  [sig:$SigDecl
   (when (attribute sig.block)
     (block-check #'sig.block))
   (void)]
  [fn:$FactDecl
   (todo-not-implemented 'parag-check:FactDecl)
   (void)]
  [pred:$PredDecl
   (preddecl-check #'pred)]
  [fd:$FunDecl
   (fundecl-check #'fd)]
  [ad:$AssertDecl
   (todo-not-implemented 'parag-check:AssertDecl)
   (void)]
  [cd:$CmdDecl
   (cmddecl-check this-syntax)]
  [td:$TestExpectDecl
   (testblock-check #'td.test-block)
   (void)]
  [sd:$SexprDecl
   (todo-not-implemented 'parag-check:SexprDecl)
   (void)]
  [rd:$RelDecl
   (todo-not-implemented 'parag-check:RelDecl)
   (void)]
  [od:$OptionDecl
   ;; ignore
   (void)]
  [id:$InstDecl
   (with-forge-context forge:inst
     (instdecl-check #'id))]
  [ed:$ExampleDecl
   (with-forge-context forge:example
     (exampledecl-check #'ed))]
  [par
   (log-froglet-warning "parag-check: unknown stx ~a" (syntax->datum #'par))
   (void)])

(define-parser preddecl-check
  [pred:$PredDecl
   (define predty (name-lookup #'pred.name))
   (define decl-tys (predtype-param* predty))
   (define bb #'pred.block)
   (parameterize ((current-type-env (env-extend decl-tys (current-type-env))))
     (block-check bb))
   (for* ((ee (in-list (block->list bb)))
          (tt (in-value (get-type ee)))
          #:unless (type<: (->boolty (get-type ee)) the-bool-type))
     (raise-type-error (format "pred expected a formula, given a ~a" (type-kind tt))
                       (deparse ee)))
   (void)]
  [_
    (log-froglet-warning "preddecl-check: unknown stx ~a" (syntax->datum this-syntax))
    (void)])

(define-parser fundecl-check
  [fun:$FunDecl
   (define funty (name-lookup #'fun.name))
   (define decl-tys (funtype-param* funty))
   (define out-ty (funtype->return funty))
   (define ee #'fun.body)
   (parameterize ((current-type-env (env-extend decl-tys (current-type-env))))
     (expr-check ee))
   (unless (type<: (get-type ee) out-ty)
     (raise-type-error (format "fun output type does not match body type ~s" (get-type ee))
                       (deparse ee)))
   (void)]
  [_
    (log-froglet-warning "preddecl-check: unknown stx ~a" (syntax->datum this-syntax))
    (void)])

(define-parser block->list
  [bb:$Block
   (syntax-e #'(bb.exprs ...))]
  [_
    (log-froglet-warning "block->list unknown stx ~a" (syntax->datum this-syntax))
    '()])

(define (block-check stx)
  ;; TODO disallow (some father)
  #;(log-froglet-info "block check ~s" stx)
  (with-forge-context forge:expr
    (for-each expr-check (block->list stx)))
  (set-type stx the-bool-type))

(define-parser cmddecl-check
  [cd:$CmdDecl
   (define cmd-name #'cd.name)
   (when (attribute cd.parameters)
     (params-check #'cd.parameters))
   (when (attribute cd.pred-name)
     (define pn #'cd.pred-name)
     (unless (name->pred pn)
       (raise-unknown-pred-error pn)))
   (when (syntax-e #'cd.pred-block)
     (block-check #'cd.pred-block))
   (when (syntax-e #'cd.scope)
     (scope-check #'cd.scope))
   (when (attribute cd.bounds)
     (with-forge-context forge:bounds
       (bounds-check #'cd.bounds)))
   (void)]
  [_
    (log-froglet-warning "cmddecl-check: unknown stx ~a" (syntax->datum this-syntax))
    (void)])

(define-parser exampledecl-check
  [ee:$ExampleDecl
   (define name #'ee.name)
   (with-forge-context forge:expr
     (expr-check #'ee.pred))
   (with-forge-context forge:bounds
     (bounds-check #'ee.bounds))
   (void)]
  [_
    (log-froglet-warning "exampledecl-check: unknown stx ~a" (syntax->datum this-syntax))
    (void)])

(define-parser instdecl-check
  [id:$InstDecl
   (define name #'id.name)
   (with-forge-context forge:bounds
     (bounds-check #'id.bounds))
   (when (attribute id.scope)
     (scope-check #'id.scope))
   (void)]
  [_
    (log-froglet-warning "instdecl-check: unknown stx ~a" (syntax->datum this-syntax))
    (void)])

(define-parser testblock-check
  [tb:$TestBlock
    (define ty
      (for/last ([tt (in-list (syntax-e #'(tb.test-decls ...)))])
        (testdecl-check tt)))
    (set-type this-syntax ty)]
  [_
    (log-froglet-warning "exampledecl-check: unknown stx ~a" (syntax->datum this-syntax))
    (void)])

(define-parser testdecl-check
  [td:$TestDecl
    #;(log-froglet-info "typechecking TestDecl ~s" (syntax-e #'td.name))
    (when (attribute td.parameters)
      (params-check #'td.parameters))
    (when (syntax-e #'td.pred-name)
      (name-check #'td.pred-name))
    (when (syntax-e #'td.pred-block)
      (block-check #'td.pred-block))
    (when (attribute td.scope)
      (scope-check #'td.scope))
    (when (attribute td.bounds)
      (with-forge-context forge:bounds
        (bounds-check #'td.bounds)))
    (set-type this-syntax the-unknown-type)]
  [_
    (log-froglet-warning "exampledecl-check: unknown stx ~a" (syntax->datum this-syntax))
    (void)])

(define (params-check stx)
  ;; TODO
  (todo-not-implemented 'params-check)
  (set-type stx the-unknown-type))

(define (scope-check stx)
  ;; TODO
  (todo-not-implemented 'scope-check)
  (set-type stx the-unknown-type))

(define-parser bounds-check
  [bb:$Bounds
   (define exact? (syntax-e #'bb.exact?))
   (with-forge-context forge:expr
     (for-each expr-check (syntax-e #'(bb.exprs ...))))
   (set-type this-syntax the-bool-type)]
  [_
   (log-froglet-warning "bounds-check: bad syntax ~s" this-syntax)
   (set-type this-syntax the-unknown-type)])

(define-parser expr-check
  [exp:$Expr
    (define ctx this-syntax)
    (define tt
      (syntax-parse (syntax/loc ctx (exp.body ...))
       [(_:$NotOp e)
        ;; set a parameter?
        (void ;;with-forge-context forge:subexpr
          (expr-check #'e))
        the-bool-type]
       [("let" decls:$LetDeclList bob:$BlockOrBar)
        (define decl-tys (letdecllist-check #'decls))
        (parameterize ((current-type-env (env-extend decl-tys (current-type-env))))
          (blockorbar-check #'bob))]
       [("bind" decls:$LetDeclList bob$BlockOrBar)
        (raise-form-error (datum->syntax ctx 'bind ctx))
        the-unknown-type]
       [(qq:$QuantStr e1:$Expr)
        (define e1-ty
          (parameterize ([current-quant (string->symbol (syntax-e #'qq))])
            (expr-check #'e1)))
        the-bool-type]
       [(q:$Quant (~optional (~and "disj" disj) #:defaults ([disj #'#f]))
                  decls:$DeclList bob:$BlockOrBar)
        (define decl-tys
          (parameterize ([current-quant (list (syntax-e #'q.symbol) (and (syntax-e #'disj) #t))])
            (decllist-check #'decls)))
        (parameterize ((current-type-env (env-extend decl-tys (current-type-env))))
          (blockorbar-check #'bob))
        the-bool-type]
       [(e1:$Expr (~optional negate:$NotOp)
                  (~or -op:$CompareOp -op:$BinaryOp)
                  e2:$Expr
                  (~optional (~seq "else" e3:$Expr) #:defaults ([e3 #'#f])))
        (define op #'(~? -op.symbol -op))
        (define neg? (and (attribute negate) #true))
        (cond
          [(syntax-e #'e3)
           (when neg?
             (raise-type-error "cannot negate an implication" (deparse this-syntax)))
           (ifelse-check #'e1 #'e2 #'e3 ctx)]
          [(eq? (syntax->datum op) 'is)
           (bounds-is-check #'e1 #'e2 ctx)]
          [else
           (void ;;with-forge-context forge:subexpr
             (expr-check #'e1)
             (expr-check #'e2))
           (binop-check op #'e1 #'e2 ctx #:negate? neg?)])]
       [(e1:$Expr (~datum ".") e2:$Expr)
        (with-forge-context forge:subexpr
          (expr-check #'e1)
          (expr-check #'e2))
        (join-check #'e1 #'e2 ctx)]
       [(op:$UnaryOp e1:$Expr)
        (void ;;with-forge-context forge:subexpr
          (expr-check #'e1))
        (unop-check #'op #'e1)]
       [(e1:$Expr "'")
        (todo-not-implemented (format "expr-check: ~s" ctx))
        the-unknown-type]
       [("[" _:$ExprList "]")
        ;; not implemented in normal forge, doesn't even parse apparently
        (raise-form-error (datum->syntax ctx "list expressions [ ... ]"))
        the-unknown-type]
       [(nm:$Name "[" ee:$ExprList "]")
        (define e* (syntax-e #'(ee.exprs ...)))
        (void (name-check #'nm.name))
        (with-forge-context forge:subexpr
          (for-each expr-check e*))
        (app-check #'nm e* ctx)]
       [(ex1:$Expr "[" ee:$ExprList "]")
        (define e* (syntax-e #'(ee.exprs ...)))
        (void
          (expr-check #'ex1))
        (with-forge-context forge:subexpr
          (for-each expr-check e*))
        (app-check #'ex1 e* ctx)]
       [(cn:$Const)
        (define ty
          (let* ((ct #'cn.translate)
                 (dd (syntax->datum ct)))
            (cond
              [(identifier? ct)
               (consttype ct ct)]
              [(and (pair? dd) (eq? 'int (car dd)))
               (consttype (type-name the-int-type) (cadr dd))]
              [else
               (log-froglet-warning "unknown const ~s" #'cn)
               the-unknown-type])))
        (set-type #'cn ty)]
       [(qn:$QualName)
        (set-type #'qn (nametype #'qn.name))]
       [("this")
        (nametype (car (syntax-e this-syntax)))]
       [("`" nm:$Name)
        (nametype #'nm.name)]
       [("{" decls:$DeclList bob:$BlockOrBar "}")
        (define decl-tys (decllist-check #'decls))
        (define bob-ty
          (parameterize ((current-type-env (env-extend decl-tys (current-type-env))))
            (blockorbar-check #'bob)))
        (define elem-ty* (map paramtype-sig decl-tys))
        (todo-not-implemented (format "singleton check for ~s : ~s" (deparse/datum ctx) elem-ty*))
        (reltype elem-ty* #true)]
       [(block:$Block)
        (block-check #'block)]
       [(sexpr:$Sexpr)
        (sexpr-check #'sexpr)]
       [_
        (log-froglet-warning "expr-check: internal error parsing expr ~e" this-syntax)
        the-unknown-type]))
    #;(log-froglet-info "expr-check: ~s~n type: ~s" this-syntax tt)
    (when (and (context=? forge:expr)
               (not (or (and (in-context? forge:bounds) (quant=? 'no))
                        (in-context? forge:inst)
                        (in-context? forge:example)))
               (is-field? (reltype->type tt)))
      (raise-type-error
        "expected an object"
        (deparse ctx)))
    (set-type ctx tt)]
  [_
    (log-froglet-warning "expr-check: expected $Expr got ~e" this-syntax)
    (set-type this-syntax the-unknown-type)])

(define (reltype->type tt)
  (cond
    ((type? tt)
     tt)
    ((reltype? tt)
     (define elem* (reltype-col-type* tt))
     (if (or (null? elem*) (not (null? (cdr elem*))))
       (begin
         (log-froglet-warning "reltype->type: multi-column relation ~e" tt)
         the-unknown-type)
       (car elem*)))
    (else
      (log-froglet-warning "reltype->type: internal error, unexpected arg ~e" tt)
      the-unknown-type)))

(define-parser sexpr-check
  [ss:$Sexpr
    (todo-not-implemented 'sexpr-check)
    (set-type this-syntax the-unknown-type)]
  [_
    (set-type this-syntax the-unknown-type)])

(define (is-field? tt)
  (define parent (field->sigty tt))
  (sigtype? parent))

(define (ifelse-check e1 e2 e3 ctx)
  ;; TODO use e1 type to refine the rest!
  (void
    (expr-check e1)
    (unless (type<: (->boolty (get-type e1)) the-bool-type)
      (raise-type-error "expected a formula, given a ~a" (type-kind (get-type e1)) (deparse e1)))
    (expr-check e2)
    (expr-check e3))
  (printf "UNION ~a ~a~n ==> ~a~n"
          (get-type e2) (get-type e3)
          (union-type (get-type e2) (get-type e3)))
  (union-type (get-type e2) (get-type e3)))

(define (union-type t0 t1)
  (or (type<: t0 t1)
      (type<: t1 t0)
      the-unknown-type))

(define (bounds-is-check e1 e2 ctx)
  (unless (in-context? forge:bounds)
    (raise-type-error "found 'is' bound outside of a bounds block" (deparse ctx)))
  (let ((bound-val (deparse/datum e2)))
    (unless (eq? 'linear bound-val)
      (raise-type-error "unsupported bound 'is ~a'" (deparse ctx))))
  (void ;; lhs must be a field
    (with-forge-context forge:subexpr
      (expr-check e1))
    (field->sigty/error e1))
  the-bool-type)

(define (binop-check op e1 e2 ctx #:negate? [negate? #f])
  (define e1-ty (get-type e1))
  (define e2-ty (get-type e2))
  ;; TODO
  ;; negate? may not matter for typing
  (syntax-parse op
   [ao:$ArrowOp
    ;; TODO unless in example
    (unless (or (in-context? forge:example)
                (in-context? forge:inst))
      (raise-operator-error #'ao.arr))
    the-bool-type]
   [(~or "-" "&" "+")
    (unless (or (in-context? forge:example)
                (in-context? forge:inst))
      (raise-operator-error op))
    the-bool-type]
   [(~or (~datum =) (~datum <) (~datum <=) (~datum >=) (~datum >)
         "or" "and" "&&" "iff" "implies" "=>")
    (define needs-int?
      (let ((sym (syntax-e op)))
        (and (symbol? sym) (memq sym '(< <= >= >)))))
    (unless (or (in-context? forge:example)
                (in-context? forge:inst))
      (define sig1 (->sigty e1-ty))
      (define sig2 (->sigty e2-ty))
      (void
        (for ((t (in-list (list e1-ty e2-ty)))
              (s (in-list (list sig1 sig2)))
              #:when (and (nametype? t) (id=? (type-name t) (type-name s)))
              #:unless (one-mult? (sigtype-mult s)))
          (raise-type-error
            "expected an object"
            (deparse t #:ctx ctx))))
      (void
        (if needs-int?
          (for ((tt (in-list (list sig1 sig2)))
                #:unless (sigtype<=: tt the-int-type))
            (raise-type-error
              (format "expected an Int, got ~s" (deparse/datum tt))
              (deparse ctx)))
          (unless (type-similar? sig1 sig2)
            (raise-type-error
              (format "inputs to ~a must have similar type, got ~s and ~s"
                      (syntax-e op)
                      (deparse/datum sig1)
                      (deparse/datum sig2))
              (deparse ctx))))))
    the-bool-type]
   [_
    (log-froglet-warning "binop-check: (~e ~e ~e)" op e1-ty e2-ty)
    the-bool-type]))

(define (unop-check op e1 #:negate? [negate? #f])
  ;; TODO use negate?
  (define e1-ty (get-type e1))
  (syntax-parse op
   [(~or "*" "^" "~")
    (raise-operator-error op)]
   [(~or "#")
    (unless (reltype? e1-ty)
      (raise-type-error
        "expected a relation"
        (deparse e1)))
    the-int-type]
   [_
    (todo-not-implemented (format "unop-check: (~e ~e)" op e1-ty))
    the-unknown-type]))

(define (app-check fn arg* ctx #:negate? [negate? #f])
  ;; TODO use negate?
  (define fn-ty (get-type fn))
  (define arg-ty* (map get-type arg*))
  (cond
    [(and (nametype? fn-ty)
          (eq? 'reachable (syntax-e (type-name fn-ty))))
     (when (or (null? arg*)
               (null? (cdr arg*))
               (null? (cddr arg*)))
       (raise-type-error
         "reachable expects 3 or more arguments"
         (deparse ctx)))
     (define arg0 (first arg*))
     (define arg0-sigty (->sigty (first arg-ty*)))
     (void ;; TODO use weaker check here?
       (join-check-lhs arg0 arg0-sigty ctx))
     (define arg1 (second arg*))
     (define arg1-sigty (->sigty (second arg-ty*)))
     (void (join-check-lhs arg1 arg1-sigty ctx))
     (define field* (cddr arg*))
     (define f-parent* (map field->sigty/error field*))
     ;; march forward using sigs worklist via all fields
     ;;  track all visited sigs and fields
     (let loop ((worklist (list arg1-sigty))
                (sig* '()) ;; should be a hash (set), but needs to use type-equal?
                (field# (hash)))
       (cond
         [(null? worklist)
          ;; error if src not exited
          (when (hash-empty? field#)
            (raise-type-error
              (format "reachable found no field matching source sig ~a" (deparse/datum arg1-sigty))
              (deparse ctx)))
          ;; error if tgt not visited
          (unless (member arg0-sigty sig* type-equal?)
            (raise-type-error
              (format "reachable found no path to target sig ~a" (deparse/datum arg0-sigty))
              (deparse ctx)))
          ;; error if unused field
          (for ((field (in-list field*)))
            (unless (hash-has-key? field# field)
              (raise-type-error
                (format "reachable cannot use field ~a" (deparse/datum field))
                (deparse ctx))))
          (void)]
         [else
          (define curr-sig (first worklist))
          (define work+ (rest worklist))
          (define-values [curr-field* next-sig*]
            (for*/lists (_1 _2)
                        ((field (in-list field*))
                         (f-parent (in-value (field->sigty (get-type field))))
                         #:when (sigtype<=: curr-sig f-parent))
              (values field (sigtype-find-field f-parent (get-type field) ctx))))
          (loop (append work+ (filter-not (lambda (s) (member s sig* type-equal?)) next-sig*))
                (cons curr-sig sig*)
                (apply hash-set* field# (append-map (lambda (f) (list f #true)) curr-field*)))]))
     the-bool-type]
    [else
     (printf "APP check ~a[~a]~n" (get-type fn) (map get-type arg*))
     (log-froglet-warning "app check not implemented ~a" (deparse/datum ctx))
     the-unknown-type]))

(define-parser letdecllist-check
  [decls:$LetDeclList
   (decl*-check (syntax-e #'(decls.decls ...)) 'one)]
  [_
    (log-froglet-warning "letdecllist-check: unknown stx ~a" (syntax->datum this-syntax))
    (void)])

(define-parser decllist-check
  [decls:$DeclList
   (decl*-check (syntax-e #'(decls.decls ...)) (current-quant))]
  [_
    (log-froglet-warning "decllist-check: unknown stx ~a" (syntax->datum this-syntax))
    (void)])

(define-parser decl-check
  [decl:$Decl
    (decl*-check (syntax-e #'(decl.decls ...)) (current-quant))]
  [_
    (log-froglet-warning "decl-check: unknown stx ~a" (syntax->datum this-syntax))
    (void)])

(define (decl*-check name+expr* mult)
  (for/list ([ne-stx (in-list name+expr*)])
    (define ne (syntax-e ne-stx))
    (define name (car ne))
    (define expr (cadr ne))
    (define expr-ty
      (with-forge-context forge:expr
        (expr-check expr)))
    (define tt (paramtype name mult expr-ty))
    (set-type ne-stx tt)))

(define-parser blockorbar-check
  [bob:$BlockOrBar
   (syntax-parse #'bob.exprs
    [bb:$Block
      (set-type this-syntax (block-check #'bb))]
    [ee:$Expr
      (define ty (with-forge-context forge:expr (expr-check #'ee)))
      (set-type this-syntax ty)])]
  [_
    (log-froglet-warning "blockorbar-check: unknown stx ~a" (syntax->datum this-syntax))
    the-unknown-type])

(define (join-check e1 e2 ctx)
  #;(log-froglet-info "join-check: context ~s" (current-forge-context))
  (define e1-ty (get-type e1))
  (define e2-ty (get-type e2))
  (define e1-sigty (->sigty e1-ty))
  (join-check-lhs e1 e1-sigty ctx)
  (join-check-rhs e1 e1-sigty e2 ctx))

(define (join-check-lhs e1 e1-sigty ctx)
  (define e1-ty (get-type e1))
  (when (->paramty e1-ty)
    (unless (singleton-sigtype? e1-sigty)
      (raise-type-error
        "expected a singleton sig"
        (deparse e1))))
  (when (and (context=? forge:expr)
             (not (sigtype? e1-sigty)))
    (raise-type-error
      "expected an object"
      (deparse e1))))

(define (join-check-rhs e1 e1-sigty e2 ctx)
  (define e2-parent (field->sigty/error e2))
  (unless (sigtype<=: e1-sigty e2-parent)
    (raise-type-error
      (format "object ~a has no field ~a" (deparse/datum e1) (deparse/datum e2))
      (deparse ctx)))
  (sigtype-find-field e2-parent (get-type e2) ctx))

(define (field->sigty/error e2)
  (define e2-ty
    (get-type e2))
  (define e2-sigty
    (or (field->sigty e2-ty)
        (and (nametype? e2-ty) (name->sig (type-name e2-ty)))))
  (if (sigtype? e2-sigty)
    e2-sigty
    (raise-type-error
      "expected a field"
      (deparse e2))))

(define (atom->type sym ctx)
  (if (symbol? sym)
    (datum->syntax ctx sym)
    sym))

(define (param*->paramtype* stx*)
  (filter values (map param->paramtype stx*)))

(define-parser param->paramtype
  ;; param ~ the decl part of a pred
  [(~or
    (nm:id (_:ExprHd qn:$QualName))
    (nm:id (_:ExprHd mm:$MultStr (_:ExprHd qn:$QualName))))
   ;; TODO if qn is arity 1 / a sig, default multiplicity is one
   ;; if qn is higher arity (A -> B), default is set
   (define multiplicity (or (syntax-e #'mm) "one"))
   (paramtype #'nm (string->symbol multiplicity) #'qn.name)]
  [_
   (log-froglet-warning "param->paramtype: unknown syntax ~e" this-syntax)
   #f])

(define-parser return->type
  [(~or
    (_:ExprHd qn:$QualName)
    (_:ExprHd mm:$MultStr (_:ExprHd qn:$QualName)))
   ;; TODO default multiplicity?
   (nametype #'qn.name)]
  [_
   (log-froglet-warning "param->paramtype: unknown syntax ~e" this-syntax)
   #f])

(define (parse-arrow-decl* decl*)
  (map parse-arrow-decl decl*))

(define (parse-arrow-decl decl)
  (syntax-parse decl
   [ad:$ArrowDecl
    (define name (syntax-e #'(ad.name* ...)))
    (define sig (syntax-e #'(ad.type* ...)))
    (define mult
      (let* ([sym (syntax-e #'ad.mult)])
        (if (memq sym '(one lone func pfunc))
          sym
          (raise-type-error
            "invalid field declaration, must be one, lone, func, or pfunc"
            (if (eq? sym 'default)
              (syntax/loc #'ad.mult set)
              #'ad.mult)))))
    (fieldtype name mult sig)]))

;; ---

(define (sigtype<=: s0 s1)
  (or (type-equal? s0 s1)
      (unknown-type? s0)
      (unknown-type? s1)
      (sigtype<: s0 s1)))

(define (sigtype<: s0 s1)
  (let ((xx (sigtype-extends s0)))
    (and xx (sigtype<=: (->sigty xx) s1))))

(define (type=? t1 t2)
  (or (type-equal? t1 t2)
      (unknown-type? t1)
      (unknown-type? t2)
      (and (sigtype? t1) (sigtype? t2) (sigtype<: t1 t2) (sigtype<: t2 t1))))

(define (type-similar? t1 t2)
  (or (type-equal? t1 t2)
      (unknown-type? t1)
      (unknown-type? t2)
      (and (sigtype? t1)
           (sigtype? t2)
           (or (sigtype<: t1 t2)
               (sigtype<: t2 t1)))))

(define (type<: t0 t1)
  (match* (t0 t1)
   [(_ _)
    #:when (eq? t0 t1)
    t0]
   [((== the-unknown-type) _)
    the-unknown-type]
   [(_ (== the-unknown-type))
    the-unknown-type]
   [((nametype nm) (== the-bool-type eq?))
    #:when (memq (syntax-e nm) '(true false))
    the-bool-type]
   [(_ (== the-bool-type eq?))
    #false]
   [((nametype n0) t1)
    (type<: (name->sig n0) t1)]
   [(t0 (nametype n1))
    (type<: t0 (name->sig n1))]
   [(_ _)
    (log-froglet-warning "inexhaustive match: ~s <: ~s" t0 t1)
    #f]))

(define (->sigty stx)
  (cond
    [(sigtype? stx)
     stx]
    [(or (nametype? stx)
         (consttype? stx))
     (name->sig (type-name stx))]
    [else
     (name->sig stx)]))

(define (->boolty ty)
  (if (nametype? ty)
    (let ((vv (name-lookup (type-name ty))))
      (if (and (predtype? vv) (null? (predtype-param* vv)))
        the-bool-type
        ty))
    ty))

(define (->paramty stx)
  (cond
    [(nametype? stx)
     (name->paramtype (type-name stx))]
    [else
     (name->paramtype stx)]))

(define (name->paramtype stx)
  (name-lookup stx paramtype?))

(define (->stx x)
  (cond
    [(type? x)
     (type-name x)]
    [else
      x]))

(define (deparse stx #:ctx [-ctx #f])
  (define ctx (or -ctx stx))
  (datum->syntax ctx (deparse/datum stx) ctx))

(define-parser deparse/datum
  [(_:ExprHd cc:$Const)
   (syntax-e #'cc.translate)]
  [(_:ExprHd qn:$QualName)
   (syntax-e #'qn.name)]
  [(_:ExprHd q:$QuantStr body)
   (cons (string->symbol (syntax-e #'q))
         (deparse/datum #'body))]
  [(_:ExprHd e1
             (~optional negate:$NotOp) (~or op:$CompareOp op:$BinaryOp)
             e2
             (~optional (~seq "else" e3:$Expr) #:defaults ([e3 #'#f])))
   (define e1+ (deparse/datum #'e1))
   (define e2+ (deparse/datum #'e2))
   (define op+ (syntax-e #'(~? op.symbol op)))
   (define neg (syntax-e #'(~? negate #f)))
   (define e3* (if (syntax-e #'e3) (list "else" (deparse/datum #'e3)) '()))
   (filter values (list* e1+ neg op+ e2+ e3*))]
  [(_:ExprHd a "." b)
   (list (deparse/datum #'a) "." (deparse/datum #'b))]
  [(_:ExprHd e1 "[" ee:$ExprList "]")
   (list (deparse/datum #'e1)
         #;"["
         (map deparse/datum (syntax-e #'(ee.exprs ...)))
         #;"]")]
  [(_:ExprHd "{" decls:$DeclList bob:$BlockOrBar "}")
   (list "{"
         (deparse/datum #'decls)
         "|"
         (deparse/datum #'bob)
         "}")]
  [dd:$DeclList
    (for*/list ((-decl (in-list (syntax-e #'(dd.decls ...))))
                (decl (in-value (syntax-e -decl))))
      (list (syntax-e (car decl)) (deparse/datum (cadr decl))))]
  [bb:$BlockOrBar
    (deparse/datum #'bb.exprs)]
  [_
   #:when (type? (syntax-e this-syntax))
   (syntax-e (type-name (syntax-e this-syntax)))]
  [_
   (todo-not-implemented (format "deparse/datum: ~s" (syntax->datum this-syntax)))
   this-syntax])

(define (name-check stx)
  (set-type stx (name-lookup stx)))

(define (name->sig stx)
  (or (name-lookup stx sigtype?)
      (let* ([paramty (->paramty stx)]
             [sigty (and paramty (->sigty (paramtype-sig paramty)))]
                    ; TODO ?? (name->sig (type-name ^^^ ))
             [sigty (and sigty
                         (sigtype-update-mult
                           sigty
                           (paramtype-mult paramty)))])
        #;(printf "name->sig fallback:~n paramty ~s~n sigty ~s~n mult ~s~n" paramty sigty p-mult)
        sigty)
      #;(raise-type-error "no type for name" stx)
      the-unknown-type))

(define (name->pred stx)
  (name-lookup stx predtype?))

(define id=? free-identifier=?) ;; fails on bridge_crossing.frg

(define (name-lookup stx [-env-guard #f])
  (define env-guard (or -env-guard values))
  (define stx=? (make-stx=? stx))
  (for/first ([elem (in-list (current-type-env))]
              #:when (and (env-guard elem)
                          (stx=? (type-name elem))))
    elem))

(define (make-stx=? stx)
  (if (syntax? stx)
    (lambda (that) (id=? stx that))
    (lambda (that) (eq? stx (syntax-e that)))))

(define (field->sigty idty)
  (define stx=? (make-stx=? (type-name idty)))
  (for/first ([elem (in-list (current-type-env))]
              #:break (stx=? (type-name elem))
              ;; 2023-02-11: the #:break is for shadowing; should we use a general lookup function instead?
              #:when (and (sigtype? elem)
                          (sigtype-has-field? elem idty)))
    elem))

(define (sigtype-has-field? sigty idty)
  (define id (type-name idty))
  (for*/or ((ft (in-list (sigtype-field* sigty)))
            (nm (in-list (type-name ft))))
    (free-identifier=? id nm)))

(define (sigtype-find-field sigty idty ctx)
  (define id (type-name idty))
  (or
    (for/or ([ft (in-list (sigtype-field* sigty))])
      (for/or ([nm (in-list (type-name ft))]
               [ty (in-list (fieldtype-sig ft))]
               #:when (free-identifier=? id nm))
        (->sigty ty)))
    (raise-type-error
      (format "field '~s' not found" (syntax-e id))
      (deparse ctx))))

(define (env-check env)
  (unknown-sig-check env)
  (void))

(define (env-sig-ids env)
  (for/fold ([acc (immutable-free-id-set)])
            ([vv (in-list env)]
             #:when (sigtype? vv))
    (free-id-set-add acc (type-name vv))))

(define (env-pred-ids env)
  (for/list ((vv (in-list env))
             #:when (predtype? vv))
    (type-name vv)))

(define (unknown-sig-check env)
  (define ids (env-sig-ids env))
  (for ((vv (in-list env)))
    (let loop ((vv vv))
      (cond
        [(sigtype? vv)
         (unknown-sig-check/sig vv ids)]
        [(or (nametype? vv) (consttype? vv))
         (loop (name->sig (type-name vv)))]
        [(predtype? vv)
         (unknown-sig-check/pred vv ids)]
        [(funtype? vv)
         (unknown-sig-check/fun vv ids)
         (loop (funtype->return vv))]
        [(reltype? vv)
         (map loop (reltype-col-type* vv))]
        [(unknown-type? vv)
         (void)]
        [else
         (log-froglet-warning "env-fold: unexpected arg ~e" vv)
         (void)]))))

(define (unknown-sig-check/sig sigty ids)
  (for ([fieldty (in-list (sigtype-field* sigty))])
    (unknown-sig-check/field fieldty ids)))

(define (unknown-sig-check/pred predty ids)
  (for ([ft (in-list (predtype-param* predty))])
    (define nm (paramtype-sig ft))
    (unless (free-id-set-member? ids nm)
      (raise-unknown-sig-error nm))
    (void)))

(define (unknown-sig-check/fun predty ids)
  (for ([ft (in-list (funtype-param* predty))])
    (define nm (paramtype-sig ft))
    (unless (free-id-set-member? ids nm)
      (raise-unknown-sig-error nm))
    (void)))

(define (unknown-sig-check/field fieldty ids)
  (for ((nm (in-list (fieldtype-sig fieldty))))
    (unless (free-id-set-member? ids nm)
      (raise-unknown-sig-error nm)))
  (void))

(define (env-elem? x)
  (or (sigtype? x)
      (predtype? x)))

(define (env-extend . env*)
  (apply append env*))

(define (env-serialize env)
  (for/list ((elem (in-list env)))
    (type-serialize elem)))

;; ---

(define (env-collect stx)
  (syntax-parse stx
    #:datum-literals (AlloyModule ModuleDecl Import)
    ;; [ev:EvalDecl ...]
    [mod:$AlloyModule
     (define env0 (append-map env-collect/paragraph (syntax-e #'(mod.parag* ...))))
     (define env1 (append-map env-collect/import (syntax-e #'(mod.import* ...))))
     (values env0 env1)]))

(define (env-collect/paragraph stx)
  (syntax-parse stx
   [sig:$SigDecl
    (define mult
      (let ((mm (syntax-e #'(~? sig.mult #f))))
        (and mm (keyword->symbol mm))))
    (define extends (syntax-e #'(~? sig.extends #f)))
    (define field* (parse-arrow-decl* (syntax-e #'(sig.relation-decl* ...))))
    (for/list ([name (in-list (syntax-e #'(sig.name* ...)))])
      (sigtype name mult extends field*))]
   [pred:$PredDecl
    (define param-ty* (param*->paramtype* (syntax-e #'(pred.decls ...))))
    (list (make-predtype #'pred.name param-ty*))]
   [fun:$FunDecl
    (define param-ty* (param*->paramtype* (syntax-e #'(fun.decls ...))))
    (define out-ty (return->type #'fun.output))
    (list (make-funtype #'fun.name param-ty* out-ty))]
   [assert:$AssertDecl
    (todo-not-implemented 'env-collect/paragraph:AssertDecl)
    '()]
   [cmd:$CmdDecl
     ;; TODO anything to check?
    '()]
   [testexpect:$TestExpectDecl
    '()]
   [sexpr:$SexprDecl
    (todo-not-implemented 'env-collect/paragraph:SexprDecl)
    '()]
   [query:$QueryDecl
    (todo-not-implemented 'env-collect/paragraph:QueryDecl)
    '()]
   [option:$OptionDecl
     '()]
   [inst:$InstDecl
    '()]
   [example:$ExampleDecl
    '()]
   [_
    (log-froglet-warning "env-collect/paragraph: unknown syntax ~e" this-syntax)
    '()]))

(define (env-collect/import stx)
  (syntax-parse stx
   [ii:$Import
    (define env (file->froglet-env (syntax-e #'ii.file-path) stx))
    (log-froglet-info "import env ~s" env)
    env]))

(define (file->froglet-env fn ctx)
  (datum->type-env (file->froglet-env-datum fn) ctx))

(define (dyn-require-fail-thunk)
  '())

(define (file->froglet-env-datum fn)
  (with-handlers ((exn:fail:contract? (lambda (exn) (dyn-require-fail-thunk))))
    (parameterize ([current-namespace (make-base-namespace)])
      ;; id defined in froglet
      (dynamic-require `(submod ,fn ,type-env-name) type-env-name dyn-require-fail-thunk))))

(define (datum->type-env x* ctx)
  (for/list ((xx (in-list x*)))
    (cond
      [(sigtype? xx)
       (sig-datum->type xx ctx)]
      [(predtype? xx)
       (pred-datum->type xx ctx)]
      [else
        (log-froglet-warning "datum->type-env: cannot revive ~e" xx)
        xx])))

(define (sig-datum->type ss ctx)
  (sigtype (atom->type (type-name ss) ctx)
           (atom->type (sigtype-mult ss) ctx)
           (atom->type (sigtype-extends ss) ctx)
           (for/list ((ff (in-list (sigtype-field* ss))))
             (field-datum->type ff ctx))))

(define (pred-datum->type pp ctx)
  (predtype (atom->type (type-name pp) ctx)
            (for/list ((p (in-list (predtype-param* pp))))
              (param-datum->type p ctx))))

(define (field-datum->type ff ctx)
  (fieldtype (for/list ((nn (in-list (type-name ff))))
               (atom->type nn ctx))
             (atom->type (fieldtype-mult ff) ctx)
             (for/list ((ss (in-list (fieldtype-sig ff))))
               (atom->type ss ctx))))

(define (param-datum->type pp ctx)
  (paramtype (atom->type (type-name pp) ctx)
             (atom->type (paramtype-mult pp) ctx)
             (atom->type (paramtype-sig pp) ctx)))

(define (keyword->symbol x)
  (string->symbol (keyword->string x)))

