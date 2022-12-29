#lang racket/base

;; TODO can we use turnstile?
;;  collect env and then use turnstile?

(provide
  typecheck)

(require
  forge/lang/alloy-syntax/syntax-class
  froglet/typecheck/error
  froglet/typecheck/struct
  racket/list
  racket/pretty
  syntax/id-set
  syntax/parse)

(define-syntax-rule (define-parser id clause ...)
  (define id (syntax-parser clause ...)))

;; -----------------------------------------------------------------------------

(define current-type-env (make-parameter '()))

(define (typecheck mod)
  (define env (collect-env mod))
  (pretty-write env)
  (env-check env)
  ;; TODO bounds
  (parameterize ([current-type-env env])
    (mod-check mod))
  mod)

(define-parser mod-check
  [mod:$AlloyModule
   (for ((par (in-list (syntax-e #'(mod.parag* ...)))))
     (parag-check par))
   (for ((exp (in-list (syntax-e #'(mod.expr* ...)))))
     (expr-check exp))
   (void)]
  [_
    (void)])

(define-parser parag-check
  ;; TODO what are the cases???
  [sig:$SigDecl
    ;; TODO
    #;(raise-user-error 'parag-check "sig not implemented ~a" (syntax->datum #'sig))
    (void)]
  [pred:$PredDecl
    (block-check #'pred.block)]
  [par
    (raise-user-error 'parag-check "unknown stx ~a" (syntax->datum #'par))
    (void)])

(define-parser block-check
  [bb:$Block
    (for-each expr-check (syntax-e #'(bb.exprs ...)))])

;; ---
;;

(define-parser expr-check
  [exp:$Expr
    ;; TODO syntax class for those muliplicity ops
    ;; TODO ... copy things over from expr macro
    (let loop ([stx (syntax/loc this-syntax (exp.body ...))])
      (syntax-parse stx
       [("let" decls:$LetDeclList bob:$BlockOrBar)
        ;; TODO
        ;; - check letdecl
        ;; - update env
        ;; - check bob
        (error 'die1)]
       [("bind" decls:$LetDeclList bob$BlockOrBar)
        ;; bind not implemented
        (void)]
       [(q:$Quant (~optional (~and "disj" disj)) decls:$DeclList bob:$BlockOrBar)
        (define decl-tys (decls-check #'decls))
        (parameterize ((current-type-env (env-extend (current-type-env) decl-tys)))
          (blockorbar-check #'bob))]
       [(e1:$Expr _:$BinaryOp
                  e2:$Expr
                  (~optional (~seq "else" e3:$Expr)))
        ;; TODO
        (error 'die4)]
       [(lhs:$Expr (~datum ".") rhs:$Expr)
        ;; ??
        (define lhs-ty (loop #'(lhs.body ...)))
        (define rhs-ty (loop #'(rhs.body ...)))
        (or (field-check lhs-ty rhs-ty)
            (raise-syntax-error 'froglet:typecheck
                                "Expected matching sig and field"
                                this-syntax))]
       [(_:$UnaryOp e1:$Expr)
         ;; TODO
         (error 'die5)]
       [(e1:$Expr (~optional (~and (~or "!" "not") not)) _:$BinaryOp e2:$Expr)
        ;; TODO
        (error 'die6)]
       [(_:$QuantStr exp:$Expr)
        ;; ??
        (loop (syntax/loc this-syntax (exp.body ...)))]
       [(e1:$Expr "'")
         ;; TODO
         (error 'die7)]
       [("[" _:$ExprList "]")
        ;; not implemented in expander
        (void)]
       [(name:$Name "[" _:$ExprList "]")
        (error 'die8)]
       [(ex1:$Expr "[" _:$ExprList "]")
        (error 'die9)]
       [(cn:$Const)
        #'cn.translate]
       [(qn:$QualName)
        #'qn.name]
       [("this")
        (error 'die10)]
       [("`" nm:$Name)
        (error 'die11)]
       [("{" decls:$DeclList bob:$BlockOrBar "}")
        (error 'die12)]
       [(block:$Block)
        (error 'die13)]
       [(sexpr:$Sexpr)
        (error 'die14)]
       [_
         ;; ??
        (raise-syntax-error 'expr-check "internal error parsing expr" stx)]))]
  [_
    ;; TODO more cases, for the things inside exprs?
    (void)]
  [_
    (raise-argument-error 'expr-check "$Expr" this-syntax)])

(define-parser decls-check
  ;; TODO map instead?
  [decl
    (raise-user-error 'decls-check "not impled ~s~n" this-syntax)])

(define-parser blockorbar-check
  [bob
    (raise-user-error 'blockorbar-check "not impled ~s~n" this-syntax)])

(define (field-check lhs rhs)
  (define lhs-sigty
    (or (->sigty lhs)
        (raise-syntax-error 'froglet:typecheck
                            "Expected sig"
                            lhs)))
  (define rhs-sigty
    (or (field->sigty rhs)
        (raise-syntax-error 'froglet:typecheck
                            "Expected field"
                            rhs)))
  (and (sigtype<: lhs-sigty rhs-sigty)
       (sigtype-find-field rhs-sigty rhs)))

(define (sigtype<: s0 s1)
  (or (eq? s0 s1)
      (let ((xx (sigtype-extends s0)))
        (and xx (sigtype<: (->sigty xx) s1)))))

(define (->sigty stx)
  (cond
    [(sigtype? stx)
     stx]
    [else
     (define stx=?
       (if (syntax? stx)
         (lambda (that) (free-identifier=? stx that))
         (lambda (that) (eq? stx (syntax-e that)))))
     (for/first ([elem (in-list (current-type-env))]
                 #:when (and (sigtype? elem)
                             (stx=? (type-name elem))))
       elem)]))

(define (field->sigty stx)
  (for/first ([elem (in-list (current-type-env))]
              #:when (and (sigtype? elem)
                          (sigtype-has-field? elem stx)))
    elem))

(define (sigtype-has-field? sigty id)
  (for*/or ([ft (in-list (sigtype-field* sigty))]
            [nm (in-list (type-name ft))])
    (free-identifier=? id nm)))

(define (sigtype-find-field sigty id)
  (or
    (for/or ([ft (in-list (sigtype-field* sigty))])
      (for/or ([nm (in-list (type-name ft))]
               [ty (in-list (fieldtype-sig ft))]
               #:when (free-identifier=? id nm))
        (->sigty ty)))
    (raise-user-error 'sigtype-find-field "Field not found")))

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
    (cond
      [(sigtype? vv)
       (unknown-sig-check/sig vv ids)]
      [(predtype? vv)
       (unknown-sig-check/pred vv ids)]
      [else
        (raise-argument-error 'env-fold "env-elem?" vv)])))

(define (unknown-sig-check/sig sigty ids)
  (for ([fieldty (in-list (sigtype-field* sigty))])
    (unknown-sig-check/field fieldty ids)))

(define (unknown-sig-check/pred predty ids)
  (for ([pp (in-list (predtype-param* predty))])
    ;; ids
    (raise-user-error 'unknown-sig-check/pred "not implemented")))

(define (unknown-sig-check/field fieldty ids)
  (for ((nm (in-list (fieldtype-sig fieldty))))
    (unless (free-id-set-member? ids nm)
      (raise-syntax-error 'froglet:typecheck
                          "Undefined sig name"
                          nm)))
  (void))

(define (env-elem? x)
  (or (sigtype? x)
      (predtype? x)))

(define (env-extend a b)
  (append a b))

;; ---

(define (collect-env stx)
  (syntax-parse stx
    #:datum-literals (AlloyModule ModuleDecl Import)
    ;; [ev:EvalDecl ...]
    [mod:$AlloyModule
     #:fail-unless (null? (syntax-e #'(mod.import* ...)))
                   "Cannot typecheck module with Import statements"
     ; (printf "Module-decl ~a~n~n" #'(~? mod.moduledecl "None present"))
     ; (printf "Paragraphs: ~a~n~n" #'(mod.parag* ...))
     ;; TODO (printf "Exprs: ~a~n~n" #'(mod.expr* ...))
     (append-map collect-env/paragraph (syntax-e #'(mod.parag* ...)))]))

(define (collect-env/paragraph stx)
  (syntax-parse stx
   [sig:$SigDecl
     (define mult (syntax-e #'(~? sig.mult #f)))
     (define extends (syntax-e #'(~? sig.extends #f)))
     (define field* (parse-arrow-decl* (syntax-e #'(sig.relation-decl* ...))))
     (for/list ([name (in-list (syntax-e #'(sig.name* ...)))])
       (sigtype name mult extends field*))]
   [pred:$PredDecl
     (list (predtype #'pred.name (syntax-e #'(~? pred.decls ()))))]
   [fun:$FunDecl
     (raise-arguments-error 'collect-env/paragraph "not implemented" "stx" stx)]
   [assert:$AssertDecl
     (raise-arguments-error 'collect-env/paragraph "not implemented" "stx" stx)]
   [cmd:$CmdDecl
     (raise-arguments-error 'collect-env/paragraph "not implemented" "stx" stx)]
   [testexpect:$TestExpectDecl
     '()]
   [sexpr:$SexprDecl
     (raise-arguments-error 'collect-env/paragraph "not implemented" "stx" stx)]
   [query:$QueryDecl
     (raise-arguments-error 'collect-env/paragraph "not implemented" "stx" stx)]
   [evalrel:$EvalRelDecl
     (raise-arguments-error 'collect-env/paragraph "not implemented" "stx" stx)]
   [option:$OptionDecl
     (raise-arguments-error 'collect-env/paragraph "not implemented" "stx" stx)]
   [inst:$InstDecl
     (raise-arguments-error 'collect-env/paragraph "not implemented" "stx" stx)]
   [example:$ExampleDecl
     (raise-arguments-error 'collect-env/paragraph "not implemented" "stx" stx)]
   [_
     (raise-argument-error 'collect-env/paragraph "Paragraph?" stx)]))

(define (parse-arrow-decl* decl*)
  (map parse-arrow-decl decl*))

(define (parse-arrow-decl decl)
  (syntax-parse decl
   [ad:$ArrowDecl
    (define name (syntax-e #'(ad.name* ...)))
    (define sig (syntax-e #'(ad.type* ...)))
    (define mult (syntax-e #'ad.mult))
    (fieldtype name mult sig)]))

