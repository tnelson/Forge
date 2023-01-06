#lang racket/base

;; TODO can we use turnstile?
;;  collect env and then use turnstile?

;; TODO annotate syntax as we go ... build a map like TR
;; - (hasheq syntax? type?)
;; - ... any more?

(provide
  typecheck)

(require
  forge/lang/alloy-syntax/syntax-class
  (only-in froglet/util log-froglet-info)
  froglet/typecheck/error
  froglet/typecheck/struct
  racket/list
  racket/pretty
  syntax/id-set
  syntax/parse)

(define-syntax-rule (define-parser id clause ...)
  (define id (syntax-parser clause ...)))

(define who 'froglet:typecheck)

;; -----------------------------------------------------------------------------

(define current-type-env (make-parameter '()))

(define current-quant (make-parameter #f))
;; (or/c #f               ;; uninitialized
;;       symbol?          ;; quant
;;       (list symbol?    ;; quant
;;             boolean?)) ;; disj?

(define (typecheck mod)
  (define env (env-extend (env-init) (env-collect mod)))
  (log-froglet-info (pretty-format env 120 #:mode 'write))
  (env-check env)
  ;; TODO record all type info ... in a hash (easy) or as syntax properties (hard)
  ;; TODO bounds-check
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
  [sig:$SigDecl
    ;; TODO any check needed?
    (void)]
  [fn:$FactDecl
   (error 'die2)]
  [pred:$PredDecl
    ;; TODO pred.decls
    (block-check #'pred.block)]
  [fd:$FunDecl
    (error 'die3)]
  [ad:$AssertDecl
    (error 'die4)]
  [cd:$CmdDecl
    (error 'die5)]
  [td:$TestExpectDecl
    (testblock-check #'td.test-block)]
  [sd:$SexprDecl
    (error 'die7)]
  [rd:$RelDecl
    (raise-arguments-error 'parag-check "not implemented for RelDecl" "stx" this-syntax)]
  [rd:$OptionDecl
    (error 'die9)]
  [rd:$InstDecl
    (error 'die10)]
  [rd:$ExampleDecl
    (error 'die7)]
  [par
    (raise-user-error 'parag-check "unknown stx ~a" (syntax->datum #'par))
    (void)])

(define-parser block-check
  [bb:$Block
    (for/last ([ee (in-list (syntax-e #'(bb.exprs ...)))])
      (expr-check ee))])

(define-parser testblock-check
  [tb:$TestBlock
    (for-each testdecl-check (syntax-e #'(tb.test-decls ...)))])

(define-parser testdecl-check
  [td:$TestDecl
    ;; TODO hd name parameters pred-name pred-block scope bounds
    (when (syntax-e #'td.name)
      (printf "TD name ~s~n" #'td.name))
    (when (attribute td.parameters)
      (printf "TD params ~s~n" #'td.parameters))
    (when (syntax-e #'td.pred-name)
      (name-check #'td.pred-name))
    (when (syntax-e #'td.pred-block)
      (block-check #'td.pred-block))
    (scope-check #'td.scope)
    (bounds-check #'td.bounds)
    (void)])

(define (scope-check stx)
  ;; TODO
  (void))

(define (bounds-check stx)
  ;; TODO
  (void))

(define-parser sexpr-check
  [ss:$Sexpr
    (raise-arguments-error 'sexpr-check "not implemented SEXPR" "expr" this-syntax)])

(define-parser expr-check
  [exp:$Expr
    ;; TODO syntax class for those muliplicity ops
    ;; TODO ... copy things over from expr macro
    (let loop ([stx (syntax/loc this-syntax (exp.body ...))])
      (syntax-parse stx
       [("let" decls:$LetDeclList bob:$BlockOrBar)
        (define decl-tys (letdecllist-check #'decls))
        (parameterize ((current-type-env (env-extend (current-type-env) decl-tys)))
          (blockorbar-check #'bob))]
       [("bind" decls:$LetDeclList bob$BlockOrBar)
        ;; bind not implemented in normal Forge
        (void)]
       [(q:$Quant (~optional (~and "disj" disj) #:defaults ([disj #'#f]))
                  decls:$DeclList bob:$BlockOrBar)
        (define decl-tys
          (parameterize ([current-quant (list (syntax-e #'q.symbol) (and (syntax-e #'disj) #t))])
            (decllist-check #'decls)))
        (parameterize ((current-type-env (env-extend (current-type-env) decl-tys)))
          (blockorbar-check #'bob))]
       [(e1:$Expr (~optional (~and (~or "!" "not") negate) #:defaults ([negate #'#f]))
                  (~or -op:$CompareOp -op:$BinaryOp)
                  e2:$Expr
                  (~optional (~seq "else" e3:$Expr) #:defaults ([e3 #'#f])))
        (define e1-ty (expr-check #'e1))
        (define e2-ty (expr-check #'e2))
        (cond
          [(syntax-e #'e3)
           (define e3-ty (expr-check #'e3))
           (ifelse-check e1-ty e2-ty e3-ty this-syntax)]
          [else
           (define op #'(~? -op.symbol -op))
           ;; TODO throw error up?
           (binop-check op e1-ty e2-ty this-syntax #:negate? (and (syntax-e #'negate) #t))])]
       [(lhs:$Expr (~datum ".") rhs:$Expr)
        ;; ??
        (define lhs-ty (loop #'(lhs.body ...)))
        (define rhs-ty (loop #'(rhs.body ...)))
        (or (field-check lhs-ty rhs-ty)
            (raise-syntax-error who
                                "Expected matching sig and field"
                                this-syntax))]
       [(op:$UnaryOp e1:$Expr)
        (define e1-ty (expr-check #'e1))
        (unop-check #'op e1-ty)]
       [(qq:$QuantStr exp:$Expr)
        ;; bg: 12/31 what does this mean?
        (parameterize ([current-quant (string->symbol (syntax-e #'qq))])
          (loop (syntax/loc this-syntax (exp.body ...))))]
       [(e1:$Expr "'")
        ;; TODO ... is this a quantifier? a time?
        (raise-arguments-error 'expr-check "not implemented for (e1 ')" "expr" this-syntax)]
       [("[" _:$ExprList "]")
        ;; not implemented in forge expander
        (void)]
       [(nm:$Name "[" ee:$ExprList "]")
        (define name-ty (name-check #'nm.name))
        (define expr-ty* (map expr-check #'(ee.exprs ...)))
        (app-check name-ty expr-ty*)]
       [(ex1:$Expr "[" ee:$ExprList "]")
        (define e1-ty (expr-check #'ex1))
        (define expr-ty* (map expr-check (syntax-e #'(ee.exprs ...))))
        (app-check e1-ty expr-ty*)]
       [(cn:$Const)
        (consttype #'cn.translate)]
       [(qn:$QualName)
        (nametype #'qn.name)]
       [("this")
        (car (syntax-e this-syntax))]
       [("`" nm:$Name)
        (nametype #'nm.name)]
       [("{" decls:$DeclList bob:$BlockOrBar "}")
        (define decl-tys (decllist-check #'decls))
        (parameterize ((current-type-env (env-extend (current-type-env) decl-tys)))
          (blockorbar-check #'bob))]
       [(block:$Block)
        (block-check #'block)]
       [(sexpr:$Sexpr)
        (sexpr-check #'sexpr)]
       [_
         ;; ??
        (raise-syntax-error 'expr-check "internal error parsing expr" stx)]))]
  [_
    (raise-argument-error 'expr-check "$Expr" this-syntax)])

(define (ifelse-check e1-ty e2-ty e3-ty)
  ;; TODO
  (void))

(define (binop-check op e1-ty e2-ty ctx #:negate? [negate? #f])
  ;; TODO
  ;; negate? may not matter for typing
  (syntax-parse op
   [ao:$ArrowOp
    (raise-syntax-error who
                        (format "Operator ~a is not allowed in froglet" (syntax-e #'ao.arr))
                        #'ao.arr)]
   [(~or "-" "&")
    (raise-syntax-error who
                        (format "Operator ~a is not allowed in froglet" (syntax-e op))
                        op)]
   [_
    (raise-arguments-error 'binop-check "die" "op" op "e1t" e1-ty "e2t" e2-ty)]))

(define (unop-check op e1-ty)
  ;; TODO
  (void))

(define (app-check fn-ty arg-ty*)
  ;; TODO
  (void))

(define-parser letdecllist-check
  [decls:$LetDeclList
   (decl*-check (syntax-e #'decls.translate) 'one)])

(define-parser decllist-check
  [decls:$DeclList
   (decl*-check (syntax-e #'decls.translate) (current-quant))])

(define-parser decl-check
  [decl:$Decl
   (decl*-check (syntax-e #'decl.translate) (current-quant))])

(define (decl*-check name+expr* mult)
  (for/list ([ne-stx (in-list name+expr*)])
    (define ne (syntax-e ne-stx))
    (define name (car ne))
    (define expr (cadr ne))
    (define expr-ty (expr-check expr))
    (fieldtype name expr-ty mult)))

(define-parser blockorbar-check
  [bob:$BlockOrBar
   (syntax-parse #'bob.exprs
    [bb:$Block
      (block-check #'bb)]
    [ee:$Expr
      (expr-check #'ee)])])

(define (field-check lhs rhs)
  (define lhs-sigty
    (or (->sigty lhs)
        (raise-syntax-error who
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
    [(nametype? stx)
     (name->sig (type-name stx))]
    [else
     (name->sig stx)]))

(define (name-check stx)
  (name-lookup stx))

(define (name->sig stx)
  (name-lookup stx sigtype?))

(define (name->pred stx)
  (name-lookup stx predtype?))

(define (name-lookup stx [-env-guard #f])
  (define env-guard (or -env-guard values))
  (define stx=?
    (if (syntax? stx)
      (lambda (that) (free-identifier=? stx that))
      (lambda (that) (eq? stx (syntax-e that)))))
  (for/first ([elem (in-list (current-type-env))]
              #:when (and (env-guard elem)
                          (stx=? (type-name elem))))
    elem))

(define (field->sigty stx)
  (for/first ([elem (in-list (current-type-env))]
              #:when (and (sigtype? elem)
                          (sigtype-has-field? elem stx)))
    elem))

(define (sigtype-has-field? sigty idty)
  (define id (type-name idty))
  (for*/or ([ft (in-list (sigtype-field* sigty))]
            [nm (in-list (type-name ft))])
    (free-identifier=? id nm)))

(define (sigtype-find-field sigty idty)
  (define id (type-name idty))
  (or
    (for/or ([ft (in-list (sigtype-field* sigty))])
      (for/or ([nm (in-list (type-name ft))]
               [ty (in-list (fieldtype-sig ft))]
               #:when (free-identifier=? id nm))
        (->sigty ty)))
    (raise-user-error 'sigtype-find-field "Field not found")))

(define (env-init)
  (list (sigtype (datum->syntax #f 'Int) #f #f '())))

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
    ;; TODO ... implement this, fix parsing first
    (raise-user-error 'unknown-sig-check/pred "not implemented~n ~a" pp)))

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

(define (env-collect stx)
  (syntax-parse stx
    #:datum-literals (AlloyModule ModuleDecl Import)
    ;; [ev:EvalDecl ...]
    [mod:$AlloyModule
     #:fail-unless (null? (syntax-e #'(mod.import* ...)))
                   "Cannot typecheck module with Import statements"
     ; (printf "Module-decl ~a~n~n" #'(~? mod.moduledecl "None present"))
     ; (printf "Paragraphs: ~a~n~n" #'(mod.parag* ...))
     ;; TODO (printf "Exprs: ~a~n~n" #'(mod.expr* ...))
     (append-map env-collect/paragraph (syntax-e #'(mod.parag* ...)))]))

(define (env-collect/paragraph stx)
  (syntax-parse stx
   [sig:$SigDecl
     (define mult (syntax-e #'(~? sig.mult #f)))
     (define extends (syntax-e #'(~? sig.extends #f)))
     (define field* (parse-arrow-decl* (syntax-e #'(sig.relation-decl* ...))))
     (for/list ([name (in-list (syntax-e #'(sig.name* ...)))])
       (sigtype name mult extends field*))]
   [pred:$PredDecl
     ;; TODO fix ... parsing all wrong, bad decls
     (list (predtype #'pred.name (syntax-e #'(~? pred.decls ()))))]
   [fun:$FunDecl
     (raise-arguments-error 'env-collect/paragraph "not implemented" "stx" stx)]
   [assert:$AssertDecl
     (raise-arguments-error 'env-collect/paragraph "not implemented" "stx" stx)]
   [cmd:$CmdDecl
     (raise-arguments-error 'env-collect/paragraph "not implemented" "stx" stx)]
   [testexpect:$TestExpectDecl
     '()]
   [sexpr:$SexprDecl
     (raise-arguments-error 'env-collect/paragraph "not implemented" "stx" stx)]
   [query:$QueryDecl
     (raise-arguments-error 'env-collect/paragraph "not implemented" "stx" stx)]
   [option:$OptionDecl
     (raise-arguments-error 'env-collect/paragraph "not implemented" "stx" stx)]
   [inst:$InstDecl
     (raise-arguments-error 'env-collect/paragraph "not implemented" "stx" stx)]
   [example:$ExampleDecl
     (raise-arguments-error 'env-collect/paragraph "not implemented" "stx" stx)]
   [_
     (raise-argument-error 'env-collect/paragraph "Paragraph?" stx)]))

(define (parse-arrow-decl* decl*)
  (map parse-arrow-decl decl*))

(define (parse-arrow-decl decl)
  (syntax-parse decl
   [ad:$ArrowDecl
    (define name (syntax-e #'(ad.name* ...)))
    (define sig (syntax-e #'(ad.type* ...)))
    (define mult (syntax-e #'ad.mult))
    (fieldtype name mult sig)]))

