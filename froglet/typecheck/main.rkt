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

(define-parser expr-check
  [exp:$Expr
    ;; (Expr8 (Expr15 (Expr15 (QualName Tim))
    ;;                . (Expr16 (QualName father)))
    ;;        . (Expr16 (QualName grad)))
    ;; TODO syntax class for those muliplicity ops
    ;; TODO ... copy things over from expr macro
    (let loop ([stx (syntax/loc this-syntax (exp.body ...))])
      (syntax-parse stx
       [(_:$QuantStr exp:$Expr)
        (loop (syntax/loc this-syntax (exp.body ...)))]
       [(lhs:$Expr (~datum ".") rhs:$Expr)
        (define lhs-ty (loop #'(lhs.body ...)))
        (define rhs-ty (loop #'(rhs.body ...)))
        (or (field-check lhs-ty rhs-ty)
            (raise-syntax-error 'froglet:typecheck
                                "Expected matching sig and field"
                                this-syntax))]
       [(qn:$QualName)
        #'qn.name]
       [_
        (raise-user-error 'dieee "~a" (syntax->datum #'exp))]))]
  [_
    ;; could be #'(raise ....)
    (void)])

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

