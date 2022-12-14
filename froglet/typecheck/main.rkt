#lang racket/base

;; TODO can we use turnstile?
;;  collect env and then use turnstile?

(provide
  typecheck)

(require
  forge/lang/alloy-syntax/syntax-class
  froglet/typecheck/struct
  racket/list
  racket/pretty
  syntax/id-set
  syntax/parse)

;; -----------------------------------------------------------------------------

(define (typecheck mod)
  (define env (collect-env mod))
  (pretty-write env)
  (env-check env)
  ;; TODO assert valid env
  ;; TODO bounds
  ;; TODO ... collect exprs everywhere, traverse
  mod)

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
     #:fail-unless (null? (syntax-e #'mod.import*))
                   "Cannot typecheck module with Import statements"
     ; (printf "Module-decl ~a~n~n" #'(~? mod.moduledecl "None present"))
     ; (printf "Paragraphs: ~a~n~n" #'mod.parag*)
     ;; TODO (printf "Exprs: ~a~n~n" #'mod.expr*)
     (append-map collect-env/paragraph (syntax-e #'mod.parag*))]))

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

