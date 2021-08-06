#lang racket/base

(require syntax/parse)
(require forge/lang/alloy-syntax/parser)
(require forge/lang/alloy-syntax/tokenizer)
(require (prefix-in logging: forge/logging/logging))

(provide coerce-ints-to-atoms)
(define (coerce-ints-to-atoms tree)
  (define (replace-ints-expr expr)
    ; (printf "Replace-int-expr: ~a~n~n" expr)
    (syntax-parse expr #:datum-literals (Name QualName Const Number)
      [(_ (Const (~optional "-")
                 (Number n)))
       #`(Expr (Expr (QualName sing)) "[" (ExprList #,expr) "]")]
      [(_ (~or (Name (~literal sing))
               (_ (QualName (~literal sing)))) "[" _ "]")
       expr]
      [(_ expr1 (~optional neg-tok) (CompareOp "<=") expr2)
       expr]
      [(parts ...)
       (datum->syntax expr
                      (map replace-ints-expr (syntax->list #'(parts ...))))]
      [_ expr]))

  (define (replace-ints-paragraph paragraph)
    ; (printf "Replace-ints-paragraph ~a~n~n" paragraph)
    ; InstDecl : /INST-TOK Name Bounds Scope?
    (syntax-parse paragraph #:datum-literals (IntsDecl Bounds)
      [(InstDecl name 
                 (Bounds (~optional (~and "exactly" exactly-tok))
                         exprs ...)
                 (~optional scope))
       (with-syntax ([(updated-exprs ...)
                      (datum->syntax #'(exprs ...) 
                                     (map replace-ints-expr 
                                          (syntax->list #'(exprs ...))))])
         #'(InstDecl name 
                     (Bounds (~? exactly-tok) updated-exprs ...)
                     (~? scope)))]
      [(_ ...) paragraph]))

  ; AlloyModule: ModuleDecl? Import* Paragraph*
  (syntax-parse tree #:datum-literals (AlloyModule ModuleDecl Import)
    [(AlloyModule (~optional (~and module-decl
                                   (ModuleDecl _ ...)))
                  (~seq (~and import
                              (Import _ ...)) ...)
                  (~seq paragraphs ...))
     (with-syntax ([(paragraphs ...)
                    (datum->syntax #'(paragraphs ...) 
                                   (map replace-ints-paragraph
                                        (syntax->list #'(paragraphs ...))))])
       ; (printf "Module-decl ~a~n~n" #'(~? module-decl "None present"))
       ; (printf "Paragraphs: ~a~n~n" #'(paragraphs ...))
       #'(AlloyModule (~? module-decl)
                      import ...
                      paragraphs ...))]))

(define (read-syntax path port)
  (define-values (logging-on? project email) (logging:log-execution 'forge port path))
  (define parse-tree (parse path (make-tokenizer port)))
  (define ints-coerced (coerce-ints-to-atoms parse-tree))
  (define final `((provide (except-out (all-defined-out) ; So other programs can require it
                                       forge:n))

                  (define-namespace-anchor forge:n) ; Used for evaluator
                  (forge:nsa forge:n)

                  (require (only-in forge/logging/logging 
                                    [flush-logs logging:flush-logs]
                                    [log-errors logging:log-errors]))
                  (require (only-in racket printf))

                  (require forge/choose-lang-specific)
                  (require forge/bsl/lang/bsl-lang-specific-checks) ; TODO: can this be relative?
                  ; ANSWER: maybe using dynamic-require
                  (set-checker-hash! bsl-checker-hash)
                  
                  ;(printf "ch = ~a~n" bsl-checker-hash) 
                  ;(printf "ch = ~a~n" (get-checker-hash))

                  (logging:log-errors
                    ,ints-coerced)
                  
                  (module+ execs)
                  (module+ main
                    (require (submod ".." execs))
                    (logging:flush-logs))))

  (define module-datum `(module forge-mod forge/lang/expander
                          ,@final))
  ; (printf "Ints-coerced: ~a~n" ints-coerced)
  ; (raise "STOP")
  (define result (datum->syntax #f module-datum))
  ;(printf "debug result of expansion: ~a~n" result)
  result)
(provide read-syntax)