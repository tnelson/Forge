#lang racket/base

(provide coerce-ints-to-atoms
         read-syntax)

(require syntax/parse)
(require forge/lang/alloy-syntax/parser)
(require forge/lang/alloy-syntax/tokenizer)
(require (prefix-in log: forge/logging/2023/main))
(require forge/shared)

(do-time "forge/lang/reader")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This series of functions intervenes at the pre-Forge-AST level to
; insert automatic conversion between int-exprs and rel-exprs in
; *some* cases; the others are handled in the AST construction.

; Regarding syntax replacement, here and in the expander,
; NOTE WELL this sentence from the Racket docs on syntax/loc: 
; "The source location is adjusted only if the resulting syntax object
; comes from the template itself rather than the value of a syntax pattern variable."
; (In all other cases, we apparently must use datum->syntax.)

(define (coerce-ints-to-atoms tree)
  ; AlloyModule: ModuleDecl? Import* Paragraph*
  (syntax-parse tree #:datum-literals (AlloyModule ModuleDecl Import)
    [(AlloyModule (~optional (~and module-decl
                                   (ModuleDecl _ ...)))
                  (~seq (~and import
                              (Import _ ...)) ...)
                  . parags)
     #:with (paragraphs ...)
            (quasisyntax/loc tree
              #,(replace-ints-paragraph* (syntax/loc tree parags)))
       ; (printf "Module-decl ~a~n~n" #'(~? module-decl "None present"))
       ; (printf "Paragraphs: ~a~n~n" #'(paragraphs ...))
       (quasisyntax/loc tree
         (AlloyModule (~? module-decl)
                      import ...
                      paragraphs ...))]))

(define (replace-ints-expr expr)
  ;(printf "Replace-int-expr: ~a~n~n" expr)
  (syntax-parse expr #:datum-literals (Name QualName Const Number)
    [(_ (Const (~optional "-") (Number n)))
     (quasisyntax/loc expr
       (Expr
        #,(quasisyntax/loc expr (Expr
                                 #,(quasisyntax/loc expr (QualName sing)))) "["
        #,(quasisyntax/loc expr (ExprList #,expr)) "]"))]
    [(_ (~or (Name (~literal sing))
             (_ (QualName (~literal sing)))) "[" _ "]")     
     expr]
    [(_ expr1 (~optional neg-tok) (CompareOp "<=") expr2)
     expr]
    [(parts ...)
     (replace-ints-expr* expr)]
    [_ expr]))

(define (replace-ints-paragraph paragraph)
  ;(printf "Replace-ints-paragraph ~a~n~n" paragraph)
  ; InstDecl : /INST-TOK Name Bounds Scope?  
  (syntax-parse paragraph #:datum-literals (IntsDecl Bounds)
    [(InstDecl name 
               (Bounds (~optional (~and "exactly" exactly-tok))
                       exprs ...)
               (~optional scope))
     #:with (updated-exprs ...) (replace-ints-expr* (syntax/loc paragraph (exprs ...)))
     (quasisyntax/loc paragraph
       (InstDecl name
                 #,(quasisyntax/loc paragraph (Bounds (~? exactly-tok) updated-exprs ...))
                 (~? scope)))]
    [(_ ...) paragraph]))

(define (replace/list f stxs)
  ; Using quasisyntax/loc like this will NOT properly replace the syntax-location.
  ;(quasisyntax/loc stxs
  ;  #,(map f (syntax-e stxs))))
  ; Instead, need to reconstruct the syntax object via datum->syntax
  (datum->syntax stxs
                 (map f (syntax-e stxs))
                 stxs))

(define (replace-ints-expr* exprs)
  (replace/list replace-ints-expr exprs))

(define (replace-ints-paragraph* parags)
  (replace/list replace-ints-paragraph parags))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (require racket/pretty)

; Forge's reader function
(define (read-syntax path port)
  (define this-lang 'forge)
  (define-values (logging-on? project email) (log:setup this-lang port path))
  (define compile-time (current-seconds))
  (when logging-on?
    (uncaught-exception-handler (log:error-handler logging-on? compile-time (uncaught-exception-handler)))
    (log:register-run compile-time project this-lang email path))
  (define parse-tree (parse path (make-tokenizer port)))
  (define ints-coerced (coerce-ints-to-atoms parse-tree))

  ;(printf "ints-coerced/parse-tree: ~a~n" ints-coerced)
  
  (define final `((provide (except-out (all-defined-out) ; So other programs can require it
                                       forge:n))
                  (require (only-in forge/shared do-time))
                  (do-time "forge-mod toplevel")

                  ;; Used for the evaluator
                  (define-namespace-anchor forge:n) 
                  (forge:nsa forge:n)

                  (require (prefix-in log: forge/logging/2023/main))
                  (require (only-in racket printf uncaught-exception-handler))

                  ;; Set up language-specific error messages
                  (require forge/choose-lang-specific)
                  (require forge/lang/lang-specific-checks)              
                  (set-checker-hash! forge-checker-hash)
                  (set-ast-checker-hash! forge-ast-checker-hash)
                  (set-inst-checker-hash! forge-inst-checker-hash)
                  (set-check-lang! 'forge)                  

                  ;; Override default exception handler
                  (uncaught-exception-handler (log:error-handler ',logging-on? ',compile-time (uncaught-exception-handler)))

                  ;; Expanded model, etc.
                  ,ints-coerced
                  (do-time "forge-mod ints-coerced")

                  ; Declare submodule "execs". Macros like "test" or "run" etc. will add to 
                  ; this submodule. After execution of execs, print test failures (if any).
                  (module+ execs
                    ; All tests should have been added to `execs` prior to this point. 
                    (output-all-test-failures)
                    ; At this point, all commands should be defined. Open Sterling.
                    (start-sterling-menu forge:curr-state forge:nsa))

                  ;; Declare submodule "main"
                  (module+ main
                    ; Invoke the execs submodule
                    (require (submod ".." execs)))                                    
                  
                  (log:flush-logs ',compile-time "no-error")))

  (define module-datum `(module forge-mod forge/lang/expander
                          ,@final))
  ; For debugging purposes, convert to a datum first or pretty-format will truncate.
  ;(printf "Ints-coerced: ~a~n" (pretty-format (syntax->datum ints-coerced)))

  ; (raise "STOP")
  (define result (datum->syntax #f module-datum))
  ;(printf "debug result of expansion: ~a~n" result)
  result)

