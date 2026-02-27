#lang racket/base

(provide coerce-ints-to-atoms-and-preprocess
         read-syntax
         generic-forge-reader)

(require syntax/parse
         (for-syntax racket/base syntax/parse)
         racket/pretty
         (only-in racket/list partition second))
(require forge/lang/alloy-syntax/parser)
(require forge/lang/alloy-syntax/tokenizer)
(require forge/shared)

(do-time "forge/lang/reader")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This series of functions intervenes at the pre-Forge-AST level to
; insert automatic conversion between int-exprs and rel-exprs in
; *some* cases; the others are handled in the AST construction.

; It also separates the (leading!) imports out from the main module,
; so that the reader can lift (require ...) expressions.

; Regarding syntax replacement, here and in the expander,
; NOTE WELL this sentence from the Racket docs on syntax/loc: 
; "The source location is adjusted only if the resulting syntax object
; comes from the template itself rather than the value of a syntax pattern variable."
; (In all other cases, we apparently must use datum->syntax.)

;; Declaration hoisting: reorder paragraphs so users can write run/check
;; before the sigs they reference. Emitted module order:
;;   0. option problem_type  (file-level config; needed before var sigs)
;;   1. sig, pred, fun       (foundations; pred/fun bodies are thunked)
;;   2. inst                 (references sigs eagerly)
;;   3. everything else      (commands, other options — source order)
;; Other options are NOT hoisted so they can vary between runs.

(define (paragraph-tag stx)
  (define d (syntax-e stx))
  (and (pair? d) (syntax->datum (car d))))

;; Check if this is `option problem_type ...`.
;; Parse tree: (NT-OptionDecl (QualName problem_type) ...)
(define (problem-type-option? stx)
  (and (equal? (paragraph-tag stx) 'NT-OptionDecl)
       (let ([elems (syntax->list stx)])
         (and elems
              (>= (length elems) 2)
              (let ([name-stx (second elems)])
                (let ([name-elems (syntax->list name-stx)])
                  (and name-elems
                       (>= (length name-elems) 2)
                       (equal? (syntax->datum (second name-elems))
                               'problem_type))))))))

(define (sig-like-paragraph? stx)
  (member (paragraph-tag stx) '(NT-SigDecl NT-PredDecl NT-FunDecl)))

(define (inst-paragraph? stx)
  (equal? (paragraph-tag stx) 'NT-InstDecl))

(define (coerce-ints-to-atoms-and-preprocess tree)
  ; AlloyModule: ModuleDecl? Import* Paragraph*
  (syntax-parse tree #:datum-literals (NT-AlloyModule NT-ModuleDecl NT-Import)
    [(NT-AlloyModule (~optional (~and module-decl
                                      (NT-ModuleDecl _ ...)))
                     ;(~seq (~and import
                     ;            (Import _ ...)) ...)
                     (~seq (~and import (NT-Import _)) ...)
                     . parags)
     #:with (paragraphs ...)
            (quasisyntax/loc tree
              #,(replace-ints-paragraph* (syntax/loc tree parags)))
            ;(printf "Module-decl ~a~n~n" #'(~? module-decl "None present"))
            ;(printf "Imports: ~a~n~n" #'(import ...))
            ;(printf "Paragraphs: ~a~n~n" #'(paragraphs ...))

     ;; Reorder paragraphs into tiers (see declaration-hoisting comment above).
     ;; Original order within each tier is preserved.
     (define par-list (syntax->list #'(paragraphs ...)))
     (define-values (problem-type rest-after-pt) (partition problem-type-option? par-list))
     (define-values (sigs rest-after-sigs) (partition sig-like-paragraph? rest-after-pt))
     (define-values (insts rest) (partition inst-paragraph? rest-after-sigs))
     (define reordered (append problem-type sigs insts rest))

       (values
        ; The module imports
        (quasisyntax/loc tree (import ...))
        ; The module body with integers coerced, imports removed, declarations hoisted
        (quasisyntax/loc tree
         (NT-AlloyModule (~? module-decl)
                      ;import ...
                      #,@reordered)))]))

(define (replace-ints-expr expr)
  ;(printf "Replace-int-expr: ~a~n~n" expr)
  (syntax-parse expr #:datum-literals (Name QualName Const Number)
    [(_ (Const (~optional "-") (Number n)))
     (quasisyntax/loc expr
       (NT-Expr
        #,(quasisyntax/loc expr (NT-Expr
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

; Forge's reader function. Use the 'forge language, and inject nothing.
(define (read-syntax path port)
  (generic-forge-reader
   path
   port
   'forge
   forge-checker-hash
   forge-ast-checker-hash
   forge-inst-checker-hash
   '(forge/lang/lang-specific-checks)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: Changes to this macro may need re-compiling Forge before they are soon.

(define-syntax (generic-forge-reader stx)
  (syntax-parse stx
    [((~datum generic-forge-reader)
      path port LANG-NAME CH ACH ICH EXTRA-REQUIRES (~optional INJECTED #:defaults ([INJECTED #''()])))
     ; This is the Racket code for the specialized reader:
     (quasisyntax/loc stx
       (begin 
         (define compile-time (current-seconds))
         (define injected-if-any INJECTED)
         (define extra-requires EXTRA-REQUIRES)

         (define parse-tree (parse path (make-tokenizer port)))
         (define-values (imports ints-coerced) (coerce-ints-to-atoms-and-preprocess parse-tree))
         
         (define final `((provide (except-out (all-defined-out) ; So other programs can require it
                                              forge:n))         ; but don't share the namespace anchor
                         ; Most Forge stuff should be available via the module language below. But some 
                         ; Racket functions are needed too.
                         (require (only-in racket/base unless hash-empty?))
                         (require (only-in forge/shared do-time))
                         ; Module imports (via "open")
                         ,@(syntax->list imports)
                         
                         (do-time "forge-mod toplevel")
                         
                         ;; Used for the evaluator
                         (define-namespace-anchor forge:n) 
                         (forge:nsa forge:n)
                         
                         ;; Set up language-specific error messages
                         (require forge/choose-lang-specific
                                  ,@extra-requires)             
                         (set-checker-hash! CH)
                         (set-ast-checker-hash! ACH)
                         (set-inst-checker-hash! ICH)
                         (set-check-lang! LANG-NAME)                  
                         
                         ;; Add any code to inject before the model is expanded
                         ,@injected-if-any
                         ;; Expanded model, etc.
                         ,ints-coerced
                         (do-time "forge-mod ints-coerced")
                         
                         ; Declare submodule "execs". Macros like "test" or "run" etc. will add to 
                         ; this submodule. After execution of execs, print test failures (if any).
                         (module+ execs
                           ; All tests should have been added to `execs` prior to this point. 
                           (output-all-test-failures)
                           ; At this point, all commands should be defined. Open Sterling, if there
                           ; are commands to visualize. If not, don't try to open Sterling.
                           (unless (hash-empty? (forge:State-runmap forge:curr-state))
                             (start-sterling-menu forge:curr-state forge:nsa)))
                         
                         ;; Declare submodule "main"
                         (module+ main
                           ; Invoke the execs submodule
                           (require (submod ".." execs)))                                    
                         ))
         
         (define module-datum `(module forge-mod forge/lang/expander
                                 ,@final))
         
         ; For debugging purposes, convert to a datum first or pretty-format will truncate.
         ;(printf "Ints-coerced: ~a~n" (pretty-format (syntax->datum ints-coerced)))
         ;printf "Parse-tree (converted to datum:~n   ~a~n" (pretty-format (syntax->datum parse-tree)))
         
         (define result (datum->syntax #f module-datum))
         ;(printf "debug result of expansion: ~a~n" result)
         result)
       )]))
       