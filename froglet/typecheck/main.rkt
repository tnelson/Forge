#lang racket/base

;; TODO can we use turnstile?
;;  collect env and then use turnstile?

(provide
  typecheck)

(require
  forge/lang/alloy-syntax/syntax-class
  racket/pretty
  syntax/parse)

;; -----------------------------------------------------------------------------


(define (typecheck mod)
  (define env (collect-env mod))
  (pretty-write env)
  mod)


;; ---

(define (collect-env stx)
  (syntax-parse stx
    #:datum-literals (AlloyModule ModuleDecl Import)
    ;; [ev:EvalDecl ...]
    [mod:$AlloyModule
     #:fail-unless (null? (syntax-e #'mod.import*)) "Cannot typecheck module with Import statements"
     ; (printf "Module-decl ~a~n~n" #'(~? mod.moduledecl "None present"))
     ; (printf "Paragraphs: ~a~n~n" #'mod.parag*)
     (map collect-env/paragraph (syntax-e #'mod.parag*))]))


;; (SigDecl (NameList Node) (ArrowDeclList (ArrowDecl (NameList next) (ArrowMult "one") (ArrowExpr (QualName Node)))))

(define (collect-env/paragraph stx)
  (syntax-parse stx
   [sig:$SigDecl
     (printf "NAME ~s~n" #'sig.name*)
     #'sig.name*]
   [pred:$PredDecl
     (raise-arguments-error 'collect-env/paragraph "not implemented" "stx" stx)]
   [fun:$FunDecl
     (raise-arguments-error 'collect-env/paragraph "not implemented" "stx" stx)]
   [assert:$AssertDecl
     (raise-arguments-error 'collect-env/paragraph "not implemented" "stx" stx)]
   [cmd:$CmdDecl
     (raise-arguments-error 'collect-env/paragraph "not implemented" "stx" stx)]
   [testexpect:$TestExpectDecl
     (raise-arguments-error 'collect-env/paragraph "not implemented" "stx" stx)]
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

