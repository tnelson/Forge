#lang racket/base

;; TODO can we use turnstile?
;;  collect env and then use turnstile?

(provide
  typecheck)

(require
  racket/pretty
  syntax/parse)

;; -----------------------------------------------------------------------------


(define (typecheck mod)
  (define env (collect-env mod))
  (pretty-write env)
  mod)


;; ---

(define-syntax-class alloy-import
  (pattern ((~datum Import) _ ...)))

(define-syntax-class alloy-module
  (pattern ((~datum ModuleDecl) _ ...)))

(define (collect-env mod)
  (syntax-parse mod
    #:datum-literals (AlloyModule ModuleDecl Import)
    [(AlloyModule
       (~optional module-decl:alloy-module)
       imp*:alloy-import ...
       . parag*)
     #:fail-unless (null? (syntax-e #'(imp* ...))) "Cannot typecheck module with Import statements"
     #:with (paragraph* ...)
            (quasisyntax/loc mod
              #,(map collect-env/paragraph (syntax-e (syntax/loc mod parag*))))
       ; (printf "Module-decl ~a~n~n" #'(~? module-decl "None present"))
       ; (printf "Paragraphs: ~a~n~n" #'parag*)
       (quasisyntax/loc mod
         (AlloyModule (~? module-decl)
                      imp* ...
                      paragraph* ...))]))

(define (collect-env/paragraph parag)
  (raise-user-error 'cep (format "die~n ~s" parag)))
;; TODO
;  (syntax-parse paragraph #:datum-literals (IntsDecl Bounds)
;    [(InstDecl name 
;               (Bounds (~optional (~and "exactly" exactly-tok))
;                       exprs ...)
;               (~optional scope))
;     #:with (updated-exprs ...) (replace-ints-expr* (syntax/loc paragraph (exprs ...)))
;     (quasisyntax/loc paragraph
;       (InstDecl name
;                 (Bounds (~? exactly-tok) updated-exprs ...)
;                 (~? scope)))]
;    [(_ ...) paragraph])

