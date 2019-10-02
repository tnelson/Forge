#lang br/quicklang

(require racket/pretty)
(require "../../forge/lang/ast.rkt" "../../forge/sigs.rkt")

(provide (except-out (all-defined-out) forge2-module-begin)
         (rename-out [forge2-module-begin #%module-begin]))

;;;;;;;;

(define-macro (forge2-module-begin MODULE)
  #'(#%module-begin MODULE)
  )

(define-syntax (AlloyModule stx) (datum->syntax #'0 `(begin ,@(cdr (syntax->datum stx)))))
(define-syntax (ModuleDecl stx) #'(begin))
(define-syntax (SexprDecl stx) 
  (define datum (list 'begin (cadr (syntax->datum stx))))
  (println datum)
  (datum->syntax #'0 datum))
(define-syntax (SigDecl stx) 
  (define args (cdr (syntax->datum stx)))
  (define-values (abstract one names qualName decls exprs) (values #f #f '() #f '() '()))
  (for ([arg args])
    (syntax-case arg (NameList Mult SigExt DeclList Block)
      ["abstract" (set! abstract #t)]
      [(Mult "one") (set! one #t)]
      [(NameList ns ...) (set! names #'(ns ...))]
      [(SigExt "extends" qn) (set! qualName #'qn)]
      [(DeclList ds ...) (set! decls #'(ds ...))]
      [(Block es ...) (set! exprs #'(es ...))]
      [_ #f]
    )
  )
  (set! names (map string->symbol (syntax->datum names)))
  (if qualName (set! qualName (string->symbol (cadr (syntax->datum qualName)))) #f)

  (define op (if one 'declare-one-sig 'declare-sig))
  (define ex (if qualName `(#:extends ,qualName) '()))
  
  (define datum (cons 'begin (map (lambda (name) `(,op ,name ,@ex)) names)))
  (println datum)
  (datum->syntax #'0 datum)
)
(define-syntax (Sexpr stx) 
  (define s (cadr (syntax->datum stx)))
  (define datum (read (open-input-string s)))
  (datum->syntax #'0 datum)
  )




; (define-syntax (declare-sig stx) #'"<declare-sig>")
;   ; (define args (cdr (syntax->datum stx)))
;   ; (println `(list 'declare-sig ,args))
;   ; #'123)
;   ; (datum->syntax #'0 `(list 'declare-sig ,args)))


;;;;;;;;

;; note: many of these are implemented by processing arguments while ignoring order
;;       this is mostly just for clarity, and lets us ignore syntax details
(define Number string->number)  
(define (Import . args) '())
(define (PredDecl . args) 
  (define-values (name paras block) (values #f '() #f))
  (for ([arg args])
    (match arg
      [(list 'Name n) (set! name (string->symbol n))]
      [(list 'ParaDecls (list 'Decl (cons 'NameList ps) _ ...) ...) 
       (set! paras (map string->symbol (flatten ps)))]
      [(cons 'Block b) (set! block b)]
      [_ #f]
    )
  )
  (list `(pred (,name ,@paras) ,@block))
)
(define (FunDecl . args) 
  (define-values (name paras expr1 expr2) (values #f '() #f #f))
  (for ([arg args])
    (match arg
      [(list 'Name n) (set! name (string->symbol n))]
      [(list 'ParaDecls (list 'Decl (cons 'NameList ps) _ ...) ...) 
       (set! paras (map string->symbol (flatten ps)))]
      ; [(cons 'Block b) (set! block b)]
      [e (if expr1 (set! expr2 e) (set! expr1 e))]
      ; [_ #f]
    )
  )
  (list `(fun (,name ,@paras) ,expr2))
)
(define (Expr . args)
  (match args
    [(list a) a]
    [(list "#" a) `(card ,a)]
    [(list (? string? op) a) `(,(string->symbol op) ,a)]
    [(list a "." b) `(join ,a ,b)]
    [(list a "implies" b) `(=> ,a ,b)]
    [(list a (list 'ArrowOp _) b) `(-> ,a ,b)]
    [(list a (? string? op) b) `(,(string->symbol op) ,a ,b)]
    [(list a "[" (list 'ExprList bs ...) "]") 
     (foldl (lambda (b acc) (list 'join b acc)) a bs)]
    [(list (list 'Quant q) 
           (list 'DeclList (list 'Decl (list 'NameList ns ...) ts) ...)
           body)
     (define bounds 
      (apply append (map (lambda (ns t) (map (lambda (n) (list (string->symbol n) t)) ns)) 
                         ns 
                         ts)))
     `(,(string->symbol q) ,bounds ,@body)]
    [_ (cons 'Expr args)]
  )
)
(define QualName string->symbol)
(define Const identity)
(define (CmdDecl . args) 
  (define-values (name cmd arg scope block) (values #f #f #f #f #f))
  (for ([arg args])
    (match arg
      [(list 'Name n) (set! name n)]
      ["run"   (set! cmd 'run)]
      ["check" (set! cmd 'check)]
      [(? symbol? s) (set! arg (string->symbol s))]
      [(list 'Scope scopes ...) 
      (define (f x)
        (match x
         [(list 'Typescope "exactly" n things) (list things n n)]
         [(list 'Typescope n things) (list things 0 n)]))
      (set! scope (map f scopes))]
      [(list 'Block bs ...) (set! block bs)]
      [_ #f]
    )
  )
  (if name #f (raise "please name your commands"))
  (list `(,cmd ,name (,@block) ,scope))
)
(define (AssertDecl . args) '())
(define (BlockOrBar . args)
  (match args
    [(list "|" e) (list e)]
    [(list (list 'Block es)) es]
  )
)


;;;;;;;;

;; hack to do datatypes because I don't know racket
(define (SigExt . args) (cons 'SigExt args))
(define (Mult . args) (cons 'Mult args))
(define (Decl . args) (cons 'Decl args))
(define (FactDecl . args) (cons 'FactDecl args))
(define (ParaDecls . args) (cons 'ParaDecls args))
(define (Scope . args) (cons 'Scope args))
(define (Typescope . args) (cons 'Typescope args))
(define (UnOp . args) (cons 'UnOp args))
(define (BinOp . args) (cons 'BinOp args))
(define (ArrowOp . args) (cons 'ArrowOp args))
(define (CompareOp . args) (cons 'CompareOp args))
(define (LetDecl . args) (cons 'LetDecl args))
(define (Block . args) (cons 'Block args))
; (define (BlockOrBar . args) (cons 'BlockOrBar args))
(define (Quant . args) (cons 'Quant args))
(define (Name . args) (cons 'Name args))
(define (NameList . args) (cons 'NameList args))
(define (QualNameList . args) (cons 'QualNameList args))
(define (DeclList . args) (cons 'DeclList args))
(define (LetDeclList . args) (cons 'LetDeclList args))
(define (TypescopeList . args) (cons 'TypescopeList args))
(define (ExprList . args) (cons 'ExprList args))


(define Expr1  Expr)
(define Expr2  Expr)
(define Expr3  Expr)
(define Expr4  Expr)
(define Expr5  Expr)
(define Expr6  Expr)
(define Expr6a Expr)
(define Expr7  Expr)
(define Expr8  Expr)
(define Expr9  Expr)
(define Expr10 Expr)
(define Expr11 Expr)
(define Expr12 Expr)
(define Expr13 Expr)
(define Expr14 Expr)
(define Expr15 Expr)
(define Expr16 Expr)
