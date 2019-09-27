#lang br/quicklang

(require racket/pretty "../../forge/lang/ast.rkt" "../../forge/sigs.rkt")

(provide (except-out (all-defined-out) forge2-module-begin)
         (rename-out [forge2-module-begin #%module-begin]))

;;;;;;;;

(define-macro (forge2-module-begin MODULE)
  #'(#%module-begin 
    ; (pretty-print 'MODULE)
    ; (displayln "")
    (pretty-print MODULE)
    ; MODULE
  ))

;;;;;;;;

;; TODO: should I be using macros?

;; note: many of these are implemented by processing arguments while ignoring order
;;       this is mostly just for clarity, and lets us ignore syntax details
(define Number string->number)  
(define AlloyModule append)         ;; append list of lists
(define (ModuleDecl . args) '())    ;; remove
(define (Import . args) '())        ;; remove
(define (SigDecl . args)
  (define-values (abstract one names qualName decls exprs) (values #f #f '() #f '() '()))
  (for ([arg args])
    (match arg
      ["abstract" (set! abstract #t)]
      [(list 'Mult "one") (set! one #t)]
      [(cons 'NameList ns) (set! names (map string->symbol ns))]
      [(list 'SigExt "extends" (list 'QualName qn)) (set! qualName (string->symbol qn))]
      [(cons 'DeclList ds) (set! decls ds)]
      [(cons 'Block es) (set! exprs es)]
      [_ #f]
    )
  )

  (define op (if one 'declare-sig-one 'declare-sig))
  (define ex (if qualName `(#:extends ,qualName) '()))

  (map (lambda (name) `(,op ,name ,@ex)) names)
)
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
(define (Expr . args)
  (match args
    [(list a) a]
    [(list "#" a) `(card ,a)]
    [(list (? string? op) a) `(,(string->symbol op) ,a)]
    [(list a "." b) `(join ,a ,b)]
    [(list a (? string? op) b) `(,(string->symbol op) ,a ,b)]
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



;;;;;;;;

;; placeholders
; (define (AlloyModule . args) (cons 'AlloyModule args))
; (define (ModuleDecl . args) (cons 'ModuleDecl args))
; (define (Import . args) (cons 'Import args))
; (define (SigDecl . args) (cons 'SigDecl args))
(define (SigExt . args) (cons 'SigExt args))
(define (Mult . args) (cons 'Mult args))
(define (Decl . args) (cons 'Decl args))
(define (FactDecl . args) (cons 'FactDecl args))
; (define (PredDecl . args) (cons 'PredDecl args))
(define (FunDecl . args) (cons 'FunDecl args))
(define (ParaDecls . args) (cons 'ParaDecls args))
(define (AssertDecl . args) (cons 'AssertDecl args))
; (define (CmdDecl . args) (cons 'CmdDecl args))
(define (Scope . args) (cons 'Scope args))
(define (Typescope . args) (cons 'Typescope args))
; (define (Const . args) (cons 'Const args))
(define (UnOp . args) (cons 'UnOp args))
(define (BinOp . args) (cons 'BinOp args))
(define (ArrowOp . args) (cons 'ArrowOp args))
(define (CompareOp . args) (cons 'CompareOp args))
(define (LetDecl . args) (cons 'LetDecl args))
(define (Block . args) (cons 'Block args))
(define (BlockOrBar . args) (cons 'BlockOrBar args))
(define (Quant . args) (cons 'Quant args))
; (define (QualName . args) (cons 'QualName args))
(define (Name . args) (cons 'Name args))
; (define (Number . args) (cons 'Number args))
(define (NameList . args) (cons 'NameList args))
(define (QualNameList . args) (cons 'QualNameList args))
(define (DeclList . args) (cons 'DeclList args))
(define (LetDeclList . args) (cons 'LetDeclList args))
(define (TypescopeList . args) (cons 'TypescopeList args))
(define (ExprList . args) (cons 'ExprList args))
(define (Sexpr . args) (cons 'Sexpr args))


; (define (Expr   . args) (cons 'Expr   args))
(define Expr1  Expr)
(define Expr2  Expr)
(define Expr3  Expr)
(define Expr4  Expr)
(define Expr5  Expr)
(define Expr6  Expr)
(define Expr65 Expr)
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
