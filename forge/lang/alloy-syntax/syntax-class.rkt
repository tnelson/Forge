#lang racket/base

(provide
  $AlloyModule
  $Import
  $ModuleDecl
  $EvalDecl

  $SigDecl
  $PredDecl
  $FunDecl
  $AssertDecl
  $CmdDecl
  $TestExpectDecl
  $SexprDecl
  $QueryDecl
  $EvalRelDecl
  $OptionDecl
  $InstDecl
  $ExampleDecl

  $Bounds
  $QualName
  )

(require
  syntax/parse)

;; ---

(define-syntax-class $AlloyModule
  #:attributes (nm moduledecl import* parag*)
  #:commit
  ;; TODO could be an EvalDecl too!
  (pattern ((~and nm (~datum AlloyModule))
            (~optional moduledecl:$AlloyModule)
            pre-import*:$Import ...
            . parag*)
    #:with import* (syntax/loc this-syntax (pre-import* ...))))

(define-syntax-class $Import
  #:attributes (nm )
  #:commit
  (pattern ((~and nm (~datum Import))
            _ ...)))

(define-syntax-class $ModuleDecl
  #:attributes (nm )
  #:commit
  (pattern ((~and nm (~datum ModuleDecl))
            _ ...)))


(define-syntax-class $EvalDecl
  #:attributes (nm )
  #:commit
  (pattern ((~and nm (~datum EvalDecl))
            _ ...)))

(define-syntax-class $SigDecl
  ;; TODO cleanup
  #:attributes (nm isv abstract mult name* extends relation-decls block)
  #:commit
  (pattern ((~and nm (~datum SigDecl))
            (~optional isv #;:VarKeywordClass #:defaults ([isv #'#f]))
            (~optional abstract #;:abstract-tok)
            (~optional mult #;:MultClass)
            name* #;:NameListClass
            ;when extending with in is implemented,
            ;if "sig A in B extends C" is allowed,
            ;check if this allows multiple SigExtClasses / how to do that if not
            ;note the parser currently does not allow that
            (~optional extends #;:SigExtClass)
            (~optional relation-decls #;:ArrowDeclListClass)
            (~optional block #;:BlockClass))))

;; SigDecl : VAR-TOK? ABSTRACT-TOK? Mult? /SIG-TOK NameList SigExt? /LEFT-CURLY-TOK ArrowDeclList? /RIGHT-CURLY-TOK Block?

(define-syntax-class $PredDecl
  #:attributes (nm )
  #:commit
  (pattern ((~and nm (~datum PredDecl))
            _ ...)))

(define-syntax-class $FunDecl
  #:attributes (nm )
  #:commit
  (pattern ((~and nm (~datum FunDecl))
            _ ...)))

(define-syntax-class $AssertDecl
  #:attributes (nm )
  #:commit
  (pattern ((~and nm (~datum AssertDecl))
            _ ...)))

(define-syntax-class $CmdDecl
  #:attributes (nm )
  #:commit
  (pattern ((~and nm (~datum CmdDecl))
            _ ...)))

(define-syntax-class $TestExpectDecl
  #:attributes (nm )
  #:commit
  (pattern ((~and nm (~datum TestExpectDecl))
            _ ...)))

(define-syntax-class $SexprDecl
  #:attributes (nm )
  #:commit
  (pattern ((~and nm (~datum SexprDecl))
            _ ...)))

(define-syntax-class $QueryDecl
  #:attributes (nm )
  #:commit
  (pattern ((~and nm (~datum QueryDecl))
            _ ...)))

(define-syntax-class $EvalRelDecl
  #:attributes (nm )
  #:commit
  (pattern ((~and nm (~datum EvalRelDecl))
            _ ...)))

(define-syntax-class $OptionDecl
  #:attributes (nm )
  #:commit
  (pattern ((~and nm (~datum OptionDecl))
            _ ...)))

(define-syntax-class $InstDecl
  #:attributes (nm bounds scope)
  #:commit
  (pattern ((~and nm (~datum InstDecl))
            bounds:$Bounds
            (~optional scope))))

(define-syntax-class $ExampleDecl
  #:attributes (nm )
  #:commit
  (pattern ((~and nm (~datum ExampleDecl))
            _ ...)))

(define-syntax-class $Bounds
; Bounds : EXACTLY-TOK? @ExprList
;        | EXACTLY-TOK? @Block
  #:attributes ()
  #:commit
  (pattern ((~optional (~datum exactly))
            n:number
            qualname:$QualName)))

(define-syntax-class $QualName
  #:attributes (nm )
  #:commit
  (pattern ((~and nm (~datum QualName))
            _ ...)))




