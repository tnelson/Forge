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

  $ParaDecls
  $Block
  $Expr
  $Bounds
  $QualName
  $Name

  )

(require
  syntax/parse)

;; ---

(define-syntax-class $AlloyModule
  #:attributes (hd moduledecl import* parag*)
  #:commit
  ;; TODO could be an EvalDecl too!
  (pattern ((~and hd (~datum AlloyModule))
            (~optional moduledecl:$AlloyModule)
            pre-import*:$Import ...
            . parag*)
    #:with import* (syntax/loc this-syntax (pre-import* ...))))

(define-syntax-class $Import
  #:attributes (hd )
  #:commit
  (pattern ((~and hd (~datum Import))
            _ ...)))

(define-syntax-class $ModuleDecl
  #:attributes (hd )
  #:commit
  (pattern ((~and hd (~datum ModuleDecl))
            _ ...)))


(define-syntax-class $EvalDecl
  #:attributes (hd )
  #:commit
  (pattern ((~and hd (~datum EvalDecl))
            _ ...)))

(define-syntax-class $SigDecl
  ;; TODO cleanup
  #:attributes (hd isv abstract mult name* extends relation-decls block)
  #:commit
  (pattern ((~and hd (~datum SigDecl))
            (~optional isv:$VarKeyword #:defaults ([isv #'#f]))
            (~optional abstract #;:abstract-tok)
            (~optional mult:$Mult)
            name*:$NameList
            ;when extending with in is implemented,
            ;if "sig A in B extends C" is allowed,
            ;check if this allows multiple SigExtClasses / how to do that if not
            ;note the parser currently does not allow that
            (~optional extends:$SigExt)
            (~optional relation-decls #;:ArrowDeclListClass)
            (~optional block:$Block))))
;; SigDecl : VAR-TOK? ABSTRACT-TOK? Mult? /SIG-TOK NameList SigExt? /LEFT-CURLY-TOK ArrowDeclList? /RIGHT-CURLY-TOK Block?

; SigExt : EXTENDS-TOK QualName 
;        | IN-TOK QualName (PLUS-TOK QualName)*
(define-syntax-class $SigExt
  #:attributes (hd symbol value)
  #:commit
  (pattern ((~and hd (~literal SigExt))
            "extends"
            name:$QualName)
    #:attr symbol #'#:extends
    #:attr value #'name.name)
  (pattern ((~and hd (~literal SigExt))
            "in"
            name:$QualName
            (~seq (~seq "+" names:QualNameClass) ...))
    #:attr symbol #'#:in
    #:attr value #'(raise "Extending with in not yet implemented.")))

(define-syntax-class $PredDecl
  #:attributes (hd prefix name decls block)
  #:commit
  (pattern ((~and hd (~datum PredDecl))
            (~optional (~seq prefix:$QualName "."))
            name:$Name
            (~optional decls:$ParaDecls)
            block:$Block)))

(define-syntax-class $ParaDecls
  #:attributes (hd decls)
  #:commit
  (pattern ((~and hd (~literal ParaDecls))
            (~seq decls:$Decl ...))))
;;    #:attr translate (datum->syntax #'(decls ...)
;;                                    (apply append (map (compose (curry map car )
;;                                                                (curry map syntax->list )
;;                                                                syntax->list) 
;;                                                       (syntax->list #'(decls.translate ...)))))))


(define-syntax-class $FunDecl
  #:attributes (hd )
  #:commit
  (pattern ((~and hd (~datum FunDecl))
            _ ...)))

(define-syntax-class $AssertDecl
  #:attributes (hd )
  #:commit
  (pattern ((~and hd (~datum AssertDecl))
            _ ...)))

(define-syntax-class $CmdDecl
  #:attributes (hd )
  #:commit
  (pattern ((~and hd (~datum CmdDecl))
            _ ...)))

(define-syntax-class $TestExpectDecl
  #:attributes (hd )
  #:commit
  (pattern ((~and hd (~datum TestExpectDecl))
            _ ...)))

(define-syntax-class $SexprDecl
  #:attributes (hd )
  #:commit
  (pattern ((~and hd (~datum SexprDecl))
            _ ...)))

(define-syntax-class $QueryDecl
  #:attributes (hd )
  #:commit
  (pattern ((~and hd (~datum QueryDecl))
            _ ...)))

(define-syntax-class $EvalRelDecl
  #:attributes (hd )
  #:commit
  (pattern ((~and hd (~datum EvalRelDecl))
            _ ...)))

(define-syntax-class $OptionDecl
  #:attributes (hd )
  #:commit
  (pattern ((~and hd (~datum OptionDecl))
            _ ...)))

(define-syntax-class $InstDecl
  #:attributes (hd bounds scope)
  #:commit
  (pattern ((~and hd (~datum InstDecl))
            bounds:$Bounds
            (~optional scope))))

(define-syntax-class $ExampleDecl
  #:attributes (hd )
  #:commit
  (pattern ((~and hd (~datum ExampleDecl))
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
  #:attributes (hd )
  #:commit
  (pattern ((~and hd (~datum QualName))
            _ ...)))

(define-syntax-class $Block
  #:attributes (hd exprs)
  #:commit
  (pattern ((~and hd (~datum Block))
            exprs:$Expr ...)))

(define-syntax-class $Name
  #:attributes (hd name)
  #:commit
  (pattern ((~and hd (~literal Name))
            name:id)))

; NameList : @Name
;          | @Name /COMMA-TOK @NameList
(define-syntax-class $NameList
  #:attributes (hd names)
  #:commit
  (pattern ((~and hd (~literal NameList))
            names:id ...)))

(define-syntax (define-datum-literal-set stx)
  (syntax-parse stx
   [(_ cls-name:id (lit*:id ...))
    #:with set-name (format-id stx "~a-set" (syntax-e #'cls-name))
    #'(begin-for-syntax
        (define-literal-set set-name
          #:datum-literals (lit* ...)
          ())
        (define-syntax-class cls-name
          #:literal-sets ([set-name])
          (pattern (~or lit* ...))))]))

(define-datum-literal-set ExprHd
  (Expr Expr1 Expr2 Expr3 Expr4 Expr4.5 Expr5 Expr6 Expr7 Expr7.5 Expr8 Expr9
   Expr10 Expr11 Expr12 Expr13 Expr14 Expr15 Expr16 Expr17))

(define-syntax-class ExprClass
  #:attributes (hd body)
  #:commit
  (pattern (hd:ExprHd body ...)))

(define-syntax-class $VarKeyword
  #:commit
  (pattern "var"))

; Mult : LONE-TOK | SOME-TOK | ONE-TOK | TWO-TOK
(define-syntax-class $Mult
  #:attributes (hd symbol)
  #:commit
  (pattern ((~and hd (~literal Mult))
            (~and str (~or "lone" "some" "one" "two")))
    #:attr symbol (datum->syntax #'hd (string->keyword (syntax-e #'str)))))

; ArrowMult : LONE-TOK | SET-TOK | ONE-TOK | TWO-TOK
(define-syntax-class $ArrowMult
  #:attributes (hd symbol)
  #:commit
  (pattern ((~and hd (~literal ArrowMult))
            (~or "lone" "pfunc"))
    #:attr symbol #'pfunc)
  (pattern ((~and hd (~literal ArrowMult))
            "set")
    #:attr symbol #'default)
  (pattern ((~and hd (~literal ArrowMult))
            (~or "one" "func"))
    #:attr symbol #'func)
  (pattern ((~and hd (~literal ArrowMult))
            "two")
    #:attr symbol #'(raise "relation arity two not implemented")))

; Decl : DISJ-TOK? NameList /COLON-TOK DISJ-TOK? SET-TOK? Expr
(define-syntax-class $Decl
  #:attributes (hd names expr translate)
  #:commit
  (pattern ((~and hd (~literal Decl))
            ;(~optional "disj")
            names:$NameList
            ;(~optional "disj")
            (~optional "set")
            expr:$Expr)))
;    #:attr translate (with-syntax ([expr #'expr])
;                       #'((names.names expr) ...))))

; DeclList : Decl
;          | Decl /COMMA-TOK @DeclList
(define-syntax-class $DeclListClass
  #:attributes (hd decls)
  #:commit
  (pattern ((~and hd (~literal DeclList))
            decls:$Decl ...)))
;    #:attr translate (datum->syntax #'(decls ...) 
;                                    (apply append 
;                                           (map syntax->list 
;                                                (syntax->list #'(decls.translate ...)))))))

; ArrowDecl : DISJ-TOK? NameList /COLON-TOK DISJ-TOK? ArrowMult ArrowExpr
(define-syntax-class $ArrowDecl
  #:attributes (hd names types mult is-var)
  #:commit
  (pattern ((~and hd (~literal ArrowDecl))
            ;(~optional "disj")
            (~optional isv:$VarKeyword #:defaults ([isv #'#f])) ; electrum
            name-list:$NameList
            ;(~optional "disj") 
            mult-class:$ArrowMult
            type-list:$ArrowExpr)
    #:attr names #'(name-list.names ...)
    #:attr types #'type-list.names
    #:attr mult #'mult-class.symbol
    #:attr is-var #'isv))

; ArrowDeclList : ArrowDecl
;               | ArrowDecl /COMMA-TOK @ArrowDeclList
(define-syntax-class $ArrowDeclList
  #:attributes (hd arrow-decl)
  #:commit
  (pattern ((~and hd (~literal ArrowDeclList)
            arrow-decl:$ArrowDecl ...))))

; ArrowExpr : QualName
;           | QualName /ARROW-TOK @ArrowExpr
(define-syntax-class $ArrowExpr
  #:attributes (hd names)
  #:commit
  (pattern ((~and hd (~literal ArrowExpr))
            name-list:$QualName ...)
    #:attr names #'(name-list.name ...)))

; FactDecl : FACT-TOK Name? Block
(define-syntax-class FactDeclClass
  (pattern ((~literal FactDecl)
            "fact" 
            (~optional name:NameClass)
            block:$Block)))

; PredDecl : /PRED-TOK (QualName DOT-TOK)? Name ParaDecls? Block
(define-syntax-class PredDeclClass
  (pattern ((~literal PredDecl)
            (~optional (~seq prefix:QualNameClass "."))
            name:NameClass
            (~optional decls:ParaDeclsClass)
            block:$Block)))

; FunDecl : /FUN-TOK (QualName DOT-TOK)? Name ParaDecls? /COLON-TOK Expr Block
(define-syntax-class FunDeclClass
  (pattern ((~literal FunDecl)
            (~optional (~seq prefix:QualNameClass "."))
            name:NameClass
            (~optional decls:ParaDeclsClass)
            output:ExprClass
            body:ExprClass)))

; ParaDecls : /LEFT-PAREN-TOK @DeclList? /RIGHT-PAREN-TOK 
;           | /LEFT-SQUARE-TOK @DeclList? /RIGHT-SQUARE-TOK
; DeclList : Decl
;          | Decl /COMMA-TOK @DeclList
(define-syntax-class ParaDeclsClass
  (pattern ((~literal ParaDecls)
            (~seq decls:DeclClass ...))
    #:attr translate (datum->syntax #'(decls ...)
                                    (apply append (map (compose (curry map car )
                                                                (curry map syntax->list )
                                                                syntax->list) 
                                                       (syntax->list #'(decls.translate ...)))))))

; AssertDecl : /ASSERT-TOK Name? Block
(define-syntax-class AssertDeclClass
  (pattern ((~literal AssertDecl)
            (~optional name:NameClass)
            block:$Block)))

; CmdDecl :  (Name /COLON-TOK)? (RUN-TOK | CHECK-TOK) Parameters? (QualName | Block)? Scope? (/FOR-TOK Bounds)?
(define-syntax-class CmdDeclClass
  (pattern ((~literal CmdDecl)
            (~optional name:NameClass)
            (~or "run" "check")
            (~optional parameters:ParametersClass)
            (~optional (~or pred-name:QualNameClass
                            pred-block:$Block))
            (~optional scope:ScopeClass)
            (~optional bounds:BoundsClass))))

; TestDecl : (Name /COLON-TOK)? Parameters? (QualName | Block)? Scope? (/FOR-TOK Bounds)? /IS-TOK (SAT-TOK | UNSAT-TOK)
(define-syntax-class TestDeclClass
  (pattern ((~literal TestDecl)
            (~optional name:NameClass)
            (~optional parameters:ParametersClass)
            (~optional (~or pred-name:QualNameClass
                            pred-block:$Block))
            (~optional scope:ScopeClass)
            (~optional bounds:BoundsClass)
            (~or "sat" "unsat" "theorem"))))

; TestBlock : /LEFT-CURLY-TOK TestDecl* /RIGHT-CURLY-TOK
(define-syntax-class $TestBlock
  (pattern ((~literal TestBlock)
            test-decls:TestDeclClass ...)))

; TestExpectDecl : TEST-TOK? EXPECT-TOK Name? TestBlock
(define-syntax-class TestExpectDeclClass
  (pattern ((~literal TestExpectDecl)
            (~optional "test")
            "expect"
            (~optional name:NameClass)
            test-block:$TestBlock)))

(define-syntax-class ExampleDeclClass
  (pattern ((~literal ExampleDecl)
            (~optional name:NameClass)
            pred:ExprClass
            bounds:BoundsClass)))

; Scope : /FOR-TOK Number (/BUT-TOK @TypescopeList)? 
;       | /FOR-TOK @TypescopeList
; TypescopeList : Typescope
;               | Typescope /COMMA-TOK @TypescopeList
(define-syntax-class ScopeClass
  (pattern ((~literal Scope)
            (~optional default:NumberClass)
            (~seq typescope:TypescopeClass ...))
    #:attr translate #'(typescope.translate ...)))

; Typescope : EXACTLY-TOK? Number QualName
(define-syntax-class TypescopeClass
  (pattern ((~literal Typescope)
            (~optional (~and "exactly" exactly))
            num:NumberClass
            name:QualNameClass)
    #:attr translate (if (attribute exactly)
                         #'(name.name num.value num.value)
                         #'(name.name 0 num.value))))

; OptionDecl : /OPTION-TOK QualName (QualName | FILE-PATH-TOK | Number)
(define-syntax-class OptionDeclClass
  #:attributes (n v)
  (pattern ((~literal OptionDecl) name:QualNameClass value:QualNameClass)
           #:attr n #'name.name
           #:attr v #'value.name)
  (pattern ((~literal OptionDecl) name:QualNameClass value:str)
           #:attr n #'name.name
           #:attr v #'value)
  (pattern ((~literal OptionDecl) name:QualNameClass value:NumberClass)
           #:attr n #'name.name
           #:attr v #'value.value)
  (pattern ((~literal OptionDecl) name:QualNameClass "-" value:NumberClass)
           #:attr n #'name.name
           #:attr v (quasisyntax #,(* -1 (syntax->datum #'value.value)))))


; QualName : (THIS-TOK /SLASH-TOK)? (@Name /SLASH-TOK)* @Name | INT-TOK | SUM-TOK
(define-syntax-class QualNameClass
  #:attributes (name)
  (pattern ((~literal QualName)
            (~optional "this") ; TODO, allow more complex qualnames
            (~seq prefixes:id ...)
            raw-name:id)
    #:attr name #'raw-name)
  (pattern ((~literal QualName) "Int")
    #:attr name #'(raise "Int as qualname?"))
  (pattern ((~literal QualName) "sum")
    #:attr name #'(raise "sum as qualname?")))

; QualNameList : @QualName
;              | @QualName /COMMA-TOK @QualNameList
(define-syntax-class QualNameListClass
  (pattern ((~literal QualNameList)
            (~or (~seq (~optional "this")
                       (~seq prefixes:id ...)
                       name:id)
                 "Int"
                 "sum") ...)))

; Number : NUM-CONST-TOK
(define-syntax-class NumberClass
  (pattern ((~literal Number) n)
    #:attr value #'n))

; SexprDecl : Sexpr
(define-syntax-class SexprDeclClass
  (pattern ((~literal SexprDecl) exp:SexprClass)))

; Sexpr : SEXPR-TOK
(define-syntax-class SexprClass
  (pattern ((~literal Sexpr) exp)))

; InstDecl : /INST-TOK Name Bounds Scope?
(define-syntax-class InstDeclClass
  (pattern ((~literal InstDecl)
            name:NameClass
            bounds:BoundsClass
            (~optional scope:ScopeClass))))

; RelDecl : ArrowDecl
(define-syntax-class RelDeclClass
  (pattern ((~literal RelDecl) decl:ArrowDeclClass)))

; Parameters : /LeftAngle @QualNameList /RightAngle 
(define-syntax-class ParametersClass
  (pattern ((~literal Parameters)
            name:QualNameClass ...)))

; Bounds : EXACTLY-TOK? @ExprList
;        | EXACTLY-TOK? @Block
(define-syntax-class BoundsClass
  (pattern ((~literal Bounds)
            (~optional "exactly")
            exprs:ExprClass ...)
    #:attr translate (datum->syntax #'(exprs ...)
                                    (syntax->list #'(exprs ...)))))

; EXPRESSIONS

; Const : NONE-TOK | UNIV-TOK | IDEN-TOK
;       | MINUS-TOK? Number 
(define-syntax-class ConstClass
  #:attributes (translate)
  (pattern ((~literal Const) "none")
    #:attr translate (syntax/loc this-syntax none))
  (pattern ((~literal Const) "univ")
    #:attr translate (syntax/loc this-syntax univ))
  (pattern ((~literal Const) "iden")
    #:attr translate (syntax/loc this-syntax iden))
  (pattern ((~literal Const) n:NumberClass)
    #:attr translate (syntax/loc this-syntax (int n.value)))
  (pattern ((~literal Const) "-" n:NumberClass)
    #:attr translate (quasisyntax/loc this-syntax (int #,(* -1 (syntax->datum #'n.value))))))

; ArrowOp : (@Mult | SET-TOK)? ARROW-TOK (@Mult | SET-TOK)?
;         | STAR-TOK
(define-syntax-class ArrowOpClass
  (pattern ((~literal ArrowOp)
            (~optional (~or "lone" "some" "one" "two" "set"))
            "->"
            (~optional (~or "lone" "some" "one" "two" "set"))))
  (pattern ((~literal ArrowOp) "*")))

; CompareOp : IN-TOK | EQ-TOK | LT-TOK | GT-TOK | LEQ-TOK | GEQ-TOK | EQUIV-TOK | IS-TOK | NI-TOK
(define-syntax-class CompareOpClass
  (pattern ((~literal CompareOp)
            (~and op
                  (~or "in" "=" "<" ">" "<=" ">="
                       "is" "ni")))
    #:attr symbol (datum->syntax #'op (string->symbol (syntax->datum #'op)))))

; LetDecl : @Name /EQ-TOK Expr
(define-syntax-class LetDeclClass
  (pattern ((~literal LetDecl)
            name:id
            exp:ExprClass)
    #:attr translate (with-syntax ([exp #'exp]) 
                       #'(name exp))))

; LetDeclList : LetDecl
;             | LetDecl /COMMA-TOK @LetDeclList
(define-syntax-class LetDeclListClass
  (pattern ((~literal LetDeclList)
            decls:LetDeclClass ...)
    #:attr translate #'(decls.translate ...)))

; BlockOrBar : Block | BAR-TOK Expr
(define-syntax-class $BlockOrBar
  #:attributes (hd exprs)
  #:commit
  (pattern ((~and hd (~literal BlockOrBar)) exprs:$Block))
  (pattern ((~and hd (~literal BlockOrBar)) "|" exprs:$Expr)))

; Quant : ALL-TOK | NO-TOK | SUM-TOK | @Mult
(define-syntax-class $Quant
  #:attributes (hd symbol)
  #:commit
  (pattern ((~and hd (~literal Quant))
            (~and q (~or "all" "no" "lone" "some" "one" "two")))
    #:attr symbol (datum->syntax #'q (string->symbol (syntax-e #'q))))
  (pattern ((~and hd (~literal Quant))
            (~literal sum))
    #:attr symbol (syntax/loc #'q sum-quant)))

; ExprList : Expr
;          | Expr /COMMA-TOK @ExprList
(define-syntax-class $ExprList
  #:attributes (hd exprs)
  #:commit
  (pattern ((~and hd (~literal ExprList))
            exprs:$Expr ...)))


