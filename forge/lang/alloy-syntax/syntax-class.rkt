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
  $ArrowDecl
  $ArrowDeclList
  $LetDeclList
  $ParaDecls
  $DeclList
  $Decl

  $Expr
  $Block
  $Bounds
  $QualName
  $Name
  $Const
  $BlockOrBar
  $Quant
  $BinaryOp
  $UnaryOp
  $ExprList
  $Sexpr

  $QuantStr
  $DotStr
  )

(require
  syntax/parse
  syntax/parse/define
  (only-in racket/function curry)
  (for-syntax racket/base racket/syntax syntax/parse))

;; ---

(define-syntax (define-datum-literal-set stx)
  (syntax-parse stx
   [(_ cls-name:id (lit*:id ...))
    #:with set-name (format-id stx "~a-set" (syntax-e #'cls-name))
    #'(begin
        (define-literal-set set-name
          #:datum-literals (lit* ...)
          ())
        (define-syntax-class cls-name
          #:literal-sets ([set-name])
          (pattern (~or lit* ...))))]))

(define-simple-macro
  (define-token-class token-class:id token-string:str token-symbol)
  ;; make-token
  (define-syntax-class token-class
    (pattern token-string
      #:attr symbol #'token-symbol)))

;; ---

(define-token-class abstract-tok "abstract" #:abstract)

(define-syntax-class $DotStr
  (pattern "."))

(define-syntax-class $QuantStr
  (pattern "all")
  (pattern "no")
  (pattern "lone")
  (pattern "some")
  (pattern "one")
  (pattern "two")
  (pattern "set"))

(define-syntax-class $AlloyModule
  #:attributes (hd (import* 1) (parag* 1) (expr* 1))
  #:commit
  (pattern ((~and hd (~datum AlloyModule))
            (~optional moduledecl:$AlloyModule)
            pre-import*:$Import ...
            . pre-parag*)
    #:attr (import* 1) (syntax-e #'(pre-import* ...))
    #:attr (parag* 1) (syntax-e #'pre-parag*)
    #:attr (expr* 1) '())
  (pattern ((~and hd (~datum AlloyModule))
            ((~literal EvalDecl) "eval" pre-expr*:$Expr ...))
    #:attr (import* 1) '()
    #:attr (parag* 1) '()
    #:attr (expr* 1) (let ([ex* (syntax-e #'(pre-expr* ...))])
                       (if (or (null? ex*) (null? (cdr ex*)))
                         ex*
                         (list
                           (syntax/loc this-syntax
                             (raise "Can't eval multiple expressions.")))))))

(define-syntax-class $Import
  #:attributes (hd as-name)
  #:commit
  (pattern ((~and hd (~literal Import))
            import-name:$QualName
            (~optional (~seq "[" other-names:$QualNameList "]"))
            (~optional (~seq "as" as-name:$Name))))
  (pattern ((~and hd (~literal Import))
            file-path:str
            (~optional (~seq "as" as-name:$Name)))))

(define-syntax-class $ModuleDecl
  #:attributes (hd module-name other-name*)
  #:commit
  (pattern ((~and hd (~literal ModuleDecl))
            module-name:$QualName
            (~optional (~seq "[" other-namelist:$NameList "]")))
    #:attr other-name* #'(~? (other-namelist.name* ...) ())))

(define-syntax-class $EvalDecl
  #:attributes (hd exp)
  #:commit
  (pattern ((~and hd (~datum EvalDecl))
            "eval"
            exp:$Expr)))

;; SigDecl : VAR-TOK? ABSTRACT-TOK? Mult? /SIG-TOK NameList SigExt? /LEFT-CURLY-TOK ArrowDeclList? /RIGHT-CURLY-TOK Block?
(define-syntax-class $SigDecl
  #:attributes (hd isv abstract mult (name* 1) extends (relation-decl* 1) block)
  #:commit
  (pattern ((~and hd (~datum SigDecl))
            (~optional isv:$VarKeyword #:defaults ([isv #'#f]))
            (~optional abstract:abstract-tok)
            (~optional pre-mult:$Mult)
            namelist:$NameList
            ;when extending with in is implemented,
            ;if "sig A in B extends C" is allowed,
            ;check if this allows multiple $SigExt / how to do that if not
            ;note the parser currently does not allow that
            (~optional pre-extends:$SigExt)
            (~optional pre-relation-decls:$ArrowDeclList)
            (~optional block:$Block))
    #:attr mult #'(~? pre-mult.symbol #f)
    #:attr (name* 1) (syntax-e #'(namelist.name* ...))
    #:attr extends #'(~? pre-extends.value #f)
    #:attr (relation-decl* 1) (syntax-e #'(~? (pre-relation-decls.arrow-decl* ...) ()))))

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
            (~seq (~seq "+" names:$QualName) ...))
    #:attr symbol #'#:in
    #:attr value #'(raise "Extending with in not yet implemented.")))

; QueryDecl : @Name /COLON-TOK ArrowExpr /EQ-TOK Expr
(define-syntax-class $QueryDecl
  #:attributes (hd exp)
  #:commit
  (pattern ((~and hd (~datum QueryDecl))
            ":"
            _:$ArrowExpr
            "="
            exp:$Expr)))

;; TODO why empty? unused?
(define-syntax-class $EvalRelDecl
  #:attributes (hd )
  #:commit
  (pattern ((~and hd (~datum EvalRelDecl))
            _ ...)))

(define-syntax-class $QualName
  #:attributes (hd name)
  (pattern ((~and hd (~literal QualName))
            (~optional "this") ; TODO, allow more complex qualnames
            (~seq prefixes:id ...)
            raw-name:id)
    #:attr name #'raw-name)
  (pattern ((~and hd (~literal QualName))
            "Int")
    #:attr name #'(raise "Int as qualname?"))
  (pattern ((~and hd (~literal QualName))
            "sum")
    #:attr name #'(raise "sum as qualname?")))

(define-syntax-class $Block
  #:attributes (hd (exprs 1))
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
  #:attributes (hd (name* 1))
  #:commit
  (pattern ((~and hd (~literal NameList))
            name*:id ...)))

(define-datum-literal-set ExprHd
  (Expr Expr1 Expr2 Expr3 Expr4 Expr4.5 Expr5 Expr6 Expr7 Expr7.5 Expr8 Expr9
   Expr10 Expr11 Expr12 Expr13 Expr14 Expr15 Expr16 Expr17))

(define-syntax-class $Expr
  #:attributes (hd (body 1))
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
    #:attr symbol #`#,(string->keyword (syntax-e #'str))))

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
  #:attributes (hd (name* 1) expr translate)
  #:commit
  (pattern ((~and hd (~literal Decl))
            ;(~optional "disj")
            namelist:$NameList
            ;(~optional "disj")
            (~optional "set")
            expr:$Expr)
    #:attr (name* 1) (syntax-e #'(namelist.name* ...))
    #:attr translate #'((namelist.name* expr) ...)))

; DeclList : Decl
;          | Decl /COMMA-TOK @DeclList
(define-syntax-class $DeclList
  #:attributes (hd (decls 1) translate)
  #:commit
  (pattern ((~and hd (~literal DeclList))
            decls:$Decl ...)
    #:attr translate (datum->syntax this-syntax
                                    (apply append
                                           (map syntax->list
                                                (syntax->list #'(decls.translate ...)))))))

; ArrowDecl : DISJ-TOK? NameList /COLON-TOK DISJ-TOK? ArrowMult ArrowExpr
(define-syntax-class $ArrowDecl
  #:attributes (hd (name* 1) (type* 1) mult is-var)
  #:commit
  (pattern ((~and hd (~literal ArrowDecl))
            ;(~optional "disj")
            (~optional isv:$VarKeyword #:defaults ([isv #'#f])) ; electrum
            namelist:$NameList
            ;(~optional "disj") 
            mult-class:$ArrowMult
            typelist:$ArrowExpr)
    #:attr (name* 1) (syntax-e #'(namelist.name* ...))
    #:attr (type* 1) (syntax-e #'(typelist.name* ...))
    #:attr mult #'mult-class.symbol
    #:attr is-var #'isv))

; ArrowDeclList : ArrowDecl
;               | ArrowDecl /COMMA-TOK @ArrowDeclList
(define-syntax-class $ArrowDeclList
  #:attributes (hd (arrow-decl* 1))
  #:commit
  (pattern ((~and hd (~literal ArrowDeclList))
            arrow-decl*:$ArrowDecl ...)))

; ArrowExpr : QualName
;           | QualName /ARROW-TOK @ArrowExpr
(define-syntax-class $ArrowExpr
  #:attributes (hd (name* 1))
  #:commit
  (pattern ((~and hd (~literal ArrowExpr))
            name-list:$QualName ...)
    #:attr (name* 1) (syntax-e #'(name-list.name ...))))

; FactDecl : FACT-TOK Name? Block
(define-syntax-class $FactDecl
  #:attributes (hd name block)
  #:commit
  (pattern ((~and hd (~literal FactDecl))
            "fact"
            (~optional pre-name:$Name)
            block:$Block)
    #:attr name #'(~? pre-name.name #f)))

; PredDecl : /PRED-TOK (QualName DOT-TOK)? Name ParaDecls? Block
(define-syntax-class $PredDecl
  #:attributes (hd prefix name decls block)
  #:commit
  (pattern ((~and hd (~literal PredDecl))
            (~optional (~seq prefix:$QualName "."))
            pre-name:$Name
            (~optional decls:$ParaDecls)
            block:$Block)
    #:attr name #'pre-name.name))

; FunDecl : /FUN-TOK (QualName DOT-TOK)? Name ParaDecls? /COLON-TOK Expr Block
(define-syntax-class $FunDecl
  #:attributes (hd prefix name decls output body)
  #:commit
  (pattern ((~and hd (~literal FunDecl))
            (~optional (~seq prefix:$QualName "."))
            pre-name:$Name
            (~optional decls:$ParaDecls)
            output:$Expr
            body:$Expr)
    #:attr name #'pre-name.name))

; ParaDecls : /LEFT-PAREN-TOK @DeclList? /RIGHT-PAREN-TOK 
;           | /LEFT-SQUARE-TOK @DeclList? /RIGHT-SQUARE-TOK
; DeclList : Decl
;          | Decl /COMMA-TOK @DeclList
(define-syntax-class $ParaDecls
  #:attributes (hd (decls 1))
  #:commit
  (pattern ((~and hd (~literal ParaDecls))
            (~seq decls:$Decl ...))
    ;; TODO clean up, find examples, is this even used?
    #:attr translate (datum->syntax #'(decls ...)
                                    (apply append (map (compose (curry map car )
                                                                (curry map syntax->list )
                                                                syntax->list)
                                                       (syntax->list #'(decls.translate ...)))))))

; AssertDecl : /ASSERT-TOK Name? Block
(define-syntax-class $AssertDecl
  #:attributes (hd name block)
  #:commit
  (pattern ((~and hd (~literal AssertDecl))
            (~optional pre-name:$Name)
            block:$Block)
    #:attr name #'(~? pre-name.name #f)))

; CmdDecl :  (Name /COLON-TOK)? (RUN-TOK | CHECK-TOK) Parameters? (QualName | Block)? Scope? (/FOR-TOK Bounds)?
(define-syntax-class $CmdDecl
  #:attributes (hd name parameters pred-name pred-block scope bounds)
  #:commit
  (pattern ((~and hd (~literal CmdDecl))
            (~optional pre-name:$Name)
            (~or "run" "check")
            (~optional parameters:$Parameters)
            (~optional (~or pred-name:$QualName
                            pred-block:$Block))
            (~optional scope:$Scope)
            (~optional bounds:$Bounds))
    #:attr name #'(~? pre-name.name #f)))

; TestDecl : (Name /COLON-TOK)? Parameters? (QualName | Block)? Scope? (/FOR-TOK Bounds)? /IS-TOK (SAT-TOK | UNSAT-TOK)
(define-syntax-class $TestDecl
  #:attributes (hd name parameters pred-name pred-block scope bounds)
  #:commit
  (pattern ((~and hd (~literal TestDecl))
            (~optional name:$Name)
            (~optional parameters:$Parameters)
            (~optional (~or pred-name:$QualName
                            pred-block:$Block))
            (~optional scope:$Scope)
            (~optional bounds:$Bounds)
            (~or "sat" "unsat" "theorem"))))

; TestBlock : /LEFT-CURLY-TOK TestDecl* /RIGHT-CURLY-TOK
(define-syntax-class $TestBlock
  #:attributes (hd (test-decls 1))
  #:commit
  (pattern ((~and hd (~literal TestBlock))
            test-decls:$TestDecl ...)))

; TestExpectDecl : TEST-TOK? EXPECT-TOK Name? TestBlock
(define-syntax-class $TestExpectDecl
  #:attributes (hd name test-block)
  #:commit
  (pattern ((~and hd (~literal TestExpectDecl))
            (~optional "test")
            "expect"
            (~optional name:$Name)
            test-block:$TestBlock)))

(define-syntax-class $ExampleDecl
  #:attributes (hd name pred bounds)
  #:commit
  (pattern ((~and hd (~literal ExampleDecl))
            (~optional name:$Name)
            pred:$Expr
            bounds:$Bounds)))

; Scope : /FOR-TOK Number (/BUT-TOK @TypescopeList)? 
;       | /FOR-TOK @TypescopeList
; TypescopeList : Typescope
;               | Typescope /COMMA-TOK @TypescopeList
(define-syntax-class $Scope
  #:attributes (hd default (typescope 1) translate)
  #:commit
  (pattern ((~and hd (~literal Scope))
            (~optional default:$Number)
            (~seq typescope:$Typescope ...))
    #:attr translate #'(typescope.translate ...)))

; Typescope : EXACTLY-TOK? Number QualName
(define-syntax-class $Typescope
  #:attributes (hd num name translate)
  #:commit
  (pattern ((~and hd (~literal Typescope))
            (~optional (~and "exactly" exactly))
            num:$Number
            name:$QualName)
    #:attr translate #'(~? (name.name num.value num.value)
                           (name.name 0 num.value))))

; OptionDecl : /OPTION-TOK QualName (QualName | FILE-PATH-TOK | Number)
(define-syntax-class $OptionDecl
  #:attributes (n v)
  #:commit
  (pattern ((~literal OptionDecl) name:$QualName value:$QualName)
           #:attr n #'name.name
           #:attr v #'value.name)
  (pattern ((~literal OptionDecl) name:$QualName value:str)
           #:attr n #'name.name
           #:attr v #'value)
  (pattern ((~literal OptionDecl) name:$QualName value:$Number)
           #:attr n #'name.name
           #:attr v #'value.value)
  (pattern ((~literal OptionDecl) name:$QualName "-" value:$Number)
           #:attr n #'name.name
           #:attr v (quasisyntax #,(* -1 (syntax->datum #'value.value)))))


; QualNameList : @QualName
;              | @QualName /COMMA-TOK @QualNameList
(define-syntax-class $QualNameList
  #:attributes (hd (prefixes 2) (name 1))
  #:commit
  (pattern ((~and hd (~literal QualNameList))
            (~or (~seq (~optional "this")
                       (~seq prefixes:id ...)
                       name:id)
                 "Int"
                 "sum") ...)))

; Number : NUM-CONST-TOK
(define-syntax-class $Number
  #:attributes (hd value)
  #:commit
  (pattern ((~and hd (~literal Number)) n)
    #:attr value #'n))

; SexprDecl : Sexpr
(define-syntax-class $SexprDecl
  #:attributes (hd exp)
  #:commit
  (pattern ((~and hd (~literal SexprDecl)) exp:$Sexpr)))

; Sexpr : SEXPR-TOK
(define-syntax-class $Sexpr
  #:attributes (hd exp)
  #:commit
  (pattern ((~and hd (~literal Sexpr)) exp)))

; InstDecl : /INST-TOK Name Bounds Scope?
(define-syntax-class $InstDecl
  #:attributes (hd name bounds scope)
  #:commit
  (pattern ((~and hd (~literal InstDecl))
            name:$Name
            bounds:$Bounds
            (~optional scope:$Scope))))

; RelDecl : ArrowDecl
(define-syntax-class $RelDecl
  #:attributes (hd decl)
  #:commit
  (pattern ((~and hd (~literal RelDecl)) decl:$ArrowDecl)))

; Parameters : /LeftAngle @QualNameList /RightAngle 
(define-syntax-class $Parameters
  #:attributes (hd (name 1))
  #:commit
  (pattern ((~and hd (~literal Parameters))
            name:$QualName ...)))

; Bounds : EXACTLY-TOK? @ExprList
;        | EXACTLY-TOK? @Block
(define-syntax-class $Bounds
  #:attributes (hd (exprs 1) translate)
  #:commit
  (pattern ((~and hd (~literal Bounds))
            (~optional "exactly")
            exprs:$Expr ...)
    ;; TODO cleanup
    #:attr translate (datum->syntax this-syntax
                                    (syntax->list #'(exprs ...)))))

; EXPRESSIONS

; Const : NONE-TOK | UNIV-TOK | IDEN-TOK
;       | MINUS-TOK? Number 
(define-syntax-class $Const
  #:attributes (translate)
  #:commit
  (pattern ((~literal Const) "none")
    #:attr translate (syntax/loc this-syntax none))
  (pattern ((~literal Const) "univ")
    #:attr translate (syntax/loc this-syntax univ))
  (pattern ((~literal Const) "iden")
    #:attr translate (syntax/loc this-syntax iden))
  (pattern ((~literal Const) n:$Number)
    #:attr translate (syntax/loc this-syntax (int n.value)))
  (pattern ((~literal Const) "-" n:$Number)
    #:attr translate (quasisyntax/loc this-syntax (int #,(- (syntax->datum #'n.value))))))

; ArrowOp : (@Mult | SET-TOK)? ARROW-TOK (@Mult | SET-TOK)?
;         | STAR-TOK
(define-syntax-class $ArrowOp
  #:attributes ()
  #:commit
  (pattern ((~literal ArrowOp)
            (~optional (~or "lone" "some" "one" "two" "set"))
            "->"
            (~optional (~or "lone" "some" "one" "two" "set"))))
  (pattern ((~literal ArrowOp) "*")))

(define-syntax-class $UnaryOp
  #:attributes (op symbol)
  #:commit
  (pattern (~and op
                  (~or "in" "=" "<" ">" "<=" ">="
                       "is" "ni"
                       "#" "~" "^" "*" ))
    #:attr symbol (datum->syntax #'op (string->symbol (syntax->datum #'op)))))

(define-syntax-class $BinaryOp
  #:attributes (op symbol)
  #:commit
  (pattern (~and op
                  (~or "or" "||" "iff" "<=>" "implies" "=>"
                       "and" "&&" "releases" "until" "since"
                       "triggered"
                       "+" "-" "++" "&"
                       :$ArrowOp
                       ":>" "<:" ))
    #:attr symbol (datum->syntax #'op (string->symbol (syntax->datum #'op)))))

(define-syntax-class $CompareOp
  #:attributes (op symbol)
  #:commit
  (pattern ((~literal CompareOp)
            (~and op
                  (~or "in" "=" "<" ">" "<=" ">="
                       "is" "ni")))
    #:attr symbol (datum->syntax #'op (string->symbol (syntax->datum #'op)))))

; LetDecl : @Name /EQ-TOK Expr
(define-syntax-class $LetDecl
  #:attributes (name exp translate)
  #:commit
  (pattern ((~literal LetDecl)
            name:id
            exp:$Expr)
    #:attr translate (with-syntax ([exp #'exp]) 
                       #'(name exp))))

; LetDeclList : LetDecl
;             | LetDecl /COMMA-TOK @LetDeclList
(define-syntax-class $LetDeclList
  #:attributes ((decls 1) translate)
  #:commit
  (pattern ((~literal LetDeclList)
            decls:$LetDecl ...)
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
            q:$QuantStr)
    #:attr symbol (datum->syntax #'q (string->symbol (syntax-e #'q))))
  (pattern ((~and hd (~literal Quant))
            (~literal sum))
    #:attr symbol (syntax/loc #'q sum-quant)))

; ExprList : Expr
;          | Expr /COMMA-TOK @ExprList
(define-syntax-class $ExprList
  #:attributes (hd (exprs 1))
  #:commit
  (pattern ((~and hd (~literal ExprList))
            exprs:$Expr ...)))


