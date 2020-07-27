#lang racket

(require (for-syntax syntax/parse)
         (for-syntax (for-syntax syntax/parse)))
(require syntax/parse/define
         (for-syntax syntax/parse/define))
(require (for-syntax (for-syntax racket/base)))
(require (for-syntax racket/function))
(require "../sigs.rkt")
(require "ast.rkt")


(provide #%module-begin)
(provide #%top #%app #%datum #%top-interaction)

(provide provide all-defined-out)
(provide nsa define-namespace-anchor)
; (provide (all-from-out "ast.rkt"))
(provide (all-from-out "../sigs.rkt"))
(provide (all-defined-out))

(begin-for-syntax
  (define-syntax-parser make-token
    [(make-token token-class:id token-string:str token-symbol)
     #'(define-syntax-class token-class
         (pattern token-string
           #:attr symbol #'token-symbol))])

  (make-token abstract-tok "abstract" #:abstract))

(define-for-syntax (my-expand stx)
  (define forge-core-macros
    (list #'<= #'is #'= #'in #'ni
          #'card #'node/int/constant))
  (define result (local-expand stx 'expression #f))
  result)


(begin-for-syntax
  ; AlloyModule : ModuleDecl? Import* Paragraph*
  ;             | EvalDecl*
  (define-syntax-class AlloyModuleClass
    (pattern ((~literal AlloyModule)
              (~optional module-decl:ModuleDeclClass)
              (~seq import:ImportClass ...)
              (~seq paragraph:ParagraphClass ...)))
    (pattern ((~literal AlloyModule)
              (~seq eval-decl:EvalDeclClass ...))))


  ; Header stuff

  ; ModuleDecl : /MODULE-TOK QualName (LEFT-SQUARE-TOK NameList RIGHT-SQUARE-TOK)?
  (define-syntax-class ModuleDeclClass
    (pattern ((~literal ModuleDecl)
              module-name:QualNameClass
              (~optional (~seq "[" other-names:NameListClass "]")))))

  ; Import : OPEN-TOK QualName (LEFT-SQUARE-TOK QualNameList RIGHT-SQUARE-TOK)? (AS-TOK Name)?
  (define-syntax-class ImportClass
    (pattern ((~literal Import)
              import-name:QualNameClass
              (~optional (~seq "[" other-names:QualNameListClass "]"))
              (~optional (~seq "as" as-name:NameClass))))
    (pattern ((~literal Import)
              file-path:str
              (~optional (~seq "as" as-name:NameClass)))))


  ; Main Decls

  ; EvalDecl : EVAL-TOK Expr
  (define-syntax-class EvalDeclClass
    (pattern ((~literal EvalDecl)
              "eval"
              exp:ExprClass)))

  ; @Paragraph : SigDecl | FactDecl | PredDecl | FunDecl | AssertDecl 
  ;                      | CmdDecl | TestExpectDecl | SexprDecl | BreakDecl 
  ;                      | InstanceDecl | QueryDecl | StateDecl | TransitionDecl 
  ;                      | RelDecl | OptionDecl | InstDecl | TraceDecl
  (define-syntax-class ParagraphClass
    (pattern decl:SigDeclClass)
    (pattern decl:FactDeclClass)
    (pattern decl:PredDeclClass)
    (pattern decl:FunDeclClass)
    (pattern decl:AssertDeclClass)
    (pattern decl:CmdDeclClass)
    (pattern decl:TestExpectDeclClass)
    (pattern decl:SexprDeclClass)
    ; (pattern decl:BreakDeclClass)
    ; (pattern decl:InstanceDeclClass)
    ; (pattern decl:QueryDeclClass)
    ; (pattern decl:StateDeclClass)
    ; (pattern decl:TransitionDeclClass)
    (pattern decl:RelDeclClass)
    (pattern decl:OptionDeclClass)
    (pattern decl:InstDeclClass))
    ; (pattern decl:TraceDeclClass))

  ; SigDecl : ABSTRACT-TOK? Mult? /SIG-TOK NameList SigExt? /LEFT-CURLY-TOK ArrowDeclList? /RIGHT-CURLY-TOK Block?
  (define-syntax-class SigDeclClass
    (pattern ((~literal SigDecl)
              (~optional abstract:abstract-tok)
              (~optional mult:MultClass)
              sig-names:NameListClass
              (~optional extends:SigExtClass)
              (~optional relation-decls:ArrowDeclListClass)
              (~optional block:BlockClass))))

  ; SigExt : EXTENDS-TOK QualName 
  ;        | IN-TOK QualName (PLUS-TOK QualName)*
  (define-syntax-class SigExtClass
    (pattern ((~literal SigExt)
              "extends"
              name:QualNameClass)
      #:attr symbol #'#:extends
      #:attr value #'name.name)
    (pattern ((~literal SigExt)
              "in" 
              name:QualNameClass
              (~seq (~seq "+" names:QualNameClass) ...))
      #:attr symbol #'#:in
      #:attr value #'(raise "Extending with in not yet implemented.")))

  ; Mult : LONE-TOK | SOME-TOK | ONE-TOK | TWO-TOK
  (define-syntax-class MultClass
    (pattern ((~literal Mult) "lone") #:attr symbol #'#:lone)
    (pattern ((~literal Mult) "some") #:attr symbol #'#:some)
    (pattern ((~literal Mult) "one") #:attr symbol #'#:one)
    (pattern ((~literal Mult) "two") #:attr symbol #'#:two))

  ; ArrowMult : LONE-TOK | SET-TOK | ONE-TOK | TWO-TOK
  (define-syntax-class ArrowMultClass
    (pattern ((~literal ArrowMult) "lone") #:attr symbol #'#:lone)
    (pattern ((~literal ArrowMult) "set") #:attr symbol #'#:set)
    (pattern ((~literal ArrowMult) "one") #:attr symbol #'#:one)
    (pattern ((~literal ArrowMult) "two") #:attr symbol #'#:two))

  ; Decl : DISJ-TOK? NameList /COLON-TOK DISJ-TOK? SET-TOK? Expr
  (define-syntax-class DeclClass
    (pattern ((~literal Decl)
              (~optional "disj")
              names:NameListClass
              (~optional "disj")
              (~optional "set")
              expr:ExprClass)
      #:attr translate (with-syntax ([expr (my-expand #'expr)])
                         #'((names.names expr) ...))))

  ; DeclList : Decl
  ;          | Decl /COMMA-TOK @DeclList
  (define-syntax-class DeclListClass
    (pattern ((~literal DeclList)
              decls:DeclClass ...)
      #:attr translate (datum->syntax #'(decls ...) 
                                      (apply append 
                                             (map syntax->list 
                                                  (syntax->list #'(decls.translate ...)))))))

  ; ArrowDecl : DISJ-TOK? NameList /COLON-TOK DISJ-TOK? ArrowMult ArrowExpr
  (define-syntax-class ArrowDeclClass
    (pattern ((~literal ArrowDecl)
              (~optional "disj")
              name-list:NameListClass
              (~optional "disj")
              mult:ArrowMultClass
              type-list:ArrowExprClass)
      #:attr names #'(name-list.names ...)
      #:attr types #'type-list.names))

  ; ArrowDeclList : ArrowDecl
  ;               | ArrowDecl /COMMA-TOK @ArrowDeclList
  (define-syntax-class ArrowDeclListClass
    (pattern ((~literal ArrowDeclList)
              arrow-decl:ArrowDeclClass ...)))

  ; ArrowExpr : QualName
  ;           | QualName /ARROW-TOK @ArrowExpr
  (define-syntax-class ArrowExprClass
    (pattern ((~literal ArrowExpr)
              name-list:QualNameClass ...)
      #:attr names #'(name-list.name ...)))

  ; FactDecl : FACT-TOK Name? Block
  (define-syntax-class FactDeclClass
    (pattern ((~literal FactDecl)
              "fact" 
              (~optional name:NameClass)
              block:BlockClass)))

  ; PredDecl : /PRED-TOK (QualName DOT-TOK)? Name ParaDecls? Block
  (define-syntax-class PredDeclClass
    (pattern ((~literal PredDecl)
              (~optional (~seq prefix:QualNameClass "."))
              name:NameClass
              (~optional decls:ParaDeclsClass)
              block:BlockClass)))

  ; FunDecl : /FUN-TOK (QualName DOT-TOK)? Name ParaDecls? /COLON-TOK Expr Block
  (define-syntax-class FunDeclClass
    (pattern ((~literal FunDecl)
              (~optional (~seq prefix:QualNameClass "."))
              name:NameClass
              (~optional decls:ParaDeclsClass)
              output:ExprClass
              block:BlockClass)))

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
              block:BlockClass)))

  ; CmdDecl :  (Name /COLON-TOK)? (RUN-TOK | CHECK-TOK) Parameters? (QualName | Block)? Scope? (/FOR-TOK Bounds)?
  (define-syntax-class CmdDeclClass
    (pattern ((~literal CmdDecl)
              (~optional name:NameClass)
              (~or "run" "check")
              (~optional parameters:ParametersClass)
              (~optional (~or pred-name:QualNameClass
                              pred-block:BlockClass))
              (~optional scope:ScopeClass)
              (~optional bounds:BoundsClass))))

  ; TestDecl : (Name /COLON-TOK)? Parameters? (QualName | Block)? Scope? (/FOR-TOK Bounds)? /IS-TOK (SAT-TOK | UNSAT-TOK)
  (define-syntax-class TestDeclClass
    (pattern ((~literal TestDecl)
              (~optional name:NameClass)
              (~optional parameters:ParametersClass)
              (~optional (~or pred-name:QualNameClass
                              pred-block:BlockClass))
              (~optional scope:ScopeClass)
              (~optional bounds:BoundsClass)
              (~or "sat" "unsat"))))

  ; TestBlock : /LEFT-CURLY-TOK TestDecl* /RIGHT-CURLY-TOK
  (define-syntax-class TestBlockClass
    (pattern ((~literal TestBlock)
              test-decls:TestDeclClass ...)))

  ; TestExpectDecl : TEST-TOK? EXPECT-TOK Name? TestBlock
  (define-syntax-class TestExpectDeclClass
    (pattern ((~literal TestExpectDecl)
              (~optional "test")
              "expect"
              (~optional name:NameClass)
              test-block:TestBlockClass)))

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
    (pattern ((~literal OptionDecl)
              name:QualNameClass
              (~or value:QualNameClass
                   value:str
                   value:NumberClass))))

  ; Block : /LEFT-CURLY-TOK Expr* /RIGHT-CURLY-TOK
  (define-syntax-class BlockClass
    (pattern ((~literal Block)
              exprs:ExprClass ...)))

  ; Name : IDENTIFIER-TOK
  (define-syntax-class NameClass
    (pattern ((~literal Name)
              name:id)))

  ; NameList : @Name
  ;          | @Name /COMMA-TOK @NameList
  (define-syntax-class NameListClass
    (pattern ((~literal NameList)
              names:id ...)))

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
                                      (map my-expand (syntax->list #'(exprs ...))))))

  ; EXPRESSIONS

  ; Const : NONE-TOK | UNIV-TOK | IDEN-TOK
  ;       | MINUS-TOK? Number 
  (define-syntax-class ConstClass
    #:attributes (translate)
    (pattern ((~literal Const) "none")
      #:attr translate #'none)
    (pattern ((~literal Const) "univ")
      #:attr translate #'univ)
    (pattern ((~literal Const) "iden")
      #:attr translate #'iden)
    (pattern ((~literal Const) n:NumberClass)
      #:attr translate #'(node/int/constant n.value))
    (pattern ((~literal Const) "-" n:NumberClass)
      #:attr translate (datum->syntax #'n `(node/int/constant ,(* -1 (syntax->datum #'n.value))))))

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
                         "==" "is" "ni")))
      #:attr symbol (datum->syntax #'op (string->symbol (syntax->datum #'op)))))

  ; LetDecl : @Name /EQ-TOK Expr
  (define-syntax-class LetDeclClass
    (pattern ((~literal LetDecl)
              name:id
              exp:ExprClass)
      #:attr translate (with-syntax ([exp (my-expand #'exp)]) 
                         #'(name exp))))

  ; LetDeclList : LetDecl
  ;             | LetDecl /COMMA-TOK @LetDeclList
  (define-syntax-class LetDeclListClass
    (pattern ((~literal LetDeclList)
              decls:LetDeclClass ...)
      #:attr translate #'(decls.translate ...)))

  ; BlockOrBar : Block | BAR-TOK Expr
  (define-syntax-class BlockOrBarClass
      #:attributes (exprs)
    (pattern ((~literal BlockOrBar) block:BlockClass)
      #:attr exprs (my-expand #'block))
    (pattern ((~literal BlockOrBar) "|" exp:ExprClass)
      #:attr exprs (my-expand #'exp)))

  ; Quant : ALL-TOK | NO-TOK | SUM-TOK | @Mult
  (define-syntax-class QuantClass
    (pattern ((~literal Quant) (~and q (~or "all" "no" "sum" "lone"
                                            "some" "one" "two")))
      #:attr symbol (datum->syntax #'q
                                   (string->symbol (syntax->datum #'q)))))

  (define-syntax-class ExprClass
    (pattern ((~or (~literal Expr) (~literal Expr1) (~literal Expr2) (~literal Expr3)
                   (~literal Expr4) (~literal Expr5) (~literal Expr6) (~literal Expr7)
                   (~literal Expr8) (~literal Expr9) (~literal Expr10) (~literal Expr11)
                   (~literal Expr12) (~literal Expr13) (~literal Expr14) (~literal Expr15)
                   (~literal Expr16) (~literal Expr17))
             _ ...)))

  ; ExprList : Expr
  ;          | Expr /COMMA-TOK @ExprList
  (define-syntax-class ExprListClass
    (pattern ((~literal ExprList)
              exprs:ExprClass ...))))



; AlloyModule : ModuleDecl? Import* Paragraph*
;             | EvalDecl*
(define-syntax-parser AlloyModule
  [((~literal AlloyModule) (~optional module-decl:ModuleDeclClass)
                           (~seq import:ImportClass ...)
                           (~seq paragraph:ParagraphClass ...))
   #'(begin
     (~? module-decl)
     import ...
     paragraph ...)]
  [((~literal AlloyModule) (~seq eval-decl:EvalDeclClass ...))
   #'(raise "Evaluating in #lang forge not yet implemented.")])

; ModuleDecl : /MODULE-TOK QualName (LEFT-SQUARE-TOK NameList RIGHT-SQUARE-TOK)?
(define-syntax-parser ModuleDecl
  [((~literal ModuleDecl) module-name:QualNameClass
                          (~optional (~seq "[" other-names:NameListClass "]")))
   #'(raise "ModuleDecl not yet implemented.")])


; Import : OPEN-TOK QualName (LEFT-SQUARE-TOK QualNameList RIGHT-SQUARE-TOK)? (AS-TOK Name)?
(define-syntax-parser Import
  [((~literal Import) file-path:str
                      (~optional (~seq "as" as-name:NameClass)))
   #'(begin
     (~? (require (prefix-in as-name.name file-path))
         (require file-path)))]
  [((~literal Import) import-name:QualNameClass
                      (~optional (~seq "[" other-names:QualNameListClass "]"))
                      (~optional (~seq "as" as-name:NameClass)))
   #'(begin
     (raise (format "Importing packages not yet implemented: ~a." 'import-name))
     (~? (raise (format "Bracketed import not yet implemented. ~a" 'other-names)))
     (~? (raise (format "Importing as not yet implemented. ~a" 'as-name))))])

; SigDecl : ABSTRACT-TOK? Mult? /SIG-TOK NameList SigExt? /LEFT-CURLY-TOK ArrowDeclList? /RIGHT-CURLY-TOK Block?
(define-syntax-parser SigDecl
  [((~literal SigDecl) (~optional abstract:abstract-tok)
                       (~optional mult:MultClass)
                       sig-names:NameListClass
                       (~optional extends:SigExtClass)
                       (~optional block:BlockClass))
   #'(begin
     (~? (raise (format "Sig block not yet implemented: ~a" 'block)))
     (sig sig-names.names (~? mult.symbol) 
                          (~? abstract.symbol) 
                          (~? (~@ extends.symbol extends.value))) ...)]

  [((~literal SigDecl) (~optional abstract:abstract-tok)
                       (~optional mult:MultClass)
                       sig-names:NameListClass
                       (~optional extends:SigExtClass)
                       ((~literal ArrowDeclList) arrow-decl:ArrowDeclClass ...)
                       (~optional block:BlockClass))
   #`(begin
     (~? (raise (format "Sig block not yet implemented: ~a" 'block)))
     (sig sig-names.names (~? mult.symbol) 
                          (~? abstract.symbol) 
                          (~? (~@ extends.symbol extends.value))) ...
   #,@(apply append
        (for/list ([sig-name (syntax->list #'(sig-names.names ...))]
                   #:when #t
                   ; [relation-decl (syntax->list #'(arrow-decl ...))]
                   [relation-names (syntax->list #'(arrow-decl.names ...))]
                   [relation-types (syntax->list #'(arrow-decl.types ...))])
          (with-syntax ([sig-name sig-name]
                        ; [relation-names relation-names]
                        [relation-types (datum->syntax relation-types 
                                                       (cons (syntax->datum sig-name)
                                                             (syntax->list relation-types)))])
            (for/list ([relation-name (syntax->list relation-names)])
              (with-syntax ([relation-name relation-name])
                #'(relation relation-name relation-types)))))))])
   
; RelDecl : ArrowDecl
(define-syntax-parser RelDecl
  [((~literal RelDecl) arrow-decl:ArrowDeclClass)
   #`(begin
   #,@(for/list ([name (syntax->list #'arrow-decl.names)])
        (with-syntax ([name name])
          #'(relation name arrow-decl.types))))])

; FactDecl : FACT-TOK Name? Block
(define-syntax-parser FactDecl
  [((~literal FactDecl) _ ...)
   #'(raise "Facts are not allowed in #lang forge.")])

; PredDecl : /PRED-TOK (QualName DOT-TOK)? Name ParaDecls? Block
(define-syntax-parser PredDecl
  [((~literal PredDecl) (~optional (~seq prefix:QualNameClass "."))
                        name:NameClass
                        block:BlockClass)
   (with-syntax ([block (my-expand #'block)])
     #'(begin
       (~? (raise (format "Prefixes not allowed: ~a" 'prefix)))
       (pred name.name block)))]

  [((~literal PredDecl) (~optional (~seq prefix:QualNameClass "."))
                        name:NameClass
                        decls:ParaDeclsClass
                        block:BlockClass)
   (with-syntax ([decl (datum->syntax #'name (cons (syntax->datum #'name.name)
                                                   (syntax->list #'decls.translate)))]
                 [block (my-expand #'block)])
     #'(begin
       (~? (raise (format "Prefixes not allowed: ~a" 'prefix)))
       (pred decl block)))])

; FunDecl : /FUN-TOK (QualName DOT-TOK)? Name ParaDecls? /COLON-TOK Expr Block
(define-syntax-parser FunDecl
  [((~literal FunDecl) (~optional (~seq prefix:QualNameClass "."))
                       name:NameClass
                       output:ExprClass
                       block:BlockClass)
   (with-syntax ([block (my-expand #'block)])
     #'(begin
       (~? (raise (format "Prefixes not allowed: ~a" 'prefix)))
       (const name.name block)))]

  [((~literal FunDecl) (~optional (~seq prefix:QualNameClass "."))
                       name:NameClass
                       decls:ParaDeclsClass
                       output:ExprClass
                       block:BlockClass)
   (with-syntax ([decl (datum->syntax #'name (cons (syntax->datum #'name.name)
                                                   (syntax->list #'decls.translate)))]
                 [block (my-expand #'block)])
     #'(begin
       (~? (raise (format "Prefixes not allowed: ~a" 'prefix)))
       (fun decl block)))])

; AssertDecl : /ASSERT-TOK Name? Block
(define-syntax-parser AssertDecl
  [((~literal AssertDecl) _ ...)
   #'(raise "Assertions not yet implemented.")])

; CmdDecl :  (Name /COLON-TOK)? (RUN-TOK | CHECK-TOK) Parameters? (QualName | Block)? Scope? (/FOR-TOK Bounds)?
(define-syntax-parser CmdDecl
  [((~literal CmdDecl) (~optional name:NameClass)
                       (~and cmd-type (~or "run" "check"))
                       (~optional parameters:ParametersClass)
                       (~optional (~or pred:QualNameClass
                                       preds:BlockClass))
                       (~optional scope:ScopeClass)
                       (~optional bounds:BoundsClass))
   (with-syntax ([cmd-type (datum->syntax #'cmd-type
                                         (string->symbol (syntax->datum #'cmd-type)))]
                 [name #'(~? name.name temporary-name)]
                 [preds (my-expand #'(~? pred.name preds))])
     #'(begin
       (cmd-type name (~? (~@ #:preds [preds]))
                      (~? (~@ #:scope scope.translate))
                      (~? (~@ #:bounds bounds.translate)))
       (display name)))])

; TestDecl : (Name /COLON-TOK)? Parameters? (QualName | Block)? Scope? (/FOR-TOK Bounds)? /IS-TOK (SAT-TOK | UNSAT-TOK)
(define-syntax-parser TestDecl
  [((~literal TestDecl) (~optional name:NameClass)
                        (~optional parameters:ParametersClass)
                        (~optional (~or pred:QualNameClass
                                        preds:BlockClass))
                        (~optional scope:ScopeClass)
                        (~optional bounds:BoundsClass)
                        (~and sat-or-unsat (~or "sat" "unsat")))
   (with-syntax ([name #'(~? name.name temporary-name)]
                 [preds (my-expand #'(~? pred.name preds))]
                 [sat-or-unsat (datum->syntax #'sat-or-unsat
                                              (string->symbol (syntax->datum #'sat-or-unsat)))])
     #'(begin
       (test name (~? (~@ #:preds [preds]))
                  (~? (~@ #:scope scope.translate))
                  (~? (~@ #:bounds bounds.translate))
                  sat-or-unsat)))])

; TestExpectDecl : TEST-TOK? EXPECT-TOK Name? TestBlock
(define-syntax-parser TestExpectDecl 
  [((~literal TestExpectDecl) (~optional (~and "test" test-tok))
                              "expect" 
                              (~optional name:NameClass)
                              block:TestBlockClass)
   (if (attribute test-tok)
       #'(begin block.test-decls ...)
       #'(begin))])

; OptionDecl : /OPTION-TOK QualName (QualName | FILE-PATH-TOK | Number)
(define-syntax-parser OptionDecl
  #:datum-literals (QualName verbosity Number)
  [((~literal OptionDecl) (QualName verbosity) (Number n))
   #'(set-verbosity n)])

; InstDecl : /INST-TOK Name Bounds Scope?
(define-syntax-parser InstDecl
  [((~literal InstDecl)
              name:NameClass
              bounds:BoundsClass
              (~optional scope:ScopeClass))
   (with-syntax ([(bounds ...) #'bounds.translate])
     #'(begin
       (~? (raise (format "Scope not implemented for bounds ~a" 'scope)))
       (inst name.name bounds ...)))])


; Block : /LEFT-CURLY-TOK Expr* /RIGHT-CURLY-TOK
(define-syntax-parser Block
  [((~literal Block) exprs:ExprClass ...)
   (with-syntax ([(exprs ...) (map my-expand (syntax->list #'(exprs ...)))])
     #'(and exprs ...))])

(define-syntax-parser Expr
  [((~literal Expr) "let" decls:LetDeclListClass bob:BlockOrBarClass)
   #'(let decls.translate bob.exprs)]

  [((~literal Expr) "bind" decls:LetDeclListClass bob:BlockOrBarClass)
   #'(raise "bind not implemented.")]

  [((~literal Expr) q:QuantClass decls:DeclListClass bob:BlockOrBarClass)
   (syntax/loc #'q (q.symbol decls.translate bob.exprs))]


  [((~literal Expr) expr1:ExprClass (~or "or" "||") expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)])
     #'(or expr1 expr2))]

  [((~literal Expr) expr1:ExprClass (~or "iff" "<=>") expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)])
     #'(iff expr1 expr2))]

  [((~literal Expr) expr1:ExprClass (~or "implies" "=>") expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)])
     #'(implies expr1 expr2))]

  [((~literal Expr) expr1:ExprClass (~or "implies" "=>") expr2:ExprClass
                                    "else" expr3:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)]
                 [expr3 (my-expand #'expr3)])
     #'(ifte expr1 expr2))]

  [((~literal Expr) expr1:ExprClass (~or "and" "&&") expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)])
     #'(and expr1 expr2))]

  [((~literal Expr) (~or "!" "not") expr1:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)])
     #'(not expr1))]

  [((~literal Expr) expr1:ExprClass op:CompareOpClass expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)]
                 [op #'op.symbol])
     #'(op expr1 expr2))]

  [((~literal Expr) expr1:ExprClass 
                    (~or "!" "not") op:CompareOpClass 
                    expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)]
                 [op #'op.symbol])
     #'(not (op expr1 expr2)))]

  [((~literal Expr) (~and (~or "no" "some" "lone" "one" "two" "set")
                          op)
                    expr1:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [op (datum->syntax #'op (string->symbol (syntax->datum #'op)))])
     #'(op expr1))]

  [((~literal Expr) "#" expr1:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)])
     #'(card expr1))]

  [((~literal Expr) expr1:ExprClass "+" expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)])
     #'(+ expr1 expr2))]

  [((~literal Expr) expr1:ExprClass "-" expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)])
     #'(- expr1 expr2))]

  [((~literal Expr) expr1:ExprClass "++" expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)])
     #'(++ expr1 expr2))]

  [((~literal Expr) expr1:ExprClass "&" expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)])
     #'(& expr1 expr2))]

  [((~literal Expr) expr1:ExprClass op:ArrowOpClass expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)])
     #'(-> expr1 expr2))]

  [((~literal Expr) expr1:ExprClass ":>" expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)])
     #'(:> expr1 expr2))]

  [((~literal Expr) expr1:ExprClass "<:" expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)])
     #'(<: expr1 expr2))]

  [((~literal Expr) "[" exprs:ExprListClass "]")
   #'(raise (format "Unimplemented ~a" exprs))]

  [((~literal Expr) expr1:ExprClass "." expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)])
     #'(join expr1 expr2))]

  [((~literal Expr) expr1:ExprClass "[" exprs:ExprListClass "]")
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [(exprs ...) (datum->syntax #f (map my-expand (syntax->list #'(exprs.exprs ...))))])
     #'(expr1 exprs ...))]

  [((~literal Expr) "~" expr1:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)])
     #'(~ expr1))]

  [((~literal Expr) "^" expr1:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)])
     #'(^ expr1))]

  [((~literal Expr) "*" expr1:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)])
     #'(* expr1))]

  [((~literal Expr) const:ConstClass)
   #'const.translate]

  [((~literal Expr) name:QualNameClass)
   #'name.name]

  [((~literal Expr) "this")
   #'this]

  [((~literal Expr) "{" decls:DeclListClass bob:BlockOrBarClass "}")
   #'(set decls.translate bob.exprs)]

  [((~literal Expr) block:BlockClass)
   (my-expand #'block)]

  [((~literal Expr) sexpr:SexprClass)
   #'(read sexpr)]

  )

(define-simple-macro (Expr1 stx ...) (Expr stx ...))
(define-simple-macro (Expr2 stx ...) (Expr stx ...))
(define-simple-macro (Expr3 stx ...) (Expr stx ...))
(define-simple-macro (Expr4 stx ...) (Expr stx ...))
(define-simple-macro (Expr5 stx ...) (Expr stx ...))
(define-simple-macro (Expr6 stx ...) (Expr stx ...))
(define-simple-macro (Expr7 stx ...) (Expr stx ...))
(define-simple-macro (Expr8 stx ...) (Expr stx ...))
(define-simple-macro (Expr9 stx ...) (Expr stx ...))
(define-simple-macro (Expr10 stx ...) (Expr stx ...))
(define-simple-macro (Expr11 stx ...) (Expr stx ...))
(define-simple-macro (Expr12 stx ...) (Expr stx ...))
(define-simple-macro (Expr13 stx ...) (Expr stx ...))
(define-simple-macro (Expr14 stx ...) (Expr stx ...))
(define-simple-macro (Expr15 stx ...) (Expr stx ...))
(define-simple-macro (Expr16 stx ...) (Expr stx ...))
(define-simple-macro (Expr17 stx ...) (Expr stx ...))





; Transition System Stuff to be implemented
; StateDecl : STATE-TOK /LEFT-SQUARE-TOK QualName /RIGHT-SQUARE-TOK 
;     (QualName DOT-TOK)? Name ParaDecls? Block
; TransitionDecl : TRANSITION-TOK /LEFT-SQUARE-TOK QualName /RIGHT-SQUARE-TOK 
;     (QualName DOT-TOK)? Name ParaDecls? Block
; TraceDecl : TRACE-TOK Parameters
;     (QualName DOT-TOK)? Name ParaDecls? (/COLON-TOK Expr)? Block
; LeftAngle : LT-TOK | LEFT-TRIANGLE-TOK
; RightAngle: GT-TOK | RIGHT-TRIANGLE-TOK


; Other things (to be implemented?)
; NumberList : Number
;            | Number /COMMA-TOK @NumberList

; BreakDecl : /FACT-TOK /BREAK-TOK? Expr /COLON-TOK @NameList
;           | /BREAK-TOK Expr /COLON-TOK @NameList
; InstanceDecl : INSTANCE-TOK
; QueryDecl : @Name /COLON-TOK ArrowExpr /EQ-TOK Expr
