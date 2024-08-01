#lang racket/base

; The #lang forge reader produces a module referencing this one as its module-path:
; https://docs.racket-lang.org/reference/module.html

(require syntax/parse/define racket/stxparam
         (for-syntax racket/base syntax/parse racket/syntax syntax/parse/define racket/function
                     syntax/srcloc racket/match racket/list                     
                     (only-in racket/path file-name-from-path))
         syntax/srcloc
         ; Needed because the abstract-tok definition below requires phase 2
         (for-syntax (for-syntax racket/base)))
                 
(require (only-in racket empty? first)
         (prefix-in @ (only-in racket +)))
(require forge/sigs)
(require forge/choose-lang-specific)
(require (only-in forge/lang/ast raise-forge-error))

(provide isSeqOf seqFirst seqLast indsOf idxOf lastIdxOf elems inds isEmpty hasDups reachable)
(provide #%module-begin)
(provide #%top #%app #%datum #%top-interaction)

(provide require provide all-defined-out except-out prefix-in only-in
         module+ submod)
(provide forge:nsa define-namespace-anchor)
(provide (all-from-out forge/sigs))
(provide (all-defined-out))
(begin-for-syntax (provide (all-defined-out)))

(define-syntax-parameter current-forge-context #f)

(begin-for-syntax
  (define (forge-context=? x)
    (define ctx (syntax-parameter-value #'current-forge-context))
    (if (pair? x)
      (memq ctx x)
      (eq? ctx x)))

  (define-syntax-parser make-token
    [(make-token token-class:id token-string:str token-symbol)
     #'(define-syntax-class token-class
         (pattern token-string
           #:attr symbol #'token-symbol))])

  (make-token abstract-tok "abstract" #:abstract))

; Used because we sometimes need to evaluate "inside-out" while expanding
; macros "outside-in". The list of symbols are the "stop locations".
; This, if we ever add more macros, they need to be added here (unless
; they can be safely expanded as just themselves?)

; TODO: why is lack of "prime" not breaking something? Is this only ops meant
; to be used with [] or that have >1 argument?

(define-for-syntax (my-expand stx)
  (define core-funcs-and-macros
    (map (lambda (s) (datum->syntax stx s stx))
         '(^ * ~ + - & join
           -> => implies ! not and or && || ifte iff <=>
           = in ni != !in !ni is
           no some one lone all set two
           int< int> int= int>= int<=
           add subtract multiply divide sign abs remainder
           card sum sing succ max min sum-quant
           node/int/constant 
           let)))

  (define result (local-expand stx 'expression core-funcs-and-macros))
  result)

(begin-for-syntax
  ; AlloyModule : ModuleDecl? Import* Paragraph*
  ;             | EvalDecl*
  ; (define-syntax-class AlloyModuleClass
  ;   (pattern ((~datum AlloyModule)
  ;             (~optional module-decl:ModuleDeclClass)
  ;             (~seq import:ImportClass ...)
  ;             (~seq paragraph:ParagraphClass ...)))
  ;   (pattern ((~datum AlloyModule)
  ;             (~seq eval-decl:EvalDeclClass ...))))


  ; Header stuff

  ; ModuleDecl : /MODULE-TOK QualName (LEFT-SQUARE-TOK NameList RIGHT-SQUARE-TOK)?
  (define-syntax-class ModuleDeclClass
    (pattern ((~datum ModuleDecl)
              module-name:QualNameClass
              (~optional (~seq "[" other-names:NameListClass "]")))))

  ; Import : OPEN-TOK QualName (LEFT-SQUARE-TOK QualNameList RIGHT-SQUARE-TOK)? (AS-TOK Name)?
  (define-syntax-class ImportClass
    (pattern ((~datum Import)
              import-name:QualNameClass
              (~optional (~seq "[" other-names:QualNameListClass "]"))
              (~optional (~seq "as" as-name:NameClass))))
    (pattern ((~datum Import)
              file-path:str
              (~optional (~seq "as" as-name:NameClass)))))


  ; Main Decls

  ; EvalDecl : EVAL-TOK Expr
  (define-syntax-class EvalDeclClass
    (pattern ((~datum EvalDecl)
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
    (pattern decl:PropertyDeclClass)
    (pattern decl:QuantifiedPropertyDeclClass)
    (pattern decl:SatisfiabilityDeclClass)
    (pattern decl:TestSuiteDeclClass)
    (pattern decl:SexprDeclClass)
    ; (pattern decl:BreakDeclClass)
    ; (pattern decl:InstanceDeclClass)
    ; (pattern decl:QueryDeclClass)
    ; (pattern decl:StateDeclClass)
    ; (pattern decl:TransitionDeclClass)
    (pattern decl:RelDeclClass)
    (pattern decl:OptionDeclClass)
    (pattern decl:InstDeclClass)
    (pattern decl:ExampleDeclClass))
    ; (pattern decl:TraceDeclClass))

  ;Used for Electrum stuff
  ;Used to declare var Sigs and Relations (so they can change over time)
  (define-syntax-class VarKeywordClass (pattern "var")) 

  ; SigDecl : VAR-TOK? ABSTRACT-TOK? Mult? /SIG-TOK NameList SigExt? /LEFT-CURLY-TOK ArrowDeclList? /RIGHT-CURLY-TOK Block?
  (define-syntax-class SigDeclClass
    (pattern ((~datum SigDecl)
              (~optional isv:VarKeywordClass #:defaults ([isv #'#f]))
              (~optional abstract:abstract-tok)
              (~optional mult:MultClass)
              sig-names:NameListClass
              ;when extending with in is implemented,
              ;if "sig A in B extends C" is allowed,
              ;check if this allows multiple SigExtClasses / how to do that if not
              ;note the parser currently does not allow that
              (~optional extends:SigExtClass)
              (~optional relation-decls:ArrowDeclListClass)
              (~optional block:BlockClass))))

  ; SigExt : EXTENDS-TOK QualName 
  ;        | IN-TOK QualName (PLUS-TOK QualName)*
  (define-syntax-class SigExtClass
    (pattern ((~datum SigExt)
              "extends"
              name:QualNameClass)
      #:attr symbol (syntax/loc this-syntax #:extends)
      #:attr value (syntax/loc this-syntax name.name))
    (pattern ((~datum SigExt)
              "in" 
              name:QualNameClass
              (~seq (~seq "+" names:QualNameClass) ...))
      #:attr symbol (syntax/loc this-syntax #:in)
      #:attr value (syntax/loc this-syntax (raise "Extending with in not yet implemented."))))

  ; Mult : LONE-TOK | SOME-TOK | ONE-TOK | TWO-TOK
  (define-syntax-class MultClass
    (pattern ((~datum Mult) "lone") #:attr symbol (syntax/loc this-syntax #:lone))
    (pattern ((~datum Mult) "some") #:attr symbol (syntax/loc this-syntax #:some))
    (pattern ((~datum Mult) "one") #:attr symbol (syntax/loc this-syntax #:one))
    (pattern ((~datum Mult) "two") #:attr symbol (syntax/loc this-syntax #:two)))

  ; ArrowMult : used for field etc. declarations; the symbol attribute references a breaker
  (define-syntax-class ArrowMultClass
    (pattern ((~datum ArrowMult) "lone") #:attr symbol (syntax/loc this-syntax pfunc))
    (pattern ((~datum ArrowMult) "set") #:attr symbol (syntax/loc this-syntax default))
    (pattern ((~datum ArrowMult) "one") #:attr symbol (syntax/loc this-syntax func))
    (pattern ((~datum ArrowMult) "func") #:attr symbol (syntax/loc this-syntax func))
    (pattern ((~datum ArrowMult) "pfunc") #:attr symbol (syntax/loc this-syntax pfunc))
    (pattern ((~datum ArrowMult) "two") #:attr symbol (syntax/loc this-syntax
                                                        (raise "relation arity two not implemented"))))

  ; HelperMult : used for helper fun/pred definitions; the symbol attribute references a symbol
  (define-syntax-class HelperMultClass
    (pattern ((~datum HelperMult) "lone") #:attr symbol (syntax/loc this-syntax (quote lone)))
    (pattern ((~datum HelperMult) "set") #:attr symbol (syntax/loc this-syntax (quote set)))
    (pattern ((~datum HelperMult) "one") #:attr symbol (syntax/loc this-syntax (quote one)))
    (pattern ((~datum HelperMult) "func") #:attr symbol (syntax/loc this-syntax (quote func)))
    (pattern ((~datum HelperMult) "pfunc") #:attr symbol (syntax/loc this-syntax (quote pfunc))))

  
  ; Declaration of variables with shared expr, shared optional multiplicity
  ; The enclosing context is responsible for checking for valid multiplicities   
  (define-syntax-class DeclClass
    (pattern ((~or (~datum ParaDecl) (~datum QuantDecl))
              names-c:NameListClass              
              (~optional mult:HelperMultClass #:defaults ([mult #'#f]))
              expr:ExprClass)
      ; Assign Alloy-convention defaults: set if arity >1, one otherwise
      #:attr translate (with-syntax ([expr #'expr] 
                                     [mult (if (syntax->datum #'mult)
                                               ; Don't quote this; will be (ArrowDeclMult ...)
                                               #'mult
                                               #'(if (> (node/expr-arity expr) 1)
                                                     'set
                                                     'one))]) 
                         (syntax/loc this-syntax ((names-c.names expr mult) ...)))
      #:attr names (syntax/loc this-syntax (names-c.names ...))))
  
  ; Declaration of a comma-delimited list of variable declarations with expr and optional multiplicity
  ; DeclList : Decl
  ;          | Decl /COMMA-TOK @DeclList
  (define-syntax-class DeclListClass
    (pattern ((~or (~datum ParaDeclList) (~datum QuantDeclList))
              decls:DeclClass ...)      
      #:attr translate (datum->syntax #'(decls ...) 
                                      (apply append 
                                             (map syntax->list 
                                                  (syntax->list #'(decls.translate ...)))))))

  ; Arrow-style declaration, used in (e.g.) field definitions within sigs
  ; ArrowDecl : DISJ-TOK? NameList /COLON-TOK DISJ-TOK? ArrowMult ArrowExpr
  (define-syntax-class ArrowDeclClass
    (pattern ((~datum ArrowDecl)
              ;(~optional "disj")
              (~optional isv:VarKeywordClass #:defaults ([isv #'#f])) ; electrum
              name-list:NameListClass
              ;(~optional "disj") 
              mult-class:ArrowMultClass
              type-list:ArrowExprClass)
      #:attr names #'(name-list.names ...)
      #:attr types #'type-list.names
      #:attr mult #'mult-class.symbol
      #:attr is-var #'isv))

  ; ArrowDeclList : ArrowDecl
  ;               | ArrowDecl /COMMA-TOK @ArrowDeclList
  (define-syntax-class ArrowDeclListClass
    (pattern ((~datum ArrowDeclList)
              arrow-decl:ArrowDeclClass ...)))

  ; ArrowExpr : QualName
  ;           | QualName /ARROW-TOK @ArrowExpr
  (define-syntax-class ArrowExprClass
    (pattern ((~datum ArrowExpr)
              name-list:QualNameClass ...)
      #:attr names #'(name-list.name ...)))

  ; FactDecl : FACT-TOK Name? Block
  (define-syntax-class FactDeclClass
    (pattern ((~datum FactDecl)
              "fact" 
              (~optional name:NameClass)
              block:BlockClass)))

  (define-syntax-class PredTypeClass
    #:datum-literals (PredType)
    #:attributes (kw)
    (pattern (PredType "wheat")
      #:attr kw #'#:wheat))

  ; PredDecl : /PRED-TOK (QualName DOT-TOK)? Name ParaDecls? Block
  (define-syntax-class PredDeclClass
    #:description "predicate declaration"
    (pattern ((~datum PredDecl)
              (~optional _:PredTypeClass)
              (~optional (~seq prefix:QualNameClass "."))
              name:NameClass
              (~optional decls:ParaDeclsClass)
              block:BlockClass)))

  ; FunDecl : /FUN-TOK (QualName DOT-TOK)? Name ParaDecls? /COLON-TOK Expr Block
  (define-syntax-class FunDeclClass
    #:description "helper function declaration"
    (pattern ((~datum FunDecl)
              (~optional (~seq prefix:QualNameClass "."))
              name:NameClass
              (~optional decls:ParaDeclsClass)
              ; An optional multiplicity and required expression
              (~optional output-mult:HelperMultClass)
              output-expr:ExprClass
              body:ExprClass)))

  ;; Used only for function and predicate definitions
  (define-syntax-class ParaDeclsClass
    (pattern ((~datum ParaDecls)
              (~seq decls:DeclClass ...))
      ; The `variables` attribute removes the expression of each, returning only var
      #:attr variables (datum->syntax #'(decls ...)
                                      (apply append (map (compose (curry map car )
                                                                  (curry map syntax->list )
                                                                  syntax->list) 
                                                         (syntax->list #'(decls.translate ...)))))
      ; The `pairs` attribute retains both variable and expression (var expr mult?)
      #:attr pairs (datum->syntax #'(decls ...)
                                      (apply append (map (compose (curry map syntax->list )
                                                                  syntax->list) 
                                                         (syntax->list #'(decls.translate ...)))))
      ; For the moment, the `translate` attribute does as `variables`
      #:attr translate (datum->syntax #'(decls ...)
                                      (apply append (map (compose (curry map car )
                                                                  (curry map syntax->list )
                                                                  syntax->list) 
                                                         (syntax->list #'(decls.translate ...)))))))

  ; AssertDecl : /ASSERT-TOK Name? Block
  (define-syntax-class AssertDeclClass
    (pattern ((~datum AssertDecl)
              (~optional name:NameClass)
              block:BlockClass)))

  ; CmdDecl :  (Name /COLON-TOK)? (RUN-TOK | CHECK-TOK) Parameters? (QualName | Block)? Scope? (/FOR-TOK Bounds)?
  (define-syntax-class CmdDeclClass
    (pattern ((~datum CmdDecl)
              (~optional name:NameClass)
              (~or "run" "check")
              (~optional parameters:ParametersClass)
              (~optional (~or pred-name:QualNameClass
                              pred-block:BlockClass))
              (~optional scope:ScopeClass)
              (~optional bounds:BoundsClass))))

  ; TestDecl : (Name /COLON-TOK)? Parameters? (QualName | Block)? Scope? (/FOR-TOK Bounds)? /IS-TOK (SAT-TOK | UNSAT-TOK)
  (define-syntax-class TestDeclClass
    (pattern ((~datum TestDecl)
              (~optional name:NameClass)
              (~optional parameters:ParametersClass)
              (~optional (~or pred-name:QualNameClass
                              pred-block:BlockClass))
              (~optional scope:ScopeClass)
              (~optional bounds:BoundsClass)
              (~or "sat" "unsat" "theorem" "forge_error"))))

  ; TestBlock : /LEFT-CURLY-TOK TestDecl* /RIGHT-CURLY-TOK
  (define-syntax-class TestBlockClass
    (pattern ((~datum TestBlock)
              test-decls:TestDeclClass ...)))

  ; TestExpectDecl : TEST-TOK? EXPECT-TOK Name? TestBlock
  (define-syntax-class TestExpectDeclClass
    (pattern ((~datum TestExpectDecl)
              (~optional "test")
              "expect"
              (~optional name:NameClass)
              test-block:TestBlockClass)))

  (define-syntax-class TestConstructClass
    (pattern decl:ExampleDeclClass)
    (pattern decl:TestExpectDeclClass)
    (pattern decl:PropertyDeclClass)
    (pattern decl:QuantifiedPropertyDeclClass)
    (pattern decl:SatisfiabilityDeclClass))

  (define-syntax-class PropertyDeclClass
    #:attributes (prop-name pred-name constraint-type scope bounds)
    (pattern ((~datum PropertyDecl)      
              -prop-name:NameClass
              (~and (~or "sufficient" "necessary") ct)
              -pred-name:NameClass
              (~optional -scope:ScopeClass)
              (~optional -bounds:BoundsClass))
      #:with prop-name #'-prop-name.name
      #:with pred-name #'-pred-name.name
      #:with constraint-type (string->symbol (syntax-e #'ct))
      #:with scope (if (attribute -scope) #'-scope.translate #'())
      #:with bounds (if (attribute -bounds) #'-bounds.translate #'())))

  (define-syntax-class QuantifiedPropertyDeclClass
    #:attributes (quant-decls disj prop-name prop-exprs pred-name pred-exprs constraint-type scope bounds)
    (pattern ((~datum QuantifiedPropertyDecl)
              (~optional (~and "disj" -disj))
              -quant-decls:DeclListClass
              -prop-name:NameClass
              (~optional -prop-exprs:ExprListClass)
              (~and (~or "sufficient" "necessary") ct)
              -pred-name:NameClass
              (~optional -pred-exprs:ExprListClass)
              (~optional -scope:ScopeClass)
              (~optional -bounds:BoundsClass))
      #:with disj (if (attribute -disj) (string->symbol (syntax-e #'-disj)) '())
      #:with quant-decls #'-quant-decls.translate
      #:with prop-name #'-prop-name.name
      #:with pred-name #'-pred-name.name
      #:with pred-exprs  (if (attribute -pred-exprs) #'(-pred-exprs.exprs ...) #'()) 
      #:with prop-exprs (if (attribute -prop-exprs)  #'(-prop-exprs.exprs ...) #'())
      #:with constraint-type (string->symbol (syntax-e #'ct))
      #:with scope (if (attribute -scope) #'-scope.translate #'())
      #:with bounds (if (attribute -bounds) #'-bounds.translate #'())))

(define-syntax-class SatisfiabilityDeclClass
  #:attributes (pred-name expected scope bounds)
  (pattern ((~datum SatisfiabilityDecl)
            -pred-name:NameClass
            (~and (~or "sat" "unsat" "forge_error") ct)
            (~optional -scope:ScopeClass)
            (~optional -bounds:BoundsClass))
    #:with pred-name #'-pred-name.name
    #:with expected (string->symbol (syntax-e #'ct))
    #:with scope (if (attribute -scope) #'-scope.translate #'())
    #:with bounds (if (attribute -bounds) #'-bounds.translate #'())))


  (define-syntax-class TestSuiteDeclClass
    #:attributes (pred-name (test-constructs 1))
    (pattern ((~datum TestSuiteDecl)
              -pred-name:NameClass
              test-constructs:TestConstructClass ...)
      #:with pred-name #'-pred-name.name))

  (define-syntax-class ExampleDeclClass
    (pattern ((~datum ExampleDecl)
              (~optional name:NameClass)
              pred:ExprClass
              bounds:BoundsClass)))

  ; Scope : /FOR-TOK Number (/BUT-TOK @TypescopeList)? 
  ;       | /FOR-TOK @TypescopeList
  ; TypescopeList : Typescope
  ;               | Typescope /COMMA-TOK @TypescopeList
  (define-syntax-class ScopeClass
    (pattern ((~datum Scope)
              (~optional default:NumberClass)
              (~seq typescope:TypescopeClass ...))
      #:attr translate #'(typescope.translate ...)))

  ; Typescope : EXACTLY-TOK? Number QualName
  (define-syntax-class TypescopeClass
    (pattern ((~datum Typescope)
              (~optional (~and "exactly" exactly))
              num:NumberClass
              name:QualNameClass)
      #:attr translate (if (attribute exactly)
                           #'(name.name num.value num.value)
                           #'(name.name 0 num.value))))

  ; OptionDecl : /OPTION-TOK QualName (QualName | FILE-PATH-TOK | Number)
  (define-syntax-class OptionDeclClass
    #:attributes (n v)
    (pattern ((~datum OptionDecl) name:QualNameClass value:QualNameClass)
             #:attr n #'name.name
             #:attr v #'value.name)
    (pattern ((~datum OptionDecl) name:QualNameClass value:str)
             #:attr n #'name.name
             #:attr v #'value)
    (pattern ((~datum OptionDecl) name:QualNameClass value:NumberClass)
             #:attr n #'name.name
             #:attr v #'value.value)
    (pattern ((~datum OptionDecl) name:QualNameClass "-" value:NumberClass)
             #:attr n #'name.name
             #:attr v (quasisyntax #,(* -1 (syntax->datum #'value.value)))))

 
  ; Block : /LEFT-CURLY-TOK Expr* /RIGHT-CURLY-TOK
  (define-syntax-class BlockClass
    (pattern ((~datum Block)
              exprs:ExprClass ...)))

  ; Name : IDENTIFIER-TOK
  (define-syntax-class NameClass
    (pattern ((~datum Name)
              name:id)))

  ; NameList : @Name
  ;          | @Name /COMMA-TOK @NameList
  (define-syntax-class NameListClass
    (pattern ((~datum NameList)
              names:id ...)))

  ; QualName : (THIS-TOK /SLASH-TOK)? (@Name /SLASH-TOK)* @Name | INT-TOK | SUM-TOK
  (define-syntax-class QualNameClass
    #:attributes (name)
    (pattern ((~datum QualName)
              (~optional "this") ; TODO, allow more complex qualnames
              (~seq prefixes:id ...)
              raw-name:id)
      #:attr name #'raw-name)
    (pattern ((~datum QualName) "Int")
      #:attr name #'(raise "Int as qualname?"))
    (pattern ((~datum QualName) "sum")
      #:attr name #'(raise "sum as qualname?")))

  ; QualNameList : @QualName
  ;              | @QualName /COMMA-TOK @QualNameList
  (define-syntax-class QualNameListClass
    (pattern ((~datum QualNameList)
              (~or (~seq (~optional "this")
                         (~seq prefixes:id ...)
                         name:id)
                   "Int"
                   "sum") ...)))

  ; Number : NUM-CONST-TOK
  (define-syntax-class NumberClass
    (pattern ((~datum Number) n)
      #:attr value #'n))

  ; SexprDecl : Sexpr
  (define-syntax-class SexprDeclClass
    (pattern ((~datum SexprDecl) exp:SexprClass)))

  ; Sexpr : SEXPR-TOK
  (define-syntax-class SexprClass
    (pattern ((~datum Sexpr) exp)))

  ; InstDecl : /INST-TOK Name Bounds Scope?
  (define-syntax-class InstDeclClass
    (pattern ((~datum InstDecl)
              name:NameClass
              bounds:BoundsClass
              (~optional scope:ScopeClass))))

  ; RelDecl : ArrowDecl
  (define-syntax-class RelDeclClass
    (pattern ((~datum RelDecl) decl:ArrowDeclClass)))

  ; Parameters : /LeftAngle @QualNameList /RightAngle 
  (define-syntax-class ParametersClass
    (pattern ((~datum Parameters)
              name:QualNameClass ...)))

  (define-syntax-class BoundsClass
     #:description "bounds declarations"
    (pattern ((~datum Bounds)
              ;(~optional "exactly")
              bounds:BoundClass ...)
      #:attr translate (begin
                         ;(printf "Bounds case 1: ~a~n" (build-source-location this-syntax))
                         (datum->syntax #'(bounds ...)
                                        ; Note: syntax->list keeps the syntax objects within, along with
                                        ; their context/srcloc. (The syntax/loc here doesn't seem to be
                                        ; needed, since it's templating.)
                                        (syntax->list #'(bounds.translate ...))
                                        ;(syntax->list (syntax/loc this-syntax (bounds.translate ...)))
                                        (build-source-location this-syntax))))
    ; direct use in the example, test, run, etc.
    ; `example` macro splices the result in. Hence, not #'(Expr name).
    ; `test` macro does not splice. 
    (pattern ((~datum Bounds)
              (~optional "exactly")
              name:QualNameClass)
      #:attr translate (begin (datum->syntax #'name
                                             (list #'name.name)
                                             #'name))))

  (define-syntax-class QualNameOrAtomOrAtomizedNumberClass
     #:description "name, atom name, or number"
    (pattern name:QualNameClass
      #:attr translate (syntax/loc this-syntax (Expr name)))
    (pattern ((~datum AtomNameOrNumber) "`" name:NameClass)
      #:attr translate (syntax/loc this-syntax (Expr "`" name)))
    ; Negative numbers are now handled in the lexer, so that "- 1" is not transformed to "-1"
    (pattern ((~datum AtomNameOrNumber) num:NumberClass)
      #:attr translate
      (quasisyntax/loc this-syntax (Expr #,(syntax/loc this-syntax (Const num))))))
  
  (define-syntax-class BoundLHSClass
    #:description "left-hand-side of a bind declaration"
    ; No join, relation name only on LHS
    (pattern ((~datum BoundLHS) target:QualNameClass)
      #:attr translate (syntax/loc this-syntax (Expr target)))
    ; Join, atom name dotted with field name
    (pattern ((~datum BoundLHS) ((~datum AtomNameOrNumber) "`" atom:NameClass)                                
                                field:QualNameClass)
      #:attr translate (quasisyntax/loc this-syntax
                         (Expr
                          #,(syntax/loc this-syntax (Expr "`" atom)) "."
                          #,(syntax/loc this-syntax (Expr field))))))
  
  (define-syntax-class BoundClass
     #:description "bind declaration"
    ; tuple bounds:  LHS =/ni/in UNION
    (pattern ((~datum Bound)  
              lhs:BoundLHSClass
              op:CompareOpClass
              rhs:BindRHSUnionClass)
      #:attr translate (begin
                         (quasisyntax/loc this-syntax (Expr lhs.translate op rhs.translate))))

    ; cardinality bound (single relation): #LHS = N
    (pattern ((~datum Bound)  ; or backquote name
              ((~datum BoundLHS) "#" target:QualNameClass)
              op:CompareOpClass
              rhs:BindRHSUnionClass) 
      #:attr translate (with-syntax* ([tgt (syntax/loc #'target (Expr target))]
                                     [left-subexpr (syntax/loc #'target (Expr "#" tgt))])
                         (syntax/loc this-syntax (Expr left-subexpr op rhs.translate))))
    
    ; "no" bound: relation LHS and piecewise LHS
    (pattern ((~datum Bound) (~datum "no") 
                             ((~datum BoundLHS) target:QualNameClass)) 
    #:attr translate (with-syntax ([tgt (syntax/loc #'target (Expr target))])
                         (syntax/loc this-syntax (Expr "no" tgt))))
    (pattern ((~datum Bound) (~datum "no")
                             ((~datum BoundLHS)
                              ((~datum AtomNameOrNumber) "`" atom:NameClass)
                              field:QualNameClass))
      #:attr translate (quasisyntax/loc this-syntax
                         (Expr "no"
                               #,(quasisyntax/loc this-syntax
                                   (Expr
                                    #,(quasisyntax/loc this-syntax (Expr "`" atom)) "."
                                    #,(quasisyntax/loc this-syntax (Expr field)))))))

    ; identifier: re-use of `inst` defined
    (pattern ((~datum Bound)  
              name:QualNameClass)
      #:attr translate (syntax/loc this-syntax (Expr name))))

  (define-syntax-class BindRHSUnionClass
     #:description "union in right-hand-side of a bind declaration"
    (pattern ((~datum BindRHSUnion)
              tup:BindRHSProductClass)
      #:attr translate (syntax/loc this-syntax tup.translate))

    ; "union -> product"
    (pattern ((~datum BindRHSUnion)
              tups:BindRHSUnionClass
              tup:BindRHSProductClass)
      #:attr translate (syntax/loc this-syntax (Expr tups.translate "+" tup.translate)))
    (pattern ((~datum BindRHSUnion)
              tups1:BindRHSUnionClass
              tups2:BindRHSUnionClass) 
      #:attr translate (syntax/loc this-syntax (Expr tups1.translate "+" tups2.translate))))
  
  (define-syntax-class BindRHSProductClass
     #:description "product in right-hand-side of a bind declaration"
    (pattern ((~datum BindRHSProduct)
              union:BindRHSUnionClass) 
      #:attr translate (syntax/loc this-syntax union.translate))
    (pattern ((~datum BindRHSProduct)
              atom:QualNameOrAtomOrAtomizedNumberClass) 
      #:attr translate (syntax/loc this-syntax atom.translate))

    ; "product -> atom" or "atom -> product"
    (pattern ((~datum BindRHSProduct)
              tup:BindRHSProductClass
              atom:QualNameOrAtomOrAtomizedNumberClass) 
      #:attr translate (syntax/loc this-syntax (Expr tup.translate (ArrowOp "->") atom.translate)))
    (pattern ((~datum BindRHSProduct)
              atom:QualNameOrAtomOrAtomizedNumberClass
              tup:BindRHSProductClass) 
      #:attr translate (syntax/loc this-syntax (Expr atom.translate (ArrowOp "->") tup.translate)))

    ; "union -> product" or "product -> union"
    (pattern ((~datum BindRHSProduct)
              union1:BindRHSUnionClass
              product2:BindRHSProductClass) 
      #:attr translate (syntax/loc this-syntax (Expr union1.translate (ArrowOp "->") product2.translate)))
    (pattern ((~datum BindRHSProduct)
              product1:BindRHSProductClass
              union2:BindRHSUnionClass) 
      #:attr translate (syntax/loc this-syntax (Expr product1.translate (ArrowOp "->") union2.translate))))
  
  ; EXPRESSIONS

  ; Const : NONE-TOK | UNIV-TOK | IDEN-TOK
  ;       | MINUS-TOK? Number 
  (define-syntax-class ConstClass
    #:attributes (translate)
    (pattern ((~datum Const) "none")
      #:attr translate (syntax/loc this-syntax none))
    (pattern ((~datum Const) "univ")
      #:attr translate (syntax/loc this-syntax univ))
    (pattern ((~datum Const) "iden")
      #:attr translate (syntax/loc this-syntax iden))
    (pattern ((~datum Const) n:NumberClass)
      #:attr translate (syntax/loc this-syntax (int n.value)))
    ; Negative numbers are now handled in the lexer, so that "- 1" is not translated to "-1"
    ; Thus, this is now an error as it would indicate that the lexer got this "-" token
    ; separately from the number.
    (pattern ((~datum Const) "-" n:NumberClass)
      #:attr translate
      ; This production (ConstClass) should not permit "detatched" negatives
      (with-syntax ([loc (build-source-location (syntax/loc this-syntax #'1))])
        (quasisyntax/loc this-syntax
          (raise-forge-error
           #:msg (format "Negative numbers must not have blank space between the minus sign and the digits (~a)." n.value)
           #:context loc)))
      ;(quasisyntax/loc this-syntax (int #,(* -1 (syntax->datum #'n.value)))))
    ))
  
  ; ArrowOp : (@Mult | SET-TOK)? ARROW-TOK (@Mult | SET-TOK)?
  ;         | STAR-TOK
  (define-syntax-class ArrowOpClass
    (pattern ((~datum ArrowOp)
              (~optional (~or "lone" "some" "one" "two" "set"))
              "->"
              (~optional (~or "lone" "some" "one" "two" "set"))))
    (pattern ((~datum ArrowOp) "*")))

  ; CompareOp : IN-TOK | EQ-TOK | LT-TOK | GT-TOK | LEQ-TOK | GEQ-TOK | EQUIV-TOK | IS-TOK | NI-TOK
  (define-syntax-class CompareOpClass
    (pattern ((~datum CompareOp)
              (~and op
                    (~or "in" "=" "<" ">" "<=" ">="
                         "is" "ni")))
      #:attr symbol (datum->syntax #'op
                                   (op-symbol-to-operator (string->symbol (syntax->datum #'op)))
                                   #'op)))

  ; We don't overload (e.g.) <, but we don't want users to need to write (e.g.) "int<". 
  (define (op-symbol-to-operator sym)
    (cond [(equal? sym '<)  'int<]
          [(equal? sym '>)  'int>]
          [(equal? sym '<=) 'int<=]
          [(equal? sym '>=) 'int>=]
          [else sym]))
  
  ; LetDecl : @Name /EQ-TOK Expr
  (define-syntax-class LetDeclClass
    (pattern ((~datum LetDecl)
              name:id
              exp:ExprClass)
      #:attr translate (with-syntax ([exp #'exp]) 
                         #'(name exp))))

  ; LetDeclList : LetDecl
  ;             | LetDecl /COMMA-TOK @LetDeclList
  (define-syntax-class LetDeclListClass
    (pattern ((~datum LetDeclList)
              decls:LetDeclClass ...)
      #:attr translate #'(decls.translate ...)))

  ; BlockOrBar : Block | BAR-TOK Expr
  (define-syntax-class BlockOrBarClass
      #:attributes (exprs)
    (pattern ((~datum BlockOrBar) block:BlockClass)
      #:attr exprs #'block)
    (pattern ((~datum BlockOrBar) "|" exp:ExprClass)
      #:attr exprs #'exp))

  ; Quant : ALL-TOK | NO-TOK | SUM-TOK | @Mult
  (define-syntax-class QuantClass
    (pattern ((~datum Quant) (~and q (~or "all" "no" "lone"
                                            "some" "one" "two")))
      #:attr symbol (datum->syntax #'q
                                   (string->symbol (syntax->datum #'q))
                                   #'q))
    (pattern ((~datum Quant) (~literal sum))
      #:attr symbol (syntax/loc #'q sum-quant)))

  (define-syntax-class ExprClass
    (pattern ((~or (~datum Expr) (~datum Expr1) (~datum Expr1.5) (~datum Expr2) (~datum Expr3)
                   (~datum Expr4) (~datum Expr4.5) (~datum Expr5) (~datum Expr6) (~datum Expr7) (~datum Expr7.5)
                   (~datum Expr8) (~datum Expr9) (~datum Expr10) (~datum Expr11)
                   (~datum Expr12) (~datum Expr13) (~datum Expr14) (~datum Expr15)
                   (~datum Expr16) (~datum Expr17))
             _ ...)))

  ; ExprList : Expr
  ;          | Expr /COMMA-TOK @ExprList
  (define-syntax-class ExprListClass
    (pattern ((~datum ExprList)
              exprs:ExprClass ...))))



; AlloyModule : ModuleDecl? Import* Paragraph*
;             | EvalDecl*
(define-syntax (AlloyModule stx)    
  (syntax-parse stx
    [((~datum AlloyModule) (~optional module-decl:ModuleDeclClass)
                             (~seq import:ImportClass ...)
                             (~seq paragraph:ParagraphClass ...))
     (syntax/loc stx
       (begin
         (~? module-decl)
         import ...
         paragraph ...))]
    [((~datum AlloyModule) ((~datum EvalDecl) "eval" expr:ExprClass))
     (syntax/loc stx expr)]
    [((~datum AlloyModule) ((~datum EvalDecl) "eval" expr:ExprClass) ...+)
     (syntax/loc stx (raise "Can't eval multiple expressions."))]))

; ModuleDecl : /MODULE-TOK QualName (LEFT-SQUARE-TOK NameList RIGHT-SQUARE-TOK)?
(define-syntax (ModuleDecl stx)
  (syntax-parse stx 
    [((~datum ModuleDecl) module-name:QualNameClass
                            (~optional (~seq "[" other-names:NameListClass "]")))
     (syntax/loc stx (raise "ModuleDecl not yet implemented."))]))


; Import : OPEN-TOK QualName (LEFT-SQUARE-TOK QualNameList RIGHT-SQUARE-TOK)? (AS-TOK Name)?
(define-syntax (Import stx)
  (syntax-parse stx
      [((~datum Import) file-path:str
                          (~optional (~seq "as" as-name:NameClass)))
       (syntax/loc stx (begin
           (~? (require (prefix-in as-name.name file-path))
               (require file-path))))]
    [((~datum Import) import-name:QualNameClass
                        (~optional (~seq "[" other-names:QualNameListClass "]"))
                        (~optional (~seq "as" as-name:NameClass)))
     (syntax/loc stx (begin
         (raise (format "Importing packages not yet implemented: ~a." 'import-name))
         (~? (raise (format "Bracketed import not yet implemented. ~a" 'other-names)))
         (~? (raise (format "Importing as not yet implemented. ~a" 'as-name)))))]))
  
; SigDecl : VAR-TOK? ABSTRACT-TOK? Mult? /SIG-TOK NameList SigExt? /LEFT-CURLY-TOK ArrowDeclList? /RIGHT-CURLY-TOK Block?
(define-syntax (SigDecl stx)
  (syntax-parse stx
    [((~datum SigDecl) (~optional isv:VarKeywordClass #:defaults ([isv #'#f]))
                         (~optional abstract:abstract-tok)
                         (~optional mult:MultClass)
                         sig-names:NameListClass
                         (~optional extends:SigExtClass)
                         (~optional block:BlockClass))
     (quasisyntax/loc stx (begin
       (~? (raise (format "Sig block not yet implemented: ~a" 'block)))
       #,@(for/list ([sig-name (syntax-e #'(sig-names.names ...))])
            (with-syntax ([sig-name-p0 sig-name])
              (syntax/loc sig-name
                (sig (#:lang (get-check-lang)) sig-name-p0 (~? mult.symbol)
                     (~? abstract.symbol)
                     (~? (~@ #:is-var isv))
                     (~? (~@ extends.symbol extends.value))))))))]

    [((~datum SigDecl) (~optional isv:VarKeywordClass #:defaults ([isv #'#f]))
                         (~optional abstract:abstract-tok)
                         (~optional mult:MultClass)
                         sig-names:NameListClass
                         (~optional extends:SigExtClass)
                         ((~datum ArrowDeclList) arrow-decl:ArrowDeclClass ...)
                         (~optional block:BlockClass))
     (quasisyntax/loc stx (begin
       (~? (raise (format "Sig block not yet implemented: ~a" 'block)))
       #,@(for/list ([sig-name (syntax-e #'(sig-names.names ...))])
            (with-syntax ([sig-name-p0 sig-name])
              (syntax/loc sig-name
                (sig (#:lang (get-check-lang)) sig-name-p0 (~? mult.symbol)
                     (~? abstract.symbol)
                     (~? (~@ #:is-var isv))
                     (~? (~@ extends.symbol extends.value))))))
       #,@(apply append
            ; for each sig in this declaration (e.g., "sig A, B, C { ...")
            (for/list ([sig-name (syntax->list #'(sig-names.names ...))]
                       #:when #t
                       [relation-names (syntax->list #'(arrow-decl.names ...))]
                       [relation-types (syntax->list #'(arrow-decl.types ...))]
                       [relation-is-var (syntax->list #'(arrow-decl.is-var ...))]
                       [relation-mult (syntax->list #'((~? arrow-decl.mult default) ...))])
              ; for each field declared
              (for/list ([relation-name-p1 (syntax->list relation-names)])
                (with-syntax ([relation-name relation-name-p1]
                              [relation-types (datum->syntax relation-types 
                                                             (cons sig-name ;(syntax->datum sig-name)
                                                                   (syntax->list relation-types))
                                                             relation-types)]                          
                              [relation-mult relation-mult]
                              [is-var relation-is-var])
                      (syntax/loc relation-name-p1 (relation (#:lang (get-check-lang)) relation-name relation-types #:is relation-mult #:is-var is-var))))))))]))
   
; RelDecl : ArrowDecl
(define-syntax (RelDecl stx)
  (syntax-parse stx
  [((~datum RelDecl) arrow-decl:ArrowDeclClass)
   (quasisyntax/loc stx (begin
   #,@(for/list ([name (syntax->list #'arrow-decl.names)])
        (with-syntax ([name name])
          (syntax/loc stx (relation (#:lang (get-check-lang)) name arrow-decl.types))))))]))

; FactDecl : FACT-TOK Name? Block
(define-syntax (FactDecl stx)
  (syntax-parse stx
  [((~datum FactDecl) _ ...)
   (syntax/loc stx (raise "Facts are not allowed in #lang forge."))]))

; PredDecl : /PRED-TOK (QualName DOT-TOK)? Name ParaDecls? Block
(define-syntax (PredDecl stx)
  (syntax-parse stx
  [((~datum PredDecl) (~optional pt:PredTypeClass)
                        (~optional (~seq prefix:QualNameClass "."))
                        name:NameClass
                        block:BlockClass)
   (with-syntax ([block #'block])
     (quasisyntax/loc stx (begin
       (~? (raise (format "Prefixes not allowed: ~a" 'prefix)))
       ; preserve stx location in Racket *sub*expression
       #,(syntax/loc stx (pred (~? pt.kw) (#:lang (get-check-lang)) name.name block)))))]

  [((~datum PredDecl) (~optional pt:PredTypeClass)
                        (~optional (~seq prefix:QualNameClass "."))
                        name:NameClass
                        decls:ParaDeclsClass
                        block:BlockClass)
   (with-syntax ([decl (datum->syntax #'name
                                      (cons (syntax->datum #'name.name)
                                            (syntax->list #'decls.pairs))
                                      #'name)]
                 [block #'block])
     (quasisyntax/loc stx (begin
       (~? (raise (format "Prefixes not allowed: ~a" 'prefix)))
       ; preserve stx location in Racket *sub*expression
       #,(syntax/loc stx (pred (~? pt.kw) (#:lang (get-check-lang)) decl block)))))]))

; FunDecl : /FUN-TOK (QualName DOT-TOK)? Name ParaDecls? /COLON-TOK Expr Block
(define-syntax (FunDecl stx)
  (syntax-parse stx
  ; TODO: output type declared is currently being lost
  [((~datum FunDecl) (~optional (~seq prefix:QualNameClass "."))
                       name:NameClass
                       (~optional output-mult:HelperMultClass)
                       output-expr:ExprClass
                       body:ExprClass)
   (with-syntax ([body #'body])
     (syntax/loc stx (begin
       (~? (raise (format "Prefixes not allowed: ~a" 'prefix)))
       (const name.name body))))]

  [((~datum FunDecl) (~optional (~seq prefix:QualNameClass "."))
                       name:NameClass
                       decls:ParaDeclsClass
                       (~optional output-mult:HelperMultClass #:defaults ([output-mult #'#f]))
                       output-expr:ExprClass
                       body:ExprClass)
   (with-syntax ([decl (datum->syntax #'name (cons (syntax->datum #'name.name)
                                                   (syntax->list #'decls.pairs))
                                      #'name)]
                 ; Parser "Expr" includes both expressions and formulas. Thus, disambiguate
                 ;   (e.g.) the "Expr" `one univ` into true expr and multiplicity in the expander;
                 ;   It's not the job of the `fun` macro to handle this. (Same for `pred`, etc.)
                 [output (if (syntax->datum #'output-mult)
                             #'(output-expr (~? output-mult.symbol))
                             #'(output-expr))]
                 [body #'body])
     (syntax/loc stx (begin
       (~? (raise (format "Prefixes not allowed: ~a" 'prefix)))
       (fun decl body #:codomain output))))]))

; AssertDecl : /ASSERT-TOK Name? Block
(define-syntax (AssertDecl stx)
  (syntax-parse stx
  [((~datum AssertDecl) _ ...)
   (syntax/loc stx (raise "Assertions not yet implemented."))]))

(define-for-syntax make-temporary-name
  (let ((name-counter (box 1)))
    (lambda (stx)
      (define curr-num (unbox name-counter))
      (set-box! name-counter (+ 1 curr-num))
      (define source_disambiguator
        (cond [(and (source-location-source stx)
                    (or (path-string? (source-location-source stx))
                        (path-for-some-system? (source-location-source stx)))
                    (file-name-from-path (source-location-source stx)))
               (path-replace-extension (file-name-from-path (source-location-source stx)) #"")]
              [(symbol? (source-location-source stx)) (symbol->string (source-location-source stx))]
              [else "unknown"]))
      (string->symbol (format "temporary-name_~a_~a" source_disambiguator curr-num)))))

; CmdDecl :  (Name /COLON-TOK)? (RUN-TOK | CHECK-TOK) Parameters? (QualName | Block)? Scope? (/FOR-TOK Bounds)?
(define-syntax (CmdDecl stx)
  (syntax-parse stx
  [((~datum CmdDecl) (~optional name:NameClass)
                       (~and cmd-type (~or "run" "check"))
                       (~optional parameters:ParametersClass)
                       (~optional (~or pred:QualNameClass
                                       preds:BlockClass))
                       (~optional scope:ScopeClass)
                       (~optional bounds:BoundsClass))
   (with-syntax ([cmd-type (datum->syntax #'cmd-type
                                          (string->symbol (syntax->datum #'cmd-type))
                                          #'cmd-type)]
                 [name #`(~? name.name #,(make-temporary-name stx))]
                 [preds #'(~? pred.name preds)])
    #`(begin
       #,(syntax/loc stx (cmd-type name (~? (~@ #:preds [preds]))
                      (~? (~@ #:scope scope.translate))
                      (~? (~@ #:bounds bounds.translate))))
       #,(syntax/loc stx (display name))))]))

; TestDecl : (Name /COLON-TOK)? Parameters? (QualName | Block)? Scope? (/FOR-TOK Bounds)? /IS-TOK (SAT-TOK | UNSAT-TOK)
(define-syntax (TestDecl stx)
  ; This stx object currently has the location of the enclosing TestBlock
  ; ... unless there is a name provided? (already, at this point, even before the parse below)
  ;(printf "expander for TestDecl: ~a~n" stx)
  (syntax-parse stx
  [((~datum TestDecl) (~optional name:NameClass)
                        (~optional parameters:ParametersClass)
                        (~optional (~or pred:QualNameClass
                                        preds:BlockClass))
                        (~optional scope:ScopeClass)
                        (~optional bounds:BoundsClass)
                        (~and expected (~or "sat" "unsat" "theorem" "forge_error")))
   (with-syntax ([name #`(~? name.name #,(make-temporary-name stx))]
                 [preds #'(~? pred.name preds)]
                 [expected (datum->syntax #'expected
                                          (string->symbol (syntax->datum #'expected))
                                          #'expected)])
     (syntax/loc stx
       (test name (~? (~@ #:preds [preds]))
                  (~? (~@ #:scope scope.translate))
                  (~? (~@ #:bounds bounds.translate))
                  #:expect expected)))]))

; TestExpectDecl : TEST-TOK? EXPECT-TOK Name? TestBlock
(define-syntax (TestExpectDecl stx)
  ;(printf "expander for TestExpectDecl: ~a~n" stx)
  (syntax-parse stx
  [((~datum TestExpectDecl) (~optional (~and "test" test-tok))
                              "expect" 
                              (~optional name:NameClass)
                              block:TestBlockClass)
   (if (attribute test-tok)
       (syntax/loc stx (begin block.test-decls ...))
       (syntax/loc stx (begin)))]))


(define-syntax (PropertyDecl stx)
  (syntax-parse stx
  [pwd:PropertyDeclClass 
   #:with imp_total (if (eq? (syntax-e #'pwd.constraint-type) 'sufficient)
                        (syntax/loc stx (implies pwd.prop-name pwd.pred-name))  ;; p => q : p is a sufficient condition for q 
                        (syntax/loc stx (implies pwd.pred-name pwd.prop-name))) ;; q => p : p is a necessary condition for q
   #:with test_name (format-id stx "Assertion_~a_is_~a_for_~a" #'pwd.prop-name #'pwd.constraint-type #'pwd.pred-name)
   (syntax/loc stx
      (test
        test_name
        #:preds [imp_total]
        #:scope pwd.scope
        #:bounds pwd.bounds
        #:expect theorem ))]))


(define-syntax (QuantifiedPropertyDecl stx)
  (syntax-parse stx
    [qpd:QuantifiedPropertyDeclClass  
    ;;;#:do [(printf "QuantifiedPropertyDeclClass.PropExprs: ~a~n" (syntax->datum #'qpd.prop-exprs))]
     (with-syntax* 
        ( [(exp-pred-exprs ...) (datum->syntax stx (cons (syntax->datum #'qpd.pred-name) (syntax->datum #'qpd.pred-exprs)) stx)]
          [(exp-prop-exprs ...) (datum->syntax stx (cons (syntax->datum #'qpd.prop-name) (syntax->datum #'qpd.prop-exprs)) stx)]
        
          [prop-consolidated (if (equal? (syntax->datum #'qpd.prop-exprs) '()) 
                                (syntax/loc stx qpd.prop-name)
                                (syntax/loc stx (exp-prop-exprs ...)))]
          [pred-consolidated (if (equal? (syntax->datum #'qpd.pred-exprs) '()) 
                                (syntax/loc stx qpd.pred-name)
                                (syntax/loc stx (exp-pred-exprs ...)))]
                                
          [test_name (format-id stx "~a__Assertion_All_~a_is_~a_for_~a" (make-temporary-name stx) #'qpd.prop-name #'qpd.constraint-type #'qpd.pred-name)]
          ;;; This is ugly, I'm sure there are better ways to write this
          [imp_total
            (if (eq? (syntax-e #'qpd.disj) 'disj)
                (if (eq? (syntax-e #'qpd.constraint-type) 'sufficient)
                    (syntax/loc stx (all #:disj  qpd.quant-decls (implies prop-consolidated pred-consolidated)))
                    (syntax/loc stx (all #:disj  qpd.quant-decls (implies pred-consolidated prop-consolidated))))
                (if (eq? (syntax-e #'qpd.constraint-type) 'sufficient)
                    (syntax/loc stx (all  qpd.quant-decls (implies prop-consolidated pred-consolidated)))
                    (syntax/loc stx (all  qpd.quant-decls (implies pred-consolidated prop-consolidated)))))])
     (syntax/loc stx
       (test
         test_name
         #:preds [imp_total]
         #:scope qpd.scope
         #:bounds qpd.bounds
         #:expect theorem )))]))


(define-syntax (SatisfiabilityDecl stx)
  (syntax-parse stx
  [sd:SatisfiabilityDeclClass 
   #:with test_name (format-id stx "Assertion_~a_is_~a_~a" #'sd.pred-name #'sd.expected (make-temporary-name stx))
   (syntax/loc stx
      (test
        test_name
        #:preds [sd.pred-name]
        #:scope sd.scope
        #:bounds sd.bounds
        #:expect sd.expected ))]))

;; Quick and dirty static check to ensure a test
;; references a predicate.

;; A potential improvement is to break apart test-expect
;; blocks and examine each test.

(define-for-syntax (ensure-target-ref target-pred ex)
  (define tp (syntax-e target-pred))
  (let ([ex-as-datum (syntax->datum ex)])
    (unless 
      (memq tp (flatten ex-as-datum))
      (eprintf  "Warning: ~a ~a:~a Test does not reference ~a.\n" 
        (syntax-source ex) (syntax-line ex) (syntax-column ex)  tp))))


(define-syntax (TestSuiteDecl stx)
  (syntax-parse stx
  [tsd:TestSuiteDeclClass 
   
    ;; Static checks on test blocks go here.
   (for ([tc (syntax->list #'(tsd.test-constructs ...))])
        (ensure-target-ref #'tsd.pred-name tc))

   (syntax/loc stx
    (begin tsd.test-constructs ...))]))


(define-syntax (ExampleDecl stx)
  (syntax-parse stx
  [((~datum ExampleDecl) (~optional name:NameClass)
                           pred:ExprClass
                           bounds:BoundsClass)
   (quasisyntax/loc stx
     (example (~? name.name unnamed-example)
       pred
       (syntax-parameterize ([current-forge-context 'example])
         (list #,@(syntax/loc stx bounds.translate)))))]))

; Macro definitions for syntax that might survive into sigs.rkt macros
(define-syntax (Const stx)
  (syntax-parse stx
    [c:ConstClass
     (syntax/loc this-syntax c.translate)]))
(define-syntax (HelperMult stx)
  (syntax-parse stx
    [am:HelperMultClass
     (syntax/loc this-syntax am.symbol)]))


; OptionDecl : /OPTION-TOK QualName (QualName | FILE-PATH-TOK | Number)
(define-syntax (OptionDecl stx)
  (syntax-parse stx
  [dec:OptionDeclClass
   ; Some options contain file paths. By saving the path of the .frg file at a point
   ; we still have it, we can let (e.g.) option solver work even if racket is invoked
   ; from outside the folder containing the .frg file.
   (quasisyntax/loc stx (set-option! 'dec.n 'dec.v #:original-path #,(current-load-relative-directory)))
   ]))

; InstDecl : /INST-TOK Name Bounds Scope?
(define-syntax (InstDecl stx)
  (syntax-parse stx
  [((~datum InstDecl)
              name:NameClass
              bounds:BoundsClass
              (~optional scope:ScopeClass))
   (quasisyntax/loc stx
     (begin
       (~? (raise (format "Scope not implemented for InstDecl ~a" 'scope)))
       (inst name.name
             (syntax-parameterize ([current-forge-context 'inst])
               (list #,@(syntax/loc stx bounds.translate))))))]))

(define (disambiguate-block xs #:stx [stx #f])
  (cond [(empty? xs) 
         ; {} always means the formula true
         true]
         ; Body of a helper function: one expression
        [(and (equal? 1 (length xs)) (node/expr? (first xs)))
         (first xs)]
         ; Body of a predicate: any number of formulas
        [(andmap node/formula? xs)
         (define info (nodeinfo (build-source-location stx) 'checklangplaceholder #f))
         (&&/info info xs)]
         ; body of a helper function that produces an int-expression: one int-expression
        [(and (equal? 1 (length xs)) (node/int? (first xs)))
         (first xs)]
        [else
         ; First, check and see whether any individual member of xs is a procedure. If so, it's likely
         ; the model is using a helper function or predicate incorrectly.
         (for ([x xs]
               [i (build-list 5 (lambda (x) (@+ x 1)))])
           (when (procedure? x)
             (raise-forge-error
              #:msg (format "Element ~a of this block was an ill-formed predicate or helper function call. Check that the number of arguments given matches the declaration." i)
              #:context stx)))
         ; Otherwise, give a general error message.
         (raise-forge-error
          #:msg (format "Ill-formed block: expected either one expression or any number of formulas; got ~a" xs)
          #:context stx)]))

; Block : /LEFT-CURLY-TOK Expr* /RIGHT-CURLY-TOK
(define-syntax (Block stx)
  (syntax-parse stx
    [((~datum Block) exprs:ExprClass ...)
     (with-syntax ([(exprs ...) (syntax->list #'(exprs ...))])
       (quasisyntax/loc stx
         (disambiguate-block (list exprs ...)
                             #:stx #,(build-source-location stx))))]))

(define-syntax (Expr stx)
  ;(printf "Debug: Expr: ~a~n" stx)
  (syntax-parse stx
  [((~datum Expr) "let" decls:LetDeclListClass bob:BlockOrBarClass)
   (syntax/loc stx (let decls.translate bob.exprs))]

  [((~datum Expr) "bind" decls:LetDeclListClass bob:BlockOrBarClass)
   (syntax/loc stx (raise "bind not implemented."))]

  ; Quantifier
  [((~datum Expr) q:QuantClass decls:DeclListClass bob:BlockOrBarClass)                       
   (syntax/loc stx (q.symbol decls.translate bob.exprs))] ; stx, not #'q

  ; Quantifier with disj
  [((~datum Expr) q:QuantClass "disj" decls:DeclListClass bob:BlockOrBarClass)
   (syntax/loc stx (q.symbol #:disj decls.translate bob.exprs))] ; stx, not #'q

  [((~datum Expr) expr1:ExprClass (~or "or" "||") expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)])
     (syntax/loc stx (|| expr1 expr2)))]

    ; exclusive OR
    [((~datum Expr) expr1:ExprClass "xor" expr2:ExprClass)
     (with-syntax ([expr1 (my-expand #'expr1)]
                   [expr2 (my-expand #'expr2)])
       (syntax/loc stx (xor expr1 expr2)))]

    
  [((~datum Expr) expr1:ExprClass (~or "iff" "<=>") expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)])
     (syntax/loc stx (iff (#:lang (get-check-lang)) expr1 expr2)))]

  [((~datum Expr) expr1:ExprClass (~or "implies" "=>") expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)])     
     (syntax/loc stx (implies (#:lang (get-check-lang)) expr1 expr2)))]

  [((~datum Expr) expr1:ExprClass (~or "implies" "=>") expr2:ExprClass
                                    "else" expr3:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)]
                 [expr3 (my-expand #'expr3)])
     (syntax/loc stx (ifte expr1 expr2 expr3)))]

  [((~datum Expr) expr1:ExprClass (~or "and" "&&") expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)])
     (syntax/loc stx (&& expr1 expr2)))]

  [((~datum Expr) expr1:ExprClass (~or "releases") expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)])
     (syntax/loc stx (releases expr1 expr2)))]
  [((~datum Expr) expr1:ExprClass (~or "until") expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)])
     (syntax/loc stx (until expr1 expr2)))]

  [((~datum Expr) expr1:ExprClass (~or "since") expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)])
     (syntax/loc stx (since expr1 expr2)))]
  [((~datum Expr) expr1:ExprClass (~or "triggered") expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)])
     (syntax/loc stx (triggered expr1 expr2)))]

    
  [((~datum Expr) (~or "!" "not") expr1:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)])
     (syntax/loc stx (! (#:lang (get-check-lang)) expr1)))]

  [((~datum Expr) (~or "always") expr1:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)])
     (syntax/loc stx (always expr1)))]
  [((~datum Expr) (~or "eventually") expr1:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)])
     (syntax/loc stx (eventually expr1)))]
  [((~datum Expr) (~or "next_state") expr1:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)])
       (syntax/loc stx (next_state expr1)))]

  [((~datum Expr) (~or "historically") expr1:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)])
     (syntax/loc stx (historically expr1)))]
  [((~datum Expr) (~or "once") expr1:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)])
     (syntax/loc stx (once expr1)))]
  [((~datum Expr) (~or "prev_state") expr1:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)])
       (syntax/loc stx (prev_state expr1)))]       
    
  [((~datum Expr) expr1:ExprClass op:CompareOpClass expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)]
                 [op #'op.symbol])
     (syntax/loc stx (op (#:lang (get-check-lang)) expr1 expr2)))]

  [((~datum Expr) expr1:ExprClass 
                    (~or "!" "not") op:CompareOpClass 
                    expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)]
                 [op #'op.symbol])
     ; Need to preserve srcloc in both the "not" and the "op" nodes
     (quasisyntax/loc stx (! #,(syntax/loc stx (op (#:lang (get-check-lang)) expr1 expr2)))))]

  ; Multiplicity form
  [((~datum Expr) (~and (~or "no" "some" "lone" "one" "two" "set")
                          op)
                    expr1:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [op (datum->syntax #'op (string->symbol (syntax->datum #'op)) #'op)])
     (syntax/loc stx (op (#:lang (get-check-lang)) expr1)))]

  [((~datum Expr) "#" expr1:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)])
     (syntax/loc stx (card (#:lang (get-check-lang)) expr1)))]

  ; Semantic priming as in Electrum
  [((~datum Expr) expr1:ExprClass "'")
   (with-syntax ([expr1 (my-expand #'expr1)])
     (syntax/loc stx (prime (#:lang (get-check-lang)) expr1)))]

  [((~datum Expr) expr1:ExprClass "+" expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)]
                 [check-lang (if (forge-context=? '(inst example))
                               #''forge
                               #'(get-check-lang))])
     (syntax/loc stx (+ (#:lang check-lang) expr1 expr2)))]

  [((~datum Expr) expr1:ExprClass "-" expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)])
     (syntax/loc stx (- (#:lang (get-check-lang)) expr1 expr2)))]

  [((~datum Expr) expr1:ExprClass "++" expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)])
     (syntax/loc stx (++ (#:lang (get-check-lang)) expr1 expr2)))]

  [((~datum Expr) expr1:ExprClass "&" expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)])
     (syntax/loc stx (& (#:lang (get-check-lang)) expr1 expr2)))]

  [((~datum Expr) expr1:ExprClass op:ArrowOpClass expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)]
                 [check-lang (if (forge-context=? '(inst example))
                               #''forge
                               #'(get-check-lang))])
     (syntax/loc stx (-> (#:lang check-lang) expr1 expr2)))]

  [((~datum Expr) expr1:ExprClass ":>" expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)])
     (syntax/loc stx (:> (#:lang (get-check-lang)) expr1 expr2)))]

  [((~datum Expr) expr1:ExprClass "<:" expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)])
     (syntax/loc stx (<: (#:lang (get-check-lang)) expr1 expr2)))]

  [((~datum Expr) "[" exprs:ExprListClass "]")
   (syntax/loc stx (raise (format "Unimplemented ~a" exprs)))]

  [((~datum Expr) expr1:ExprClass "." expr2:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)]
                 [expr2 (my-expand #'expr2)])
     (syntax/loc stx (join (#:lang (get-check-lang)) expr1 expr2)))]

  [((~datum Expr) name:NameClass "[" exprs:ExprListClass "]")
   (with-syntax ([name #'name.name]
                 [(exprs ...) (datum->syntax #f (map my-expand (syntax->list #'(exprs.exprs ...))))])
     (syntax/loc stx (name exprs ...)))]

    
  [((~datum Expr) expr1:ExprClass "[" exprs:ExprListClass "]")
     (with-syntax ([expr1 (my-expand #'expr1)]
                   [(exprs ...) (datum->syntax #f (map my-expand (syntax->list #'(exprs.exprs ...))))])
       (syntax/loc stx (expr1 exprs ...)))]

  [((~datum Expr) "~" expr1:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)])
     (syntax/loc stx (~ (#:lang (get-check-lang)) expr1)))]

  [((~datum Expr) "^" expr1:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)])
     (syntax/loc stx (^ (#:lang (get-check-lang)) expr1)))]

  [((~datum Expr) "*" expr1:ExprClass)
   (with-syntax ([expr1 (my-expand #'expr1)])
     (syntax/loc stx (* (#:lang (get-check-lang)) expr1)))]

  [((~datum Expr) const:ConstClass)   
   (syntax/loc stx const.translate)]

  ; If the name references an AST node, retain use-site location information
  ; rather than the location of the declaration.
  ; If it references a _macro_, such as `add`, `max`, etc. *don't* wrap it!

  [((~datum Expr) name:QualNameClass)
   (define local-value (syntax-local-value #'name.name (lambda () #f)))
   ;(printf "local-value for ~a: ~a~n" #'name.name local-value)
   (if local-value
       ; if we have a local-value, it's likely a Forge macro or Forge-defined helper procedure
       (syntax/loc stx name.name)
       ; otherwise, it's likely an AST node (to be functionally updated with proper syntax 
       ; location at runtime). Note this is a QualName at compile time, but may be substituted
       ; to something more complex at runtime via pred/fun application.
       (with-syntax ([loc (build-source-location (syntax/loc this-syntax #'1))])
         (syntax/loc stx (correct-id-loc name.name #:loc loc))))]
   
  [((~datum Expr) "this")
   (syntax/loc stx this)]

  [((~datum Expr) "`" name:NameClass)
   (syntax/loc stx (atom 'name.name))]
  
  [((~datum Expr) "{" decls:DeclListClass bob:BlockOrBarClass "}")
   (syntax/loc stx (set (#:lang (get-check-lang)) decls.translate bob.exprs))]

  [((~datum Expr) block:BlockClass)
   (my-expand (syntax/loc stx block))]

    [((~datum Expr) sexpr:SexprClass)
     (syntax/loc stx (read sexpr))]))


; struct-copy would not retain the sub-struct identity and fields, producing just a node
; ditto the struct-update-lib library
; struct-set requires using a special form to create the struct.
; We need to handle Sig and Relation cases, but at runtime the expression might be more
; complex (via substitution from fun/pred application).

; *New* instance of the structure, with the same values but a (possibly) new source loc
(define (correct-id-loc astnode #:loc [loc #f])
  ;(printf "correct-id-loc: ~a; ~a; ~a~n" astnode (node? astnode) loc)
  
  (define new-info (if (node? astnode)
                       (nodeinfo loc (nodeinfo-lang (node-info astnode)) #f)
                       #f))
  (cond [(forge:Sig? astnode)
         (forge:Sig new-info
                    (node/expr-arity astnode) (node/expr/relation-name astnode) (node/expr/relation-typelist-thunk astnode)
                    (node/expr/relation-parent astnode) (node/expr/relation-is-variable astnode) (node/expr/relation-name astnode)
                    (forge:Sig-one astnode) (forge:Sig-lone astnode) (forge:Sig-abstract astnode) (forge:Sig-extends astnode))]
        [(forge:Relation? astnode)
         (forge:Relation new-info
                         (node/expr-arity astnode) (node/expr/relation-name astnode) (node/expr/relation-typelist-thunk astnode)
                         (node/expr/relation-parent astnode) (node/expr/relation-is-variable astnode) (node/expr/relation-name astnode)
                         ;(forge:Relation-name astnode)
                         (forge:Relation-sigs-thunks astnode) (forge:Relation-breaker astnode))]
        [(node/expr/quantifier-var? astnode)
         (node/expr/quantifier-var
          new-info
          (node/expr-arity astnode) (node/expr/quantifier-var-sym astnode) (node/expr/quantifier-var-name astnode))]
        ; Annoyingly, structs aren't polymorphic in the way we need. This is not elegant, but:

        ; join, transpose, +, -, &, ^, *, ->, sing, ', ++
        [(node/expr/op/join? astnode) (rebuild-expr-op node/expr/op/join astnode new-info)]
        [(node/expr/op/~? astnode) (rebuild-expr-op node/expr/op/~ astnode new-info)]
        [(node/expr/op/+? astnode) (rebuild-expr-op node/expr/op/+ astnode new-info)]
        [(node/expr/op/-? astnode) (rebuild-expr-op node/expr/op/- astnode new-info)]
        [(node/expr/op/&? astnode) (rebuild-expr-op node/expr/op/& astnode new-info)]
        [(node/expr/op/^? astnode) (rebuild-expr-op node/expr/op/^ astnode new-info)]
        [(node/expr/op/*? astnode) (rebuild-expr-op node/expr/op/* astnode new-info)]
        [(node/expr/op/->? astnode) (rebuild-expr-op node/expr/op/-> astnode new-info)]
        [(node/expr/op/sing? astnode) (rebuild-expr-op node/expr/op/sing astnode new-info)]
        [(node/expr/op/prime? astnode) (rebuild-expr-op node/expr/op/prime astnode new-info)]
        [(node/expr/op/++? astnode) (rebuild-expr-op node/expr/op/++ astnode new-info)]
        
        [(node/expr/ite? astnode)
         (node/expr/ite new-info (node/expr-arity astnode)
                        (node/expr/ite-condition astnode)
                        (node/expr/ite-thene astnode)
                        (node/expr/ite-elsee astnode))]
        [(node/expr/fun-spacer? astnode)
         (node/expr/fun-spacer new-info (node/expr-arity astnode)
                               (node/expr/fun-spacer-name astnode)
                               (node/expr/fun-spacer-args astnode)
                               (node/expr/fun-spacer-codomain astnode)
                               (node/expr/fun-spacer-expanded astnode))]
        [(node/expr/comprehension? astnode)
         (node/expr/comprehension new-info (node/expr-arity astnode)
                                  (node/expr/comprehension-decls astnode)
                                  (node/expr/comprehension-formula astnode))]
        [(node/expr/constant? astnode)
         (node/expr/constant new-info (node/expr-arity astnode) (node/expr/constant-type astnode))]
        [else astnode]))

(define (rebuild-expr-op constructor astnode new-info)
  (constructor new-info (node/expr-arity astnode) (node/expr/op-children astnode)))

; --------------------------
; these used to be define-simple-macro, but define-simple-macro doesn't
; preserve syntax location, so we copy the definition from the docs except use syntax/loc.

(define-syntax (dsm-keep stx-outer)
  (syntax-case stx-outer ()
    [(_ (macro-id . pattern) pattern-directive ... template)
     (with-syntax ([ellip '...])
       (syntax/loc stx-outer
         (define-syntax (macro-id stx)
           ; Uncomment this if debugging loss of syntax location. Look through the spam for
           ; syntax that doesn't carry a location or that has unexpected location.
           ;(printf "in macro-id ~a: ~a~n" 'macro-id stx)
           (syntax-parse stx
             #:track-literals
             [((~var macro-id id) . pattern) pattern-directive ... (syntax/loc stx template)]))))]))

(dsm-keep (Expr1 stx ...) (Expr stx ...))
(dsm-keep (Expr1.5 stx ...) (Expr stx ...))
(dsm-keep (Expr2 stx ...) (Expr stx ...))
(dsm-keep (Expr3 stx ...) (Expr stx ...))
(dsm-keep (Expr4 stx ...) (Expr stx ...))
(dsm-keep (Expr4.5 stx ...) (Expr stx ...))
(dsm-keep (Expr5 stx ...) (Expr stx ...))
(dsm-keep (Expr6 stx ...) (Expr stx ...))
(dsm-keep (Expr7 stx ...) (Expr stx ...))
(dsm-keep (Expr7.5 stx ...) (Expr stx ...))
(dsm-keep (Expr8 stx ...) (Expr stx ...))
(dsm-keep (Expr9 stx ...) (Expr stx ...))
(dsm-keep (Expr10 stx ...) (Expr stx ...))
(dsm-keep (Expr11 stx ...) (Expr stx ...))
(dsm-keep (Expr12 stx ...) (Expr stx ...))
(dsm-keep (Expr13 stx ...) (Expr stx ...))
(dsm-keep (Expr14 stx ...) (Expr stx ...))
(dsm-keep (Expr15 stx ...) (Expr stx ...))
(dsm-keep (Expr16 stx ...) (Expr stx ...))
(dsm-keep (Expr17 stx ...) (Expr stx ...))





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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper(s) for debugging syntax class nesting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (debug stx)
  (define result
    (syntax-parse stx
      [(_ x:BoundsClass) (syntax/loc this-syntax x.translate)]
      [(_ x:BoundClass) (syntax/loc this-syntax x.translate)]
      [(_ x:BoundLHSClass) (syntax/loc this-syntax x.translate)]
      [(_ x:BindRHSProductClass) (syntax/loc this-syntax x.translate)]
      [(_ x:BindRHSUnionClass) (syntax/loc this-syntax x.translate)]
      [(_ x:QualNameOrAtomOrAtomizedNumberClass) (syntax/loc this-syntax x.translate)]
      [(_ x:NumberClass) (syntax/loc this-syntax x.value)]))
  (printf "result:~a ~n" result)
  #'(void))

;(debug (Bounds (Bound "no" (BoundLHS (QualName Int)))))
