#lang racket

(require (except-in forge/lang/expander 
                    ParagraphClass TestDeclClass TestBlockClass TestExpectDeclClass ExampleDeclClass
                    AlloyModule TestDecl TestExpectDecl ExampleDecl
                    test example))
(require (only-in forge/check-ex-spec/library test example))
(require syntax/parse/define
         (for-syntax syntax/parse/define))

(provide define define-values values 
         raise format if filter
         length displayln
         dynamic-require quote list printf 
         parameterize lambda 
         for/list for in-naturals
         unless when)

(provide (all-from-out forge/lang/expander)
         (for-syntax ParagraphClass TestDeclClass TestBlockClass TestExpectDeclClass ExampleDeclClass)
         AlloyModule TestDecl TestExpectDecl ExampleDecl
         test example)

(begin-for-syntax

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
    (pattern decl:InstDeclClass)
    (pattern decl:ExampleDeclClass))

  ; TestDecl : (Name /COLON-TOK)? Parameters? (QualName | Block)? Scope? (/FOR-TOK Bounds)? /IS-TOK (SAT-TOK | UNSAT-TOK)
  (define-syntax-class TestDeclClass
    (pattern ((~literal TestDecl)
              (~optional name:NameClass)
              (~optional parameters:ParametersClass)
              (~optional (~or pred-name:QualNameClass
                              pred-block:BlockClass))
              (~optional scope:ScopeClass)
              (~optional bounds:BoundsClass)
              (~or "sat" "unsat" "theorem"))))

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

  (define-syntax-class ExampleDeclClass
    (pattern ((~literal ExampleDecl)
              (~optional name:NameClass)
              pred:ExprClass
              bounds:BoundsClass))))



(define-syntax (AlloyModule stx)
  (syntax-parse stx
    [((~literal AlloyModule) (~optional module-decl:ModuleDeclClass)
                             (~seq import:ImportClass ...)
                             (~seq paragraph:ParagraphClass ...))
     (syntax/loc stx (begin
     (~? module-decl)
     import ...
     paragraph ...))]
    [((~literal AlloyModule) (~seq eval-decl:EvalDeclClass ...))
     (syntax/loc stx (raise "Evaluating in #lang forge not yet implemented."))]))

; TestDecl : (Name /COLON-TOK)? Parameters? (QualName | Block)? Scope? (/FOR-TOK Bounds)? /IS-TOK (SAT-TOK | UNSAT-TOK)
(define-syntax (TestDecl stx)
  (syntax-parse stx
  [((~literal TestDecl) (~optional name:NameClass)
                        (~optional parameters:ParametersClass)
                        (~optional (~or pred:QualNameClass
                                        preds:BlockClass))
                        (~optional scope:ScopeClass)
                        (~optional bounds:BoundsClass)
                        (~and expected (~or "sat" "unsat" "theorem")))
   (with-syntax ([name #'(~? name.name temporary-name)]
                 [preds (my-expand #'(~? pred.name preds))]
                 [expected (datum->syntax #'expected
                                          (string->symbol (syntax->datum #'expected)))])
     (syntax/loc stx (begin
       (test name (~? (~@ #:preds [preds]))
                  (~? (~@ #:scope scope.translate))
                  (~? (~@ #:bounds bounds.translate))
                  #:expect expected))))]))

; TestExpectDecl : TEST-TOK? EXPECT-TOK Name? TestBlock
(define-syntax (TestExpectDecl stx)
  (syntax-parse stx
  [((~literal TestExpectDecl) (~optional (~and "test" test-tok))
                              "expect" 
                              (~optional name:NameClass)
                              block:TestBlockClass)
   (if (attribute test-tok)
       (syntax/loc stx (list block.test-decls ...))
       (syntax/loc stx (list)))]))

(define-syntax-parser ExampleDecl
  [((~literal ExampleDecl) (~optional name:NameClass)
                           pred:ExprClass
                           bounds:BoundsClass)
   #`(list (example (~? name.name unnamed-example) 
              pred
              #,@#'bounds.translate))])