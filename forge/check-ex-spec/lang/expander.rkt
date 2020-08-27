#lang racket

; (require (for-syntax syntax/parse)
;          (for-syntax (for-syntax syntax/parse)))
; (require syntax/parse/define
;          (for-syntax syntax/parse/define))
; (require (for-syntax (for-syntax racket/base)))
(require (for-syntax syntax/parse))
(require syntax/parse/define)

(require (except-in forge/lang/expander
                    AlloyModule
                    ; AlloyModuleClass
                    ParagraphClass))

(provide (all-from-out forge/lang/expander))
(provide (all-defined-out))

(begin-for-syntax
  (provide (all-defined-out))
  ; (define-syntax-class AlloyModuleClass
  ;   (pattern ((~literal AlloyModule)
  ;             (~optional module-decl:ModuleDeclClass)
  ;             (~seq import:ImportClass ...)
  ;             (~seq paragraph:ParagraphClass ...)))
  ;   (pattern ((~literal AlloyModule)
  ;             (~seq eval-decl:EvalDeclClass ...))))

  (define-syntax-class ParagraphClass
    (pattern decl:SigDeclClass)
    (pattern decl:FactDeclClass)
    (pattern decl:PredDeclClass)
    (pattern decl:FunDeclClass)
    (pattern decl:AssertDeclClass)
    (pattern decl:CmdDeclClass)
    (pattern decl:TestExpectDeclClass)
    (pattern decl:SexprDeclClass)
    (pattern decl:RelDeclClass)
    (pattern decl:OptionDeclClass)
    (pattern decl:InstDeclClass)

    (pattern decl:ExampleDeclClass))

  (define-syntax-class ExampleDeclClass
    (pattern ((~literal ExampleDecl)
              name:NameClass
              bounds:BoundsClass
              pred:ExprClass))))

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

(define-syntax-parser ExampleDecl
  [((~literal ExampleDecl) (~optional name:NameClass)
                           bounds:BoundsClass
                           pred:ExprClass)
   #'(test (~? name.name unnamed-example) 
           #:preds [pred] 
           #:bounds bounds.translate 
           #:expect sat)])

