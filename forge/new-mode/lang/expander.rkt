#lang racket

(require (for-syntax syntax/parse))
(require syntax/parse/define)

(require (except-in forge/lang/expander
                    AlloyModule
                    ParagraphClass))

(provide (all-from-out forge/lang/expander))
(provide (all-defined-out))

(begin-for-syntax
  (provide (all-defined-out))

  ; Comment out any commands you don't want to allow
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

    ; Add new syntax classes as you define them
    (pattern decl:ExampleDeclClass) ; EXAMPLE
    )

  ; Define syntax class for new command
  ; See forge/lang/expander.rkt for more examples
  (define-syntax-class ExampleDeclClass ; EXAMPLE
    (pattern ((~literal ExampleDecl)
              (~optional name:NameClass)
              pred:ExprClass
              bounds:BoundsClass))))

; You don't need to touch this; it just redefined the AlloyModule
; macro in terms of the new syntax classes you have defined.
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

; Add macros for new paragraphs here.
(define-syntax-parser ExampleDecl ; EXAMPLE
  [((~literal ExampleDecl) (~optional name:NameClass)
                           pred:ExprClass
                           bounds:BoundsClass)
   #'(test (~? name.name unnamed-example) 
           #:preds [pred] 
           #:bounds bounds.translate 
           #:expect sat)])

