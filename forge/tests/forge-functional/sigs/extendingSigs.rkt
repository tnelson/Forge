#lang forge/core

(set-option! 'verbose 0)

(define ToExtend (make-sig 'ToExtend))
(define Extension1 (make-sig 'Extension1 #:extends ToExtend))
(define Extension2 (make-sig 'Extension2 #:extends ToExtend))
(define Extension3 (make-sig 'Extension3 #:extends Extension2))

(test extensionEnforced
      #:preds [(in (+ Extension1 Extension2) ToExtend)]
      #:expect theorem)
(test multipleExtensions 
      #:preds [(and (some Extension1) (some Extension2))]
      #:expect sat)
(test extensionsDisjoint 
      #:preds [(some (& Extension1 Extension2))]
      #:expect unsat)
(test extensionsNotExhaustive 
      #:preds [(some (- ToExtend (+ Extension1 Extension2)))]
      #:expect sat)
(test doubleExtendingWorks 
      #:preds [(and (in Extension3 Extension2) (in Extension2 ToExtend))]
      #:expect theorem)

(define Parent (make-sig 'Parent))
(define Child (make-sig 'Child #:extends Parent))

(define parentRel (make-relation 'parentRel (list Parent Child)))
(define childRel (make-relation 'childRed (list Child Parent)))

(test relationsIntoExtension
      #:preds [(in (join Parent parentRel) Child)]
      #:expect theorem)
(test extensionsInheritRelations
      #:preds [(some (join Child parentRel))]
      #:expect sat)
(test parentsDontGetExtensionRelations
      #:preds [(in (join childRel Parent) Child)]
      #:expect theorem)
