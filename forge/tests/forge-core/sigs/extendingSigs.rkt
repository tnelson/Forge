#lang forge/core

(set-option! 'verbose 0)

(sig ToExtend)
(sig Extension1 #:extends ToExtend)
(sig Extension2 #:extends ToExtend)
(sig Extension3 #:extends Extension2)

(test extensionEnforced 
      #:preds [(in (+ Extension1 Extension2) ToExtend)]
      #:expect checked)
(test multipleExtensions 
      #:preds [(&& (some Extension1) (some Extension2))]
      #:expect sat)
(test extensionsDisjoint 
      #:preds [(some (& Extension1 Extension2))]
      #:expect unsat)
(test extensionsNotExhaustive 
      #:preds [(some (- ToExtend (+ Extension1 Extension2)))]
      #:expect sat)
(test doubleExtendingWorks 
      #:preds [(&& (in Extension3 Extension2) (in Extension2 ToExtend))]
      #:expect checked)


(sig Parent)
(sig Child #:extends Parent)

(relation parentRel (Parent Child))
(relation childRel (Child Parent))

(test relationsIntoExtension
      #:preds [(in (join Parent parentRel) Child)]
      #:expect checked)
(test extensionsInheritRelations
      #:preds [(some (join Child parentRel))]
      #:expect sat)
(test parentsDontGetExtensionRelations
      #:preds [(in (join childRel Parent) Child)]
      #:expect checked)
