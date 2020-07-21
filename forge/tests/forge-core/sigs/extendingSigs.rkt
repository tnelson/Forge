#lang forge/core

(sig ToExtend)
(sig Extension1 #:extends ToExtend)
(sig Extension2 #:extends ToExtend)
(sig Extension3 #:extends Extension2)

(check extensionEnforced 
       #:preds [(in (+ Extension1 Extension2) ToExtend)])
(test multipleExtensions 
      #:preds [(and (some Extension1) (some Extension2))]
      sat)
(test extensionsDisjoint 
      #:preds [(some (& Extension1 Extension2))]
      unsat)
(test extensionsNotExhaustive 
      #:preds [(some (- ToExtend (+ Extension1 Extension2)))]
      sat)
(check doubleExtendingWorks 
       #:preds [(and (in Extension3 Extension2) (in Extension2 ToExtend))])


(sig Parent)
(sig Child #:extends Parent)

(relation parentRel (Parent Child))
(relation childRel (Child Parent))

(check relationsIntoExtension
       #:preds [(in (join Parent parentRel) Child)])
(test extensionsInheritRelations
      #:preds [(some (join Child parentRel))]
      sat)
(check parentsDontGetExtensionRelations
       #:preds[(in (join childRel Parent) Child)])
