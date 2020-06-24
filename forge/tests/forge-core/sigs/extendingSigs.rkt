#lang racket

(require "../../../sigs.rkt")

(sig ToExtend)
(sig Extension1 #:extends ToExtend)
(sig Extension2 #:extends ToExtend)
(sig Extension3 #:extends Extension2)

(check extensionEnforced [(in (+ Extension1 Extension2) ToExtend)])
(test multipleExtensions [(and (some Extension1) (some Extension2))] 'sat)
(test extensionsDisjoint [(some (& Extension1 Extension2))] 'unsat)
(test extensionsNotExhaustive [(some (- ToExtend (+ Extension1 Extension2)))] 'sat)
(check doubleExtendingWorks [(and (in Extension3 Extension2) (in Extension2 ToExtend))])


(sig Parent)
(sig Child #:extends Parent)

(relation parentRel (Parent Child))
(relation childRel (Child Parent))

(check relationsIntoExtension [(in (join Parent parentRel) Child)])
(test extensionsInheritRelations [(some (join Child parentRel))] 'sat)
(check parentsDontGetExtensionRelations [(in (join childRel Parent) Child)])
