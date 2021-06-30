#lang forge/core

(set-option! 'verbose 0)

(define ToExtend (make-sig 'ToExtend))
(define Extension1 (make-sig 'Extension1 #:extends ToExtend))
(define Extension2 (make-sig 'Extension2 #:extends ToExtend))
(define Extension3 (make-sig 'Extension3 #:extends Extension2))

(make-test #:name 'extensionEnforced
           #:preds (list (in (+ Extension1 Extension2) ToExtend))
           #:sigs (list ToExtend Extension1 Extension2 Extension3)
           #:expect 'theorem)
(make-test #:name 'multipleExtensions 
           #:preds (list (&& (some Extension1) (some Extension2)))
           #:sigs (list ToExtend Extension1 Extension2 Extension3)
           #:expect 'sat)
(make-test #:name 'extensionsDisjoint 
           #:preds (list (some (& Extension1 Extension2)))
           #:sigs (list ToExtend Extension1 Extension2 Extension3)
           #:expect 'unsat)
(make-test #:name 'extensionsNotExhaustive 
           #:preds (list (some (- ToExtend (+ Extension1 Extension2))))
           #:sigs (list ToExtend Extension1 Extension2 Extension3)
           #:expect 'sat)
(make-test #:name 'doubleExtendingWorks 
           #:preds (list (&& (in Extension3 Extension2) (in Extension2 ToExtend)))
           #:sigs (list ToExtend Extension1 Extension2 Extension3)
           #:expect 'theorem)

(define Parent (make-sig 'Parent))
(define Child (make-sig 'Child #:extends Parent))

(define parentRel (make-relation 'parentRel (list Parent Child)))
(define childRel (make-relation 'childRed (list Child Parent)))

(make-test #:name 'relationsIntoExtension
           #:preds (list (in (join Parent parentRel) Child))
           #:sigs (list ToExtend Extension1 Extension2 Extension3 Parent Child)
           #:relations (list parentRel childRel)
           #:expect 'theorem)
(make-test #:name 'extensionsInheritRelations
           #:preds (list (some (join Child parentRel)))
           #:sigs (list ToExtend Extension1 Extension2 Extension3 Parent Child)
           #:relations (list parentRel childRel)
           #:expect 'sat)
(make-test #:name 'parentsDontGetExtensionRelations
           #:preds (list (in (join childRel Parent) Child))
           #:sigs (list ToExtend Extension1 Extension2 Extension3 Parent Child)
           #:relations (list parentRel childRel)
           #:expect 'theorem)
