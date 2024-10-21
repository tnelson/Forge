#lang forge/core

(set-option! 'verbose 0)

(define Abstract (make-sig 'Abstract #:abstract #t))
(define Extension1 (make-sig 'Extension1 #:extends Abstract))
(define Extension2 (make-sig 'Extension2 #:extends Abstract))

(make-test #:name 'abstractEnforced 
           #:preds (list (= Abstract (+ Extension1 Extension2)))
           #:sigs (list Abstract Extension1 Extension2)
           #:expect 'checked)
(make-test #:name 'extensionsAllowed 
           #:preds (list (some Extension1))
           #:sigs (list Abstract Extension1 Extension2)
           #:expect 'sat)
(make-test #:name 'emptyExtensionsEmptyAbstract 
           #:preds (list (=> (no (+ Extension1 Extension2)) (no Abstract)))
           #:sigs (list Abstract Extension1 Extension2)
           #:expect 'checked)

