#lang forge/core

(set-option! 'verbose 0)

(define Abstract (make-sig 'Abstract #:abstract #t))
(define Extension1 (make-sig 'Extension1 #:extends Abstract))
(define Extension2 (make-sig 'Extension2 #:extends Abstract))

(test abstractEnforced 
      #:preds [(= Abstract (+ Extension1 Extension2))]
      #:expect theorem)
(test extensionsAllowed 
      #:preds [(some Extension1)] 
      #:expect sat)
(test emptyExtensionsEmptyAbstract 
      #:preds [(=> (no (+ Extension1 Extension2)) (no Abstract))]
      #:expect theorem)

(define Unextended (make-sig 'Unextended #:abstract #t))

(test unextendedCanPopulate 
      #:preds [(some Unextended)]
      #:expect sat)


