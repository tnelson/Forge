#lang forge/core

(set-option! 'verbose 0)

(sig Abstract #:abstract)
(sig Extension1 #:extends Abstract)
(sig Extension2 #:extends Abstract)

(test abstractEnforced 
      #:preds [(= Abstract (+ Extension1 Extension2))]
      #:expect theorem)
(test extensionsAllowed 
      #:preds [(some Extension1)] 
      #:expect sat)
(test emptyExtensionsEmptyAbstract 
      #:preds [(=> (no (+ Extension1 Extension2)) (no Abstract))]
      #:expect theorem)


(sig Unextended #:abstract)

(test unextendedCanPopulate 
      #:preds [(some Unextended)]
      #:expect sat)
