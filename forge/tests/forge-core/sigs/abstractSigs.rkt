#lang forge/core

(sig Abstract #:abstract)
(sig Extension1 #:extends Abstract)
(sig Extension2 #:extends Abstract)

(check abstractEnforced 
       #:preds [(= Abstract (+ Extension1 Extension2))])
(test extensionsAllowed 
      #:preds [(some Extension1)] 
      sat)
(check emptyExtensionsEmptyAbstract 
       #:preds [(=> (no (+ Extension1 Extension2)) (no Abstract))])


(sig Unextended #:abstract)

(test unextendedCanPopulate 
      #:preds [(some Unextended)]
      sat)
