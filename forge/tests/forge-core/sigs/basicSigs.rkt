#lang forge/core

(sig A)
(sig B)

(test singleSig 
      #:preds [(some A)] 
      #:expect sat)
(test doubleSig 
      #:preds [(some A) (some B)] 
      #:expect sat)

(test sigsDisjoint 
      #:preds [(no (& A B))]
      #:expect theorem)

; (test sigsSpanUniv
;       #:preds [(= univ (+ A B Int))]
;       #:expect theorem) ; CURRENTLY BUGGED

