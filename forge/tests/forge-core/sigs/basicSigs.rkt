#lang forge/core

(set-option! 'verbose 0)

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
      #:expect checked)

; (test sigsSpanUniv
;       #:preds [(= univ (+ A B Int))]
;       #:expect checked) ; CURRENTLY BUGGED

