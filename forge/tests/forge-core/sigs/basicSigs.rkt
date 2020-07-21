#lang forge/core

(sig A)
(sig B)

(test singleSig 
      #:preds [(some A)] 
      sat)
(test doubleSig 
      #:preds [(some A) (some B)] 
      sat)

(check sigsDisjoint 
       #:preds [(no (& A B))])

; (check sigsSpanUniv
;        #:preds [(= univ (+ A B Int))]) ; CURRENTLY BUGGED

