#lang forge/core

(sig A)
(relation r (A A) #:is func)

(pred irreflexive
  (no (& r iden)))

(test func-bug 
      #:preds [irreflexive]
      #:scope ([A 3 3])
      #:expect sat)