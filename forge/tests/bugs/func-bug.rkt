#lang forge/core

(set-verbosity 0)

(sig A)
(relation r (A A) #:is func)

(pred irreflexive
  (no (& r iden)))

(test func-bug ; fails
      #:preds [irreflexive]
      #:scope ([A 3 3])
      #:expect sat)
