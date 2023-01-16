#lang forge/core

option run_sterling off


(set-option! 'verbose 0)

; No more overloading (<and> and <or> are normal Racket operators now)

(pred TrueFalse
    true
    (! false))

(pred Not 
    (! (! true))
    (! false))

(pred And 
    (&& true true)
    (! (&& true false))
    (! (&& false true))
    (! (&& false false)))

(pred Or
    (|| true true)
    (|| true false)
    (|| false true)
    (! (|| false false)))

(pred Implies ; =>, implies, <=>, iff, ifte
    (=> true true)
    (! (=> true false))
    (=> false true)
    (=> false false)

    (implies true true)
    (! (implies true false))
    (implies false true)
    (implies false false)

    (<=> true true)
    (! (<=> true false))
    (! (<=> false true))
    (<=> false false)

    (iff true true)
    (! (iff true false))
    (! (iff false true))
    (iff false false)

    (ifte true true true)
    (ifte true true false)
    (! (ifte true false true))
    (! (ifte true false false))
    (ifte false true true)
    (! (ifte false true false))
    (ifte false false true)
    (! (ifte false false false)))

(test basicTrueFalse 
      #:preds [TrueFalse]
      #:expect theorem)
(test notOps 
      #:preds [Not]
      #:expect theorem)
(test andOps 
      #:preds [And]
      #:expect theorem)
(test orOps 
      #:preds [Or]
      #:expect theorem)
(test impliesOps 
      #:preds [Implies]
      #:expect theorem)

