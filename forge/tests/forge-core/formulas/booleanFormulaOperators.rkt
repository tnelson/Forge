#lang forge/core

(set-option 'verbose 0)

(pred TrueFalse
    true
    (! false))

(pred Not ; !, not
    (! (! true))
    (! false)

    (not (not true))
    (not false))

(pred And ; &&, -and-
    (&& true true)
    (not (&& true false))
    (not (&& false true))
    (not (&& false false))

    (and true true)
    (not (and true false))
    (not (and false true))
    (not (and false false)))

(pred Or ; ||, -or-
    (|| true true)
    (|| true false)
    (|| false true)
    (not (|| false false))

    (or true true)
    (or true false)
    (or false true)
    (not (or false false)))

(pred Implies ; =>, implies, <=>, iff, ifte
    (=> true true)
    (not (=> true false))
    (=> false true)
    (=> false false)

    (implies true true)
    (not (implies true false))
    (implies false true)
    (implies false false)

    (<=> true true)
    (not (<=> true false))
    (not (<=> false true))
    (<=> false false)

    (iff true true)
    (not (iff true false))
    (not (iff false true))
    (iff false false)

    (ifte true true true)
    (ifte true true false)
    (not (ifte true false true))
    (not (ifte true false false))
    (ifte false true true)
    (not (ifte false true false))
    (ifte false false true)
    (not (ifte false false false)))

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

