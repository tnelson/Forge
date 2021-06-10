#lang forge/core

(set-option! 'verbose 0)

(define TrueFalse
  (&&/func
   true
   (! false)))

(define Not ; !, not
  (&&/func
   (! (! true))
   (! false)

   (not (not true))
   (not false)))

(define And ; &&, -and-
  (&&/func
   (&& true true)
   (not (&& true false))
   (not (&& false true))
   (not (&& false false))

   (and true true)
   (not (and true false))
   (not (and false true))
   (not (and false false))))

(define Or ; ||, -or-
  (&&/func
   (|| true true)
   (|| true false)
   (|| false true)
   (not (|| false false))

   (or true true)
   (or true false)
   (or false true)
   (not (or false false))))

(define Implies ; =>, implies, <=>, iff, ifte
  (&&/func
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
   (iff false false)

   (ifte true true true)
   (ifte true true false)
   (not (ifte true false true))
   (not (ifte true false false))
   (ifte false true true)
   (not (ifte false true false))
   (ifte false false true)
   (not (ifte false false false))))

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
