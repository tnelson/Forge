#lang forge/core

(set-option! 'verbose 0)

(define TrueFalse
  (&&/func
   true
   (! false)))

(define Not ; !, not
  (&&/func
   (! (! true))
   (! false)))

(define And ; &&, -and-
  (&&/func
   (&& true true)
   (! (&& true false))
   (! (&& false true))
   (! (&& false false))))

(define Or ; ||, -or-
  (&&/func
   (|| true true)
   (|| true false)
   (|| false true)
   (! (|| false false))))

(define Implies ; =>, implies, <=>, iff, ifte
  (&&/func
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
   (iff false false)

   (ifte true true true)
   (ifte true true false)
   (! (ifte true false true))
   (! (ifte true false false))
   (ifte false true true)
   (! (ifte false true false))
   (ifte false false true)
   (! (ifte false false false))))

(make-test #:name 'basicTrueFalse
           #:preds (list TrueFalse)
           #:expect 'checked)
(make-test #:name 'notOps
           #:preds (list Not)
           #:expect 'checked)
(make-test #:name 'andOps 
           #:preds (list And)
           #:expect 'checked)
(make-test #:name 'orOps 
           #:preds (list Or)
           #:expect 'checked)
(make-test #:name 'impliesOps 
           #:preds (list Implies)
           #:expect 'checked)
