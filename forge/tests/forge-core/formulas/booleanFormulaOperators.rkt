#lang forge/core

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test helper predicates and captured metadata

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test variations on macro use; make sure they at least expand
(pred (pred1a x) (in x Int))                                            ; 1 arg, no annotations
(pred (pred2a x y) (and (in y Int) (in x Int)))                         ; 2 arg, no annotations
(pred (pred2b (x univ 'lone) y) (and (in x Int) (in y Int)))            ; 2 arg, annotation on 1st
(pred (pred2c x (y univ 'lone)) (and (in x Int) (in y Int)))            ; 2 arg, annotation on 2nd
(pred (pred2d (x Int 'lone) (y univ 'set)) (and (in x Int) (in y Int))) ; 2 arg, annotation on 2nd
(pred (pred_arity2_arg (x iden)) (in x (-> univ univ)))                 ; 1 arg, arity 2 given by annotation

; Check the expansion of these helpers at least has the correct semantics
(pred HelperPred    
    (all ([value1 univ] [value2 univ])
         (and (iff (pred1a value1) (in value1 Int))
              (iff (pred2a value1 value2) (and (in value1 Int) (in value2 Int)))
              (iff (pred2b value1 value2) (and (in value1 Int) (in value2 Int)))
              (iff (pred2c value1 value2) (and (in value1 Int) (in value2 Int)))
              (iff (pred2d value1 value2) (and (in value1 Int) (in value2 Int)))
              ; cannot easily test higher-order helper predicate with forall, so pred_arity2_arg is left out here
              )))
(test helperpreds #:preds [HelperPred] #:expect theorem)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test metadata

(require (prefix-in @ rackunit))

; Implicit defaults (in univ, one)
(@check-equal? (node/fmla/pred-spacer-args (pred1a univ))
               (list (apply-record 'x (mexpr univ 'one) univ)))
(@check-equal? (node/fmla/pred-spacer-args (pred2a univ Int))
               (list (apply-record 'x (mexpr univ 'one) univ) (apply-record 'y (mexpr univ 'one) Int)))
; mix implicit and explicit
(@check-equal? (node/fmla/pred-spacer-args (pred2b univ Int))
               (list (apply-record 'x (mexpr univ 'lone) univ) (apply-record 'y (mexpr univ 'one) Int)))
(@check-equal? (node/fmla/pred-spacer-args (pred2c univ Int))
               (list (apply-record 'x (mexpr univ 'one) univ) (apply-record 'y (mexpr univ 'lone) Int)))
; all explicit
(@check-equal? (node/fmla/pred-spacer-args (pred2d univ Int))
               (list (apply-record 'x (mexpr Int 'lone) univ) (apply-record 'y (mexpr univ 'set) Int)))
; arity >1 argument
(@check-equal? (node/fmla/pred-spacer-args (pred_arity2_arg iden))
               (list (apply-record 'x (mexpr iden 'set) iden)))

; Test metadata is being added via expander for surface language
; pred pred_surface[x,y: lone Int, z: univ] {}
(require "metadata.frg")
(sig Node)
(@check-equal? (node/fmla/pred-spacer-args (pred_surface univ Node Int))
               (list (apply-record 'x (mexpr Int 'lone) univ)
                     (apply-record 'y (mexpr Int 'lone) Node)
                     (apply-record 'z (mexpr univ 'one) Int)))

