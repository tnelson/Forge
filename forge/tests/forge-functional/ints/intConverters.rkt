#lang forge/core

(set-option! 'verbose 0)
; (set-verbosity 10)

; sing      int -> set

; Note: these tests will not give complete assurance in cases where an int is out-of-bitwidth
; and overflow is allowed. 
(define Sing
  (&&
   ; there is no int before -8 (assumes default bitwidth of 4)
   (no (join succ (sing (int -8))))
   ; confirm successors
   (= (join (sing (int -8)) succ) 
      (sing (int -7)))
   (= (join (sing (int -7)) succ) 
      (sing (int -6)))
   (= (join (sing (int -6)) succ) 
      (sing (int -5)))
   (= (join (sing (int -5)) succ) 
      (sing (int -4)))
   (= (join (sing (int -4)) succ) 
      (sing (int -3)))
   (= (join (sing (int -3)) succ) 
      (sing (int -2)))
   (= (join (sing (int -2)) succ) 
      (sing (int -1)))
   (= (join (sing (int -1)) succ) 
      (sing (int 0)))
   (= (join (sing (int 0)) succ) 
      (sing (int 1)))
   (= (join (sing (int 1)) succ) 
      (sing (int 2)))
   (= (join (sing (int 2)) succ) 
      (sing (int 3)))
   (= (join (sing (int 3)) succ) 
      (sing (int 4)))
   (= (join (sing (int 4)) succ) 
      (sing (int 5)))
   (= (join (sing (int 5)) succ) 
      (sing (int 6)))
   (= (join (sing (int 6)) succ) 
      (sing (int 7)))
   ; there is no successor of 7 (assumes default bitwidth of 4)
   (no (join (sing (int 7)) succ))))

(define IntSet (make-sig 'IntSet #:abstract #t))
(define ints (make-relation 'ints (list IntSet Int)))

(define S1 (make-sig 'S1 #:one #t #:extends IntSet))
(define S2 (make-sig 'S2 #:one #t #:extends IntSet))
(define S3 (make-sig 'S3 #:one #t #:extends IntSet))
(define S4 (make-sig 'S4 #:one #t #:extends IntSet))
(define S5 (make-sig 'S5 #:one #t #:extends IntSet))

; sum       set -> int
; sum-quant set -> int

(define sum-inst
  (make-inst (list
              (= ints (+ (-> (atom 'S20) (sing (int 6)))
                         (+ (-> (atom 'S30) (+ (sing (int 1))
                                               (+ (sing (int 2))
                                                  (sing (int 3)))))
                            (+ (-> (atom 'S40) (+ (sing (int -5))
                                                  (+ (sing (int -1))
                                                     (sing (int 3)))))
                               (-> (atom 'S50) (+ (sing (int 7))
                                                  (sing (int 1)))))))))))

(define Sum
  (&&
   ; sum and sing are inverses
   (all ([i Int])
        (= i (sing (sum i))))

   ; Values summed based on instance, assuming overflow is happening properly
   (int= (sum (join S1 ints)) (int 0))
   (int= (sum (join S2 ints)) (int 6))
   (int= (sum (join S3 ints)) (int 6))
   (int= (sum (join S4 ints)) (int -3))
   (int= (sum (join S5 ints)) (int -8))))

(define SumQuant
  (&&
   (int= (sum-quant ([S IntSet])
                    (min (join S ints)))
         (int 3))

   (int= (sum-quant ([S IntSet])
                    (max (join S ints)))
         (int 19))

   (int= (sum-quant ([S IntSet])
                    (card (join S ints)))
         (int 9))

   ; This also checks that sum works with duplicates
   (int= (sum-quant ([S IntSet])
                    (sum-quant ([i (join S ints)])
                               (sum i)))
         (int 1))

   (int= (sum-quant ([S IntSet])
                    (sum-quant ([i (join S ints)])
                               (multiply (sum i) (sum i))))
         (int 135))))


; card      set -> int

(define card-inst
  (make-inst (list
              (= ints (+ (-> (atom 'S20) (sing (int -5)))
                         (+ (-> (atom 'S30) (+ (sing (int -3))
                                               (sing (int 0))))
                            (+ (-> (atom 'S40) (+ (sing (int -8))
                                                  (+ (sing (int 7))
                                                     (sing (int 1)))))
                               (-> (atom 'S50) (+ (sing (int 4))
                                                  (+ (sing (int 3))
                                                     (+ (sing (int 2))
                                                        (sing (int 1)))))))))))))

(define Card
  (&&
   (all ([i Int])
        (int= (card i)
              (int 1)))

   (int= (card (join S1 ints))
         (int 0))
   (int= (card (join S2 ints))
         (int 1))
   (int= (card (join S3 ints))
         (int 2))
   (int= (card (join S4 ints))
         (int 3))
   (int= (card (join S5 ints))
         (int 4))))


; max, min  set -> int
; S1 -> {0}
; S2 -> {0, 1}
; S3 -> {-5, -2, 0, 4}
; S4 -> {7}
; S5 -> Int
(define max-min-inst
  (make-inst (list
              (= ints (+ (-> (atom 'S10) (sing (int 0)))
                         (+ (-> (atom 'S20) (+ (sing (int 0))
                                               (sing (int 1))))
                            (+ (-> (atom 'S30) (+ (sing (int -5))
                                                  (+ (sing (int -2))
                                                     (+ (sing (int 0))
                                                        (sing (int 4))))))
                               (+ (-> (atom 'S40) (sing (int 7)))
                                  (-> (atom 'S50) Int)))))))))

(define MaxMin
  (&&
   (int= (min (join S1 ints))
         (int 0))
   (int= (max (join S1 ints))
         (int 0))

   (int= (min (join S2 ints))
         (int 0))
   (int= (max (join S2 ints))
         (int 1))

   (int= (min (join S3 ints))
         (int -5))
   (int= (max (join S3 ints))
         (int 4))

   (int= (min (join S4 ints))
         (int 7))
   (int= (max (join S4 ints))
         (int 7))

   (int= (min (join S5 ints))
         (int -8))
   (int= (max (join S5 ints))
         (int 7))))

(make-test #:name 'sings
           #:preds (list Sing)
           #:scope (list (list Int 4) (list IntSet 5))
           #:sigs (list IntSet S1 S2 S3 S4 S5)
           #:relations (list ints)
           #:expect 'theorem)

(make-test #:name 'sums
           #:preds (list Sum)
           #:bounds (list sum-inst)
           #:scope (list (list Int 4) (list IntSet 5))
           #:sigs (list IntSet S1 S2 S3 S4 S5)
           #:relations (list ints)
           #:expect 'theorem)

; Bitwidth 4, 0-5 IntSet atoms
(make-test #:name 'sumQuants
           #:preds (list SumQuant)
           #:bounds (list sum-inst)
           #:scope (list (list Int 4) (list IntSet 5))
           #:sigs (list IntSet S1 S2 S3 S4 S5)
           #:relations (list ints)
           #:expect 'theorem)

(make-test #:name 'cards
           #:preds (list Card)
           #:bounds (list card-inst)
           #:scope (list (list Int 4) (list IntSet 5))
           #:sigs (list IntSet S1 S2 S3 S4 S5)
           #:relations (list ints)
           #:expect 'theorem)

(make-test #:name 'maxMins
           #:preds (list MaxMin)
           #:bounds (list max-min-inst)
           #:scope (list (list Int 4) (list IntSet 5))
           #:sigs (list IntSet S1 S2 S3 S4 S5)
           #:relations (list ints)
           #:expect 'theorem)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; test implicit sing/sum

(define Person (make-sig 'Node))
(define age (make-relation 'age (list Person Int)))

(define SomeNewborn
  (some ([p Person]) (= (join p age) 0)))

(make-test #:name 'implicit=
           #:preds (list SomeNewborn)
           #:scope (list (list Int 4))
           #:sigs (list Person)
           #:relations (list age)
           #:expect 'sat)