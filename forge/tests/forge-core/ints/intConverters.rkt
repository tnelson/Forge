#lang forge/core

(set-option! 'verbose 0)
; (set-verbosity 10)

; sing      int -> set

(pred Sing
    (no (join succ (sing (int -8))))
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
    (no (join (sing (int 7)) succ)))

(sig IntSet #:abstract)
(relation ints (IntSet Int))

(sig S1 #:one #:extends IntSet)
(sig S2 #:one #:extends IntSet)
(sig S3 #:one #:extends IntSet)
(sig S4 #:one #:extends IntSet)
(sig S5 #:one #:extends IntSet)

; sum       set -> int
; sum-quant set -> int

(inst sum-inst
    (= ints (+ (-> (atom 'S20) (sing (int 6)))
               (+ (-> (atom 'S30)
                      (+ (sing (int 1)) (+ (sing (int 2)) (sing (int 3)))))
                  (+ (-> (atom 'S40)
                         (+ (sing (int -5)) (+ (sing (int -1)) (sing (int 3)))))
                     (-> (atom 'S50) (+ (sing (int 7)) (sing (int 1)))))))))

(pred Sum
    (all ([i Int])
        (= i (sing (sum i))))

    (int= (sum (join S1 ints)) (int 0))
    (int= (sum (join S2 ints)) (int 6))
    (int= (sum (join S3 ints)) (int 6))
    (int= (sum (join S4 ints)) (int -3))
    (int= (sum (join S5 ints)) (int -8)))

(pred SumQuant
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
          (int 135)))


; card      set -> int

(inst card-inst
    (= ints (+ (-> (atom 'S20) (sing (int -5)))
               (+ (-> (atom 'S30) (+ (sing (int -3)) (sing (int 0))))
                  (+ (-> (atom 'S40)
                         (+ (sing (int -8)) (+ (sing (int 7)) (sing (int 1)))))
                     (-> (atom 'S50)
                         (+ (sing (int 4))
                            (+ (sing (int 3))
                               (+ (sing (int 2)) (sing (int 1)))))))))))

(pred Card
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
          (int 4)))


; max, min  set -> int

(inst max-min-inst
    (= ints (+ (-> (atom 'S10) (sing (int 0)))
               (+ (-> (atom 'S20) (+ (sing (int 0)) (sing (int 1))))
                  (+ (-> (atom 'S30)
                         (+ (sing (int -5))
                            (+ (sing (int -2))
                               (+ (sing (int 0))
                                  (sing (int 4))))))
                     (+ (-> (atom 'S40) (sing (int 7)))
                        (-> (atom 'S50) Int)))))))

(pred MaxMin
    (int= (forge:min (join S1 ints))
          (int 0))
    (int= (forge:max (join S1 ints))
          (int 0))

    (int= (forge:min (join S2 ints))
          (int 0))
    (int= (forge:max (join S2 ints))
          (int 1))

    (int= (forge:min (join S3 ints))
          (int -5))
    (int= (forge:max (join S3 ints))
          (int 4))

    (int= (forge:min (join S4 ints))
          (int 7))
    (int= (forge:max (join S4 ints))
          (int 7))

    (int= (forge:min (join S5 ints))
          (int -8))
    (int= (forge:max (join S5 ints))
          (int 7)))



(test sings 
      #:preds [Sing]
      #:scope ([Int 4] [IntSet 5])
      #:expect theorem)

(test sums
      #:preds [Sum] 
      #:bounds [sum-inst]
      #:scope ([Int 4] [IntSet 5])
      #:expect theorem)

(test sumQuants
      #:preds [SumQuant] 
      #:bounds [sum-inst]
      #:scope ([Int 4] [IntSet 5])
      #:expect theorem)

(test cards
      #:preds [Card] 
      #:bounds [card-inst]
      #:scope ([Int 4] [IntSet 5])
      #:expect theorem)

(test maxMins
      #:preds [MaxMin] 
      #:bounds [max-min-inst]
      #:scope ([Int 4] [IntSet 5])
      #:expect theorem)
