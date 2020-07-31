#lang forge/core

; (set-verbosity 10)

; sing      int -> set

(pred Sing
    (no (join succ (sing (node/int/constant -8))))
    (= (join (sing (node/int/constant -8)) succ) 
       (sing (node/int/constant -7)))
    (= (join (sing (node/int/constant -7)) succ) 
       (sing (node/int/constant -6)))
    (= (join (sing (node/int/constant -6)) succ) 
       (sing (node/int/constant -5)))
    (= (join (sing (node/int/constant -5)) succ) 
       (sing (node/int/constant -4)))
    (= (join (sing (node/int/constant -4)) succ) 
       (sing (node/int/constant -3)))
    (= (join (sing (node/int/constant -3)) succ) 
       (sing (node/int/constant -2)))
    (= (join (sing (node/int/constant -2)) succ) 
       (sing (node/int/constant -1)))
    (= (join (sing (node/int/constant -1)) succ) 
       (sing (node/int/constant 0)))
    (= (join (sing (node/int/constant 0)) succ) 
       (sing (node/int/constant 1)))
    (= (join (sing (node/int/constant 1)) succ) 
       (sing (node/int/constant 2)))
    (= (join (sing (node/int/constant 2)) succ) 
       (sing (node/int/constant 3)))
    (= (join (sing (node/int/constant 3)) succ) 
       (sing (node/int/constant 4)))
    (= (join (sing (node/int/constant 4)) succ) 
       (sing (node/int/constant 5)))
    (= (join (sing (node/int/constant 5)) succ) 
       (sing (node/int/constant 6)))
    (= (join (sing (node/int/constant 6)) succ) 
       (sing (node/int/constant 7)))
    (no (join (sing (node/int/constant 7)) succ)))

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
    (= ints (+ (-> S20 (sing (node/int/constant 6)))
            (+ (-> S30 (+ (sing (node/int/constant 1))
                       (+ (sing (node/int/constant 2))
                          (sing (node/int/constant 3)))))
            (+ (-> S40 (+ (sing (node/int/constant -5))
                       (+ (sing (node/int/constant -1))
                          (sing (node/int/constant 3)))))
               (-> S50 (+ (sing (node/int/constant 7))
                          (sing (node/int/constant 1)))))))))

(pred Sum
    (all ([i Int])
        (= i (sing (sum i))))

    (int= (sum (join S1 ints)) (node/int/constant 0))
    (int= (sum (join S2 ints)) (node/int/constant 6))
    (int= (sum (join S3 ints)) (node/int/constant 6))
    (int= (sum (join S4 ints)) (node/int/constant -3))
    (int= (sum (join S5 ints)) (node/int/constant -8)))

(pred SumQuant
    (int= (sum-quant ([S IntSet])
              (min (join S ints)))
          (node/int/constant 3))

    (int= (sum-quant ([S IntSet])
              (max (join S ints)))
          (node/int/constant 19))

    (int= (sum-quant ([S IntSet])
              (card (join S ints)))
          (node/int/constant 9))

    ; This also checks that sum works with duplicates
    (int= (sum-quant ([S IntSet])
              (sum-quant ([i (join S ints)])
                  (sum i)))
          (node/int/constant 1))

    (int= (sum-quant ([S IntSet])
              (sum-quant ([i (join S ints)])
                  (multiply (sum i) (sum i))))
          (node/int/constant 135)))


; card      set -> int

(inst card-inst
    (= ints (+ (-> S20 (sing (node/int/constant -5)))
            (+ (-> S30 (+ (sing (node/int/constant -3))
                          (sing (node/int/constant 0))))
            (+ (-> S40 (+ (sing (node/int/constant -8))
                       (+ (sing (node/int/constant 7))
                          (sing (node/int/constant 1)))))
               (-> S50 (+ (sing (node/int/constant 4))
                       (+ (sing (node/int/constant 3))
                       (+ (sing (node/int/constant 2))
                          (sing (node/int/constant 1)))))))))))

(pred Card
    (all ([i Int])
        (int= (card i)
              (node/int/constant 1)))

    (int= (card (join S1 ints))
          (node/int/constant 0))
    (int= (card (join S2 ints))
          (node/int/constant 1))
    (int= (card (join S3 ints))
          (node/int/constant 2))
    (int= (card (join S4 ints))
          (node/int/constant 3))
    (int= (card (join S5 ints))
          (node/int/constant 4)))


; max, min  set -> int

(inst max-min-inst
    (= ints (+ (-> S10 (sing (node/int/constant 0)))
            (+ (-> S20 (+ (sing (node/int/constant 0))
                          (sing (node/int/constant 1))))
            (+ (-> S30 (+ (sing (node/int/constant -5))
                       (+ (sing (node/int/constant -2))
                       (+ (sing (node/int/constant 0))
                          (sing (node/int/constant 4))))))
            (+ (-> S40 (sing (node/int/constant 7)))
               (-> S50 Int)))))))

(pred MaxMin
    (int= (min (join S1 ints))
          (node/int/constant 0))
    (int= (max (join S1 ints))
          (node/int/constant 0))

    (int= (min (join S2 ints))
          (node/int/constant 0))
    (int= (max (join S2 ints))
          (node/int/constant 1))

    (int= (min (join S3 ints))
          (node/int/constant -5))
    (int= (max (join S3 ints))
          (node/int/constant 4))

    (int= (min (join S4 ints))
          (node/int/constant 7))
    (int= (max (join S4 ints))
          (node/int/constant 7))

    (int= (min (join S5 ints))
          (node/int/constant -8))
    (int= (max (join S5 ints))
          (node/int/constant 7)))



(check sings 
       #:preds [Sing]
       #:scope ([Int 4] [IntSet 5]))

(check sums
       #:preds [Sum] 
       #:bounds [sum-inst]
       #:scope ([Int 4] [IntSet 5]))

(check sumQuants
       #:preds [SumQuant] 
       #:bounds [sum-inst]
       #:scope ([Int 4] [IntSet 5]))

(check cards
       #:preds [Card] 
       #:bounds [card-inst]
       #:scope ([Int 4] [IntSet 5]))

(check maxMins
       #:preds [MaxMin] 
       #:bounds [max-min-inst]
       #:scope ([Int 4] [IntSet 5]))
