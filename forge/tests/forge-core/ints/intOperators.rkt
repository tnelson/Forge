#lang forge/core

; abs, sign

(pred Abs
    (= (abs (node/int/constant -8)) (node/int/constant -8))
    (= (abs (node/int/constant -7)) (node/int/constant 7))
    (= (abs (node/int/constant -6)) (node/int/constant 6))
    (= (abs (node/int/constant -5)) (node/int/constant 5))
    (= (abs (node/int/constant -4)) (node/int/constant 4))
    (= (abs (node/int/constant -3)) (node/int/constant 3))
    (= (abs (node/int/constant -2)) (node/int/constant 2))
    (= (abs (node/int/constant -1)) (node/int/constant 1))
    (= (abs (node/int/constant 0)) (node/int/constant 0))
    (= (abs (node/int/constant 1)) (node/int/constant 1))
    (= (abs (node/int/constant 2)) (node/int/constant 2))
    (= (abs (node/int/constant 3)) (node/int/constant 3))
    (= (abs (node/int/constant 4)) (node/int/constant 4))
    (= (abs (node/int/constant 5)) (node/int/constant 5))
    (= (abs (node/int/constant 6)) (node/int/constant 6))
    (= (abs (node/int/constant 7)) (node/int/constant 7)))

(pred Sign
    (= (sign (node/int/constant -8)) (node/int/constant -1))
    (= (sign (node/int/constant -7)) (node/int/constant -1))
    (= (sign (node/int/constant -6)) (node/int/constant -1))
    (= (sign (node/int/constant -5)) (node/int/constant -1))
    (= (sign (node/int/constant -4)) (node/int/constant -1))
    (= (sign (node/int/constant -3)) (node/int/constant -1))
    (= (sign (node/int/constant -2)) (node/int/constant -1))
    (= (sign (node/int/constant -1)) (node/int/constant -1))
    (= (sign (node/int/constant 0)) (node/int/constant 0))
    (= (sign (node/int/constant 1)) (node/int/constant 1))
    (= (sign (node/int/constant 2)) (node/int/constant 1))
    (= (sign (node/int/constant 3)) (node/int/constant 1))
    (= (sign (node/int/constant 4)) (node/int/constant 1))
    (= (sign (node/int/constant 5)) (node/int/constant 1))
    (= (sign (node/int/constant 6)) (node/int/constant 1))
    (= (sign (node/int/constant 7)) (node/int/constant 1)))

; add, subtract, multiply, divide, remainder


; pred Add {
;     all i: Int |
;         add[sum[i], 0] = sum[i]

;     let succ2 = succ + (Int - succ.Int)->(Int - Int.succ) |
;         all i1, i2: Int |
;             sing[add[sum[i1], sum[i2.succ2]]] = sing[add[sum[i1], sum[i2]]].succ2
; }

(pred Add
    (all ([i Int])
        (int= (add (sum i) (node/int/constant 0))
              (sum i)))

    (let ([succ2 (+ succ (-> (- Int (join succ Int))
                             (- Int (join Int succ))))])
        (all ([i1 Int]
              [i2 Int])
            (= (sing (add (sum i1)
                          (sum (join i2 succ2))))
               (join (sing (add (sum i1)
                                (sum i2)))
                     succ2)))))

; pred Multiply {
;     all i: Int |
;         multiply[sum[i], 0] = 0

;     let succ2 = succ + (Int - succ.Int)->(Int - Int.succ) |
;         all i1, i2: Int |
;             multiply[sum[i1], sum[i2.succ2]] = add[sum[i1], multiply[sum[i1], sum[i2]]]
; }

(pred Multiply
    (all ([i Int])
        (int= (multiply (sum i) (node/int/constant 0))
              (node/int/constant 0)))

    (let ([succ2 (+ succ (-> (- Int (join succ Int))
                             (- Int (join Int succ))))])
        (all ([i1 Int]
              [i2 Int])
            (int= (multiply (sum i1)
                            (sum (join i2 succ2)))
                  (add (sum i1) (multiply (sum i1) (sum i2)))))))

(pred Subtract
    (all ([i1 Int]
          [i2 Int])
        (int= (add (subtract (sum i1) (sum i2))
                   (sum i2))
              (sum i1))))

; pred DivideRemainder {
;     all i1: Int, i2: (Int - sing[0]) | let x = sum[i1], y = sum[i2] | 
;         let q = divide[x, y], r = remainder[x, y] | {
;             sign[r] = sign[x] or sign[r] = 0
;             abs[r] < abs[y] or y = -8
;             x = add[multiply[y, q], r]
;         }
; }

(pred DivideRemainder
    (all ([i1 Int]
          [i2 (- Int (sing (node/int/constant 0)))])
        (let ([x (sum i1)]
              [y (sum i2)])
            (let ([q (divide x y)]
                  [r (remainder x y)])
                (and (or (= (sign r) (sign x))
                         (= (sign r) (node/int/constant 0)))
                     (or (< (abs r) (abs y))
                         (int= y (node/int/constant -8)))
                     (int= x (add (multiply y q) r)))))))


(check abs-check
       #:preds [Abs]
       #:scope ([Int 4]))
(check sign-check
       #:preds [Sign]
       #:scope ([Int 4]))

(check add-check #:preds [Add])
(check subtract-check #:preds [Subtract])
(check multiply-check #:preds [Multiply])
(check divide-check #:preds [DivideRemainder])
