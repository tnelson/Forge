#lang forge/core

(set-option! 'verbose 0)

; abs, sign

(pred Abs
    (= (abs (int -8)) (int -8))
    (= (abs (int -7)) (int 7))
    (= (abs (int -6)) (int 6))
    (= (abs (int -5)) (int 5))
    (= (abs (int -4)) (int 4))
    (= (abs (int -3)) (int 3))
    (= (abs (int -2)) (int 2))
    (= (abs (int -1)) (int 1))
    (= (abs (int 0)) (int 0))
    (= (abs (int 1)) (int 1))
    (= (abs (int 2)) (int 2))
    (= (abs (int 3)) (int 3))
    (= (abs (int 4)) (int 4))
    (= (abs (int 5)) (int 5))
    (= (abs (int 6)) (int 6))
    (= (abs (int 7)) (int 7)))

(pred Sign
    (= (sign (int -8)) (int -1))
    (= (sign (int -7)) (int -1))
    (= (sign (int -6)) (int -1))
    (= (sign (int -5)) (int -1))
    (= (sign (int -4)) (int -1))
    (= (sign (int -3)) (int -1))
    (= (sign (int -2)) (int -1))
    (= (sign (int -1)) (int -1))
    (= (sign (int 0)) (int 0))
    (= (sign (int 1)) (int 1))
    (= (sign (int 2)) (int 1))
    (= (sign (int 3)) (int 1))
    (= (sign (int 4)) (int 1))
    (= (sign (int 5)) (int 1))
    (= (sign (int 6)) (int 1))
    (= (sign (int 7)) (int 1)))

; add, subtract, multiply, divide, remainder

(pred Add
    (all ([i Int])
        (int= (add (sum i) (int 0))
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
        (int= (multiply (sum i) (int 0))
              (int 0)))

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

(pred DivideRemainder
    (all ([i1 Int]
          [i2 (- Int (sing (int 0)))])
        (let ([x (sum i1)]
              [y (sum i2)])
            (let ([q (divide x y)]
                  [r (remainder x y)])
                (&& (|| (= (sign r) (sign x))
                         (= (sign r) (int 0)))
                     (|| (int< (abs r) (abs y))
                         (int= y (int -8)))
                     (int= x (add (multiply y q) r)))))))

(test abs-check
      #:preds [Abs]
      #:scope ([Int 4])
      #:expect checked)
(test sign-check
      #:preds [Sign]
      #:scope ([Int 4])
      #:expect checked)

(test add-check #:preds [Add] #:expect checked)
(test subtract-check #:preds [Subtract] #:expect checked)
(test multiply-check #:preds [Multiply] #:expect checked)
(test divide-check #:preds [DivideRemainder] #:expect checked)
