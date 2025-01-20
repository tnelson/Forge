#lang forge/core

(set-option! 'verbose 0)

; abs, sign

(define Abs
  (&&
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
   (= (abs (int 7)) (int 7))))

(define Sign
  (&&
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
   (= (sign (int 7)) (int 1))))

; add, subtract, multiply, divide, remainder


; pred Add {
;     all i: Int |
;         add[sum[i], 0] = sum[i]

;     let succ2 = succ + (Int - succ.Int)->(Int - Int.succ) |
;         all i1, i2: Int |
;             sing[add[sum[i1], sum[i2.succ2]]] = sing[add[sum[i1], sum[i2]]].succ2
; }

(define Add
  (&&
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
                   succ2))))))

; pred Multiply {
;     all i: Int |
;         multiply[sum[i], 0] = 0

;     let succ2 = succ + (Int - succ.Int)->(Int - Int.succ) |
;         all i1, i2: Int |
;             multiply[sum[i1], sum[i2.succ2]] = add[sum[i1], multiply[sum[i1], sum[i2]]]
; }

(define Multiply
  (&&
   (all ([i Int])
        (int= (multiply (sum i) (int 0))
              (int 0)))

   (let ([succ2 (+ succ (-> (- Int (join succ Int))
                            (- Int (join Int succ))))])
     (all ([i1 Int]
           [i2 Int])
          (int= (multiply (sum i1)
                          (sum (join i2 succ2)))
                (add (sum i1) (multiply (sum i1) (sum i2))))))))

(define Subtract
  (&&
   (all ([i1 Int]
         [i2 Int])
        (int= (add (subtract (sum i1) (sum i2))
                   (sum i2))
              (sum i1)))))

; pred DivideRemainder {
;     all i1: Int, i2: (Int - sing[0]) | let x = sum[i1], y = sum[i2] | 
;         let q = divide[x, y], r = remainder[x, y] | {
;             sign[r] = sign[x] or sign[r] = 0
;             abs[r] < abs[y] or y = -8
;             x = add[multiply[y, q], r]
;         }
; }

(define DivideRemainder
  (&&
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
                 (int= x (add (multiply y q) r))))))))


(make-test #:name 'abs-check
           #:preds (list Abs)
           #:scope (list (list Int 4))
           #:expect 'checked)
(make-test #:name 'sign-check
           #:preds (list Sign)
           #:scope (list (list Int 4))
           #:expect 'checked)

(make-test #:name 'add-check #:preds (list Add) #:expect 'checked)
(make-test #:name 'subtract-check #:preds (list Subtract) #:expect 'checked)
(make-test #:name 'multiply-check #:preds (list Multiply) #:expect 'checked)
(make-test #:name 'divide-check #:preds (list DivideRemainder) #:expect 'checked)
