#lang forge/core

(set-option! 'verbose 0)

(require (prefix-in @ racket))

(define SuccStructure
  (&&
   (all ([i Int]) ; partial function
        (lone (join i succ)))

   (some ([i Int]) ; everything reachable from init
         (= (join i (* succ))
            Int))

   (some ([i Int]) ; there is a term
         (no (join i succ)))))

(make-test #:name 'succStructure1
           #:preds (list SuccStructure)
           #:scope (list (list Int 1))
           #:expect 'theorem)
(make-test #:name 'succStructure2
           #:preds (list SuccStructure)
           #:scope (list (list Int 2))
           #:expect 'theorem)
(make-test #:name 'succStructure3
           #:preds (list SuccStructure)
           #:scope (list (list Int 3))
           #:expect 'theorem)
(make-test #:name 'succStructure4
           #:preds (list SuccStructure)
           #:scope (list (list Int 4))
           #:expect 'theorem)
(make-test #:name 'succStructure5
           #:preds (list SuccStructure)
           #:scope (list (list Int 5))
           #:expect 'theorem)


(define (make-n n)
  (cond
    [(@= n 0) (sing (int 0))]
    [(@< n 0) (join succ (make-n (add1 n)))]
    [(@> n 0) (join (make-n (sub1 n)) succ)]))

(define (Size lower upper)
  (&&
   ; lower
   (no (make-n (sub1 lower)))
   (some (make-n lower))

   ; upper
   (some (make-n upper))
   (no (make-n (add1 upper)))))

(make-test #:name 'size1
           #:preds (list (Size -1 0))
           #:scope (list (list Int 1))
           #:expect 'theorem)

(make-test #:name 'size2
           #:preds (list (Size -2 1))
           #:scope (list (list Int 2))
           #:expect 'theorem)

(make-test #:name 'size3
           #:preds (list (Size -4 3))
           #:scope (list (list Int 3))
           #:expect 'theorem)

(make-test #:name 'size4
           #:preds (list (Size -8 7))
           #:scope (list (list Int 4))
           #:expect 'theorem)

(make-test #:name 'size5
           #:preds (list (Size -16 15))
           #:scope (list (list Int 5))
           #:expect 'theorem)
