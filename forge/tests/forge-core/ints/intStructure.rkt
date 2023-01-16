#lang forge/core

(set-option! 'verbose 0)

(require (prefix-in @ racket))

(pred SuccStructure
    (all ([i Int]) ; partial function
        (lone (join i succ)))

    (some ([i Int]) ; everything reachable from init
        (= (join i (* succ))
           Int))

    (some ([i Int]) ; there is a term
        (no (join i succ))))

(test succStructure1
      #:preds [SuccStructure]
      #:scope ([Int 1])
      #:expect theorem)
(test succStructure2
      #:preds [SuccStructure]
      #:scope ([Int 2])
      #:expect theorem)
(test succStructure3
      #:preds [SuccStructure]
      #:scope ([Int 3])
      #:expect theorem)
(test succStructure4
      #:preds [SuccStructure]
      #:scope ([Int 4])
      #:expect theorem)
(test succStructure5
      #:preds [SuccStructure]
      #:scope ([Int 5])
      #:expect theorem)


(define (make-n n)
    (cond
      [(@= n 0) (sing (int 0))]
      [(@< n 0) (join succ (make-n (add1 n)))]
      [(@> n 0) (join (make-n (sub1 n)) succ)]))

(pred (Size lower upper)
    ; lower
    (no (make-n (sub1 lower))) ; no -2
    (some (make-n lower)) ; some -1

    ; upper
    (some (make-n upper)) ; some 0
    (no (make-n (add1 upper)))) ; no 1)

(test size1
      #:preds [(Size -1 0)]
      #:scope ([Int 1])
      #:expect theorem)

(test size2
      #:preds [(Size -2 1)]
      #:scope ([Int 2])
      #:expect theorem)

(test size3
      #:preds [(Size -4 3)]
      #:scope ([Int 3])
      #:expect theorem)

(test size4
      #:preds [(Size -8 7)]
      #:scope ([Int 4])
      #:expect theorem)

(test size5
      #:preds [(Size -16 15)]
      #:scope ([Int 5])
      #:expect theorem)
