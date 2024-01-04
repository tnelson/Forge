#lang forge/core

(require (only-in rackunit check-exn))

(set-option! 'verbose 0)

; > < =

(pred EQ
    (all ([i1 Int]
          [i2 Int])
        (iff (= (sum i1) (sum i2))
             (= i1 i2))))

(pred GT
    (all ([i1 Int]
          [i2 Int])
        (iff (int> (sum i1) (sum i2))
             (in i1 (join i2 (^ succ))))))

(pred LT
    (all ([i1 Int]
          [i2 Int])
        (iff (int< (sum i1) (sum i2))
             (in i1 (join (^ succ) i2)))))

; >= <= 

(pred GTE
    (all ([i1 Int]
          [i2 Int])
        (iff (int>= (sum i1) (sum i2))
             (in i1 (join i2 (* succ))))))

(pred LTE
    (all ([i1 Int]
          [i2 Int])
        (iff (int<= (sum i1) (sum i2))
             (in i1 (join (* succ) i2)))))

; != !> !< !>= !<=

(pred NEQ
    (all ([i1 Int]
          [i2 Int])
        (iff (!= (sum i1) (sum i2))
             (! (= (sum i1) (sum i2))))))

(test equal #:preds [EQ] #:expect theorem)
(test greaterThan #:preds [GT] #:expect theorem)
(test lessThan #:preds [LT] #:expect theorem)
(test greaterThanEqual #:preds [GTE] #:expect theorem)
(test lessThanEqual #:preds [LTE] #:expect theorem)

(test notEqual #:preds [NEQ] #:expect theorem)

; + is union not addition
; but because of the semantics of union, this works out without an exception
;(check-exn exn:fail:contract?
;           (lambda ()
;             (pred meow
;                   (= (int 5) (+ (int 3) (int 2))))))
