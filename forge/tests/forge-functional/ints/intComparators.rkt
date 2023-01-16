#lang forge/core

(set-option! 'verbose 0)

; > < =

(define EQ
  (&&
   (all ([i1 Int]
         [i2 Int])
        (iff (= (sum i1) (sum i2))
             (= i1 i2)))))

(define GT
  (&&
   (all ([i1 Int]
         [i2 Int])
        (iff (> (sum i1) (sum i2))
             (in i1 (join i2 (^ succ)))))))

(define LT
  (&&
   (all ([i1 Int]
         [i2 Int])
        (iff (< (sum i1) (sum i2))
             (in i1 (join (^ succ) i2))))))

; >= <= 

(define GTE
  (&&
   (all ([i1 Int]
         [i2 Int])
        (iff (>= (sum i1) (sum i2))
             (in i1 (join i2 (* succ)))))))

(define LTE
  (&&
   (all ([i1 Int]
         [i2 Int])
        (iff (<= (sum i1) (sum i2))
             (in i1 (join (* succ) i2))))))

; != !> !< !>= !<=

(define NEQ
  (&&
   (all ([i1 Int]
         [i2 Int])
        (iff (!= (sum i1) (sum i2))
             (! (= (sum i1) (sum i2)))))))

; Commented out ones are not defined in forge/core
; (pred NGT
;     (all ([i1 Int]
;           [i2 Int])
;         (iff (!> (sum i1) (sum i2))
;              (! (> (sum i1) (sum i2))))))

; (pred NLT
;     (all ([i1 Int]
;           [i2 Int])
;         (iff (!< (sum i1) (sum i2))
;              (! (< (sum i1) (sum i2))))))

; (pred NGTE
;     (all ([i1 Int]
;           [i2 Int])
;         (iff (!>= (sum i1) (sum i2))
;              (! (>= (sum i1) (sum i2))))))

; (pred NTLE
;     (all ([i1 Int]
;           [i2 Int])
;         (iff (!<= (sum i1) (sum i2))
;              (! (<= (sum i1) (sum i2))))))

(make-test #:name 'equal
           #:preds (list EQ)
           #:expect 'theorem)
(make-test #:name 'greaterThan
           #:preds (list GT)
           #:expect 'theorem)
(make-test #:name 'lessThan
           #:preds (list LT)
           #:expect 'theorem)
(make-test #:name 'greaterThanEqual
           #:preds (list GTE)
           #:expect 'theorem)
(make-test #:name 'lessThanEqual
           #:preds (list LTE)
           #:expect 'theorem)

(make-test #:name 'notEqual
           #:preds (list NEQ)
           #:expect 'theorem)
; (test notGreaterThan #:preds [NGT] #:expect theorem)
; (test notLessThan #:preds [NLT] #:expect theorem)
; (test notGreaterThanEqual #:preds [NGTE] #:expect theorem)
; (test notLessThanEqual #:preds [NLTE] #:expect theorem)
