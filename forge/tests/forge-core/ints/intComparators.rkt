#lang forge/core

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
        (iff (> (sum i1) (sum i2))
             (in i1 (join i2 (^ succ))))))

(pred LT
    (all ([i1 Int]
          [i2 Int])
        (iff (< (sum i1) (sum i2))
             (in i1 (join (^ succ) i2)))))

; >= <= 

(pred GTE
    (all ([i1 Int]
          [i2 Int])
        (iff (>= (sum i1) (sum i2))
             (in i1 (join i2 (* succ))))))

(pred LTE
    (all ([i1 Int]
          [i2 Int])
        (iff (<= (sum i1) (sum i2))
             (in i1 (join (* succ) i2)))))

; != !> !< !>= !<=

(pred NEQ
    (all ([i1 Int]
          [i2 Int])
        (iff (!= (sum i1) (sum i2))
             (not (= (sum i1) (sum i2))))))

; Commented out ones are not defined in forge/core
; (pred NGT
;     (all ([i1 Int]
;           [i2 Int])
;         (iff (!> (sum i1) (sum i2))
;              (not (> (sum i1) (sum i2))))))

; (pred NLT
;     (all ([i1 Int]
;           [i2 Int])
;         (iff (!< (sum i1) (sum i2))
;              (not (< (sum i1) (sum i2))))))

; (pred NGTE
;     (all ([i1 Int]
;           [i2 Int])
;         (iff (!>= (sum i1) (sum i2))
;              (not (>= (sum i1) (sum i2))))))

; (pred NTLE
;     (all ([i1 Int]
;           [i2 Int])
;         (iff (!<= (sum i1) (sum i2))
;              (not (<= (sum i1) (sum i2))))))

(test equal #:preds [EQ] #:expect theorem)
(test greaterThan #:preds [GT] #:expect theorem)
(test lessThan #:preds [LT] #:expect theorem)
(test greaterThanEqual #:preds [GTE] #:expect theorem)
(test lessThanEqual #:preds [LTE] #:expect theorem)

(test notEqual #:preds [NEQ] #:expect theorem)
; (test notGreaterThan #:preds [NGT] #:expect theorem)
; (test notLessThan #:preds [NLT] #:expect theorem)
; (test notGreaterThanEqual #:preds [NGTE] #:expect theorem)
; (test notLessThanEqual #:preds [NLTE] #:expect theorem)
