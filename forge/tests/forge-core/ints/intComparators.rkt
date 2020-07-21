#lang forge/core

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

(check equal #:preds [EQ])
(check greaterThan #:preds [GT])
(check lessThan #:preds [LT])
(check greaterThanEqual #:preds [GTE])
(check lessThanEqual #:preds [LTE])

(check notEqual #:preds [NEQ])
; (check notGreaterThan #:preds [NGT])
; (check notLessThan #:preds [NLT])
; (check notGreaterThanEqual #:preds [NGTE])
; (check notLessThanEqual #:preds [NLTE])