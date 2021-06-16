#lang forge/core

(set-option! 'verbose 0)

; Tests the pattern-matching in eval-int-expr from server/eval-model.rkt
; to make sure that all exprs drop into the correct branch

(define Node (make-sig 'Node))
(define ntoi (make-relation 'ntoi (list Node Int)))

(define constant-inst
	(make-inst (list (= Node (atom 'N1))
		             (= ntoi (-> (atom 'N1) (sing (int 1)))))))
(make-test #:name 'constant-sat
	       #:bounds (list constant-inst)
	       #:sigs (list Node)
	       #:relations (list ntoi)
	       #:preds (list (= (sing (int 1)) (join Node ntoi)))
	       #:expect 'sat)
(make-test #:name 'constant-theorem
	       #:bounds (list constant-inst)
	       #:sigs (list Node)
	       #:relations (list ntoi)
	       #:preds (list (= (sing (int 1)) (join Node ntoi)))
	       #:expect 'theorem)

(define sum-inst
	(make-inst (list (= Node (atom 'N1))
		             (= ntoi (-> (atom 'N1)
		             	         (sing (add (sum (sing (int 3)))
		             	          	        (sum (sing (int 5))))))))))
(make-test #:name 'sum-sat
	       #:scope (list (list Int 5))
	       #:bounds (list sum-inst)
	       #:sigs (list Node)
	       #:relations (list ntoi)
	       #:preds (list (= (sing (int 8)) (join Node ntoi)))
	       #:expect 'sat)
(make-test #:name 'sum-theorem
	       #:scope (list (list Int 5))
	       #:bounds (list sum-inst)
	       #:sigs (list Node)
	       #:relations (list ntoi)
	       #:preds (list (= (sing (int 8)) (join Node ntoi)))
	       #:expect 'theorem)

(define max-inst
	(make-inst (list (= Node (atom 'N1))
		             (= ntoi (-> (atom 'N1)
		             	         (sing (max (+ (sing (int 3))
		             	         	           (+ (sing (int 7))
		             	         	           	  (sing (int 5)))))))))))
(make-test #:name 'max-sat
	       #:bounds (list max-inst)
	       #:sigs (list Node)
	       #:relations (list ntoi)
	       #:preds (list (= (sing (int 7)) (join Node ntoi)))
	       #:expect 'sat)
(make-test #:name 'max-theorem
	       #:bounds (list max-inst)
	       #:sigs (list Node)
	       #:relations (list ntoi)
	       #:preds (list (= (sing (int 7)) (join Node ntoi)))
	       #:expect 'theorem)

(define min-inst
	(make-inst (list (= Node (atom 'N1))
		             (= ntoi (-> (atom 'N1)
		             	         (sing (min (+ (sing (int 3))
		             	         	           (+ (sing (int 7))
		             	         	           	  (sing (int 5)))))))))))
(make-test #:name 'min-sat
	       #:bounds (list min-inst)
	       #:sigs (list Node)
	       #:relations (list ntoi)
	       #:preds (list (= (sing (int 3)) (join Node ntoi)))
	       #:expect 'sat)
(make-test #:name 'min-theorem
	       #:bounds (list min-inst)
	       #:sigs (list Node)
	       #:relations (list ntoi)
	       #:preds (list (= (sing (int 3)) (join Node ntoi)))
	       #:expect 'theorem)

(define card-inst
	(make-inst (list (= Node (+ (atom 'N1) (atom 'N2)))
		             (= ntoi (-> Node (sing (card Node)))))))
(make-test #:name 'card-sat
	       #:bounds (list card-inst)
	       #:sigs (list Node)
	       #:relations (list ntoi)
	       #:preds (list (= ntoi (-> Node (sing (int 2)))))
	       #:expect 'sat)
(make-test #:name 'card-theorem
	       #:bounds (list card-inst)
	       #:sigs (list Node)
	       #:relations (list ntoi)
	       #:preds (list (= ntoi (-> Node (sing (int 2)))))
	       #:expect 'theorem)

(define add-inst
	(make-inst (list (= Node (atom 'N1))
		             (= ntoi (-> (atom 'N1)
		             	         (sing (add (int 4) (int 5))))))))
(make-test #:name 'add-sat
	       #:scope (list (list Int 5))
	       #:bounds (list add-inst)
	       #:sigs (list Node)
	       #:relations (list ntoi)
	       #:preds (list (= ntoi (-> Node (sing (int 9)))))
	       #:expect 'sat)
(make-test #:name 'add-theorem
	       #:scope (list (list Int 5))
	       #:bounds (list add-inst)
	       #:sigs (list Node)
	       #:relations (list ntoi)
	       #:preds (list (= ntoi (-> Node (sing (int 9)))))
	       #:expect 'theorem)

(define subtract-inst
	(make-inst (list (= Node (atom 'N1))
		             (= ntoi (-> (atom 'N1)
		             	         (sing (subtract (int 7) (int 4))))))))
(make-test #:name 'subtract-sat
	       #:bounds (list subtract-inst)
	       #:sigs (list Node)
	       #:relations (list ntoi)
	       #:preds (list (= ntoi (-> Node (sing (int 3)))))
	       #:expect 'sat)
(make-test #:name 'subtract-theorem
	       #:bounds (list subtract-inst)
	       #:sigs (list Node)
	       #:relations (list ntoi)
	       #:preds (list (= ntoi (-> Node (sing (int 3)))))
	       #:expect 'theorem)

(define multiply-inst
	(make-inst (list (= Node (atom 'N1))
		             (= ntoi (-> (atom 'N1)
		             	         (sing (multiply (int 6) (int 8))))))))
(make-test #:name 'multiply-sat
	       #:scope (list (list Int 7))
	       #:bounds (list multiply-inst)
	       #:sigs (list Node)
	       #:relations (list ntoi)
	       #:preds (list (= ntoi (-> Node (sing (int 48)))))
	       #:expect 'sat)
(make-test #:name 'multiply-sat
	       #:scope (list (list Int 7))
	       #:bounds (list multiply-inst)
	       #:sigs (list Node)
	       #:relations (list ntoi)
	       #:preds (list (= ntoi (-> Node (sing (int 48)))))
	       #:expect 'theorem)

(define divide-inst
	(make-inst (list (= Node (atom 'N1))
		             (= ntoi (-> (atom 'N1)
		             	         (sing (divide (int 27) (int 9))))))))
(make-test #:name 'divide-sat
	       #:scope (list (list Int 6))
	       #:bounds (list divide-inst)
	       #:sigs (list Node)
	       #:relations (list ntoi)
	       #:preds (list (= ntoi (-> Node (sing (int 3)))))
	       #:expect 'sat)
(make-test #:name 'multiply-sat
	       #:scope (list (list Int 6))
	       #:bounds (list divide-inst)
	       #:sigs (list Node)
	       #:relations (list ntoi)
	       #:preds (list (= ntoi (-> Node (sing (int 3)))))
	       #:expect 'theorem)

(define remainder-inst
	(make-inst (list (= Node (atom 'N1))
		             (= ntoi (-> (atom 'N1)
		             	         (sing (remainder (int 16) (int 5))))))))
(make-test #:name 'remainder-sat
	       #:scope (list (list Int 6))
	       #:bounds (list remainder-inst)
	       #:sigs (list Node)
	       #:relations (list ntoi)
	       #:preds (list (= ntoi (-> Node (sing (int 1)))))
	       #:expect 'sat)
(make-test #:name 'remainder-theorem
	       #:scope (list (list Int 6))
	       #:bounds (list remainder-inst)
	       #:sigs (list Node)
	       #:relations (list ntoi)
	       #:preds (list (= ntoi (-> Node (sing (int 1)))))
	       #:expect 'theorem)

(define abs-inst
	(make-inst (list (= Node (atom 'N1))
		             (= ntoi (-> (atom 'N1)
		             	         (sing (abs (int -2))))))))
(make-test #:name 'abs-sat
	       #:bounds (list abs-inst)
	       #:sigs (list Node)
	       #:relations (list ntoi)
	       #:preds (list (= ntoi (-> Node (sing (int 2)))))
	       #:expect 'sat)
(make-test #:name 'abs-theorem
	       #:bounds (list abs-inst)
	       #:sigs (list Node)
	       #:relations (list ntoi)
	       #:preds (list (= ntoi (-> Node (sing (int 2)))))
	       #:expect 'theorem)

(define sign-inst
	(make-inst (list (= Node (atom 'N1))
		             (= ntoi (-> (atom 'N1)
		             	         (sing (multiply (sign (int -5))
		             	         	             (int 8))))))))
(make-test #:name 'sign-sat
	       #:scope (list (list Int 5))
	       #:bounds (list sign-inst)
	       #:sigs (list Node)
	       #:relations (list ntoi)
	       #:preds (list (= ntoi (-> Node (sing (int -8)))))
	       #:expect 'sat)
(make-test #:name 'sign-theorem
	       #:scope (list (list Int 5))
	       #:bounds (list sign-inst)
	       #:sigs (list Node)
	       #:relations (list ntoi)
	       #:preds (list (= ntoi (-> Node (sing (int -8)))))
	       #:expect 'theorem)
