#lang forge/core

(set-option! 'verbose 0)

; Tests the pattern-matching in eval-exp from server/eval-model.rkt
; to make sure that all exprs drop into the correct branch

(define Node (make-sig 'Node))
(define Child (make-sig 'Child #:extends Node))
(define edges (make-relation 'edges (list Node Node)))
(define node-int (make-relation 'node-int (list Node Int)))

(define inst-sing
	(make-inst (list (= Node (atom 'N1))
		             (= node-int (-> (atom 'N1) (sing (int 2)))))))
(make-test #:name 'sing-test-sat
	       #:bounds (list inst-sing)
	       #:sigs (list Node Child)
	       #:relations (list edges node-int)
	       #:preds (list (&& (in (sing (int 2)) (join Node node-int))
	       	                 (one (join Node node-int))))
	       #:expect 'sat)
(make-test #:name 'sing-test-theorem
	       #:bounds (list inst-sing)
	       #:sigs (list Node Child)
	       #:relations (list edges node-int)
	       #:preds (list (&& (in (sing (int 2)) (join Node node-int))
	       	                 (one (join Node node-int))))
	       #:expect 'checked)

(define inst-expr+
	(make-inst (list (= Node (+ (atom 'N1) (atom 'N2))))))
(make-test #:name 'plus_minus_test-sat
	       #:bounds (list inst-expr+)
	       #:sigs (list Node Child)
	       #:relations (list edges node-int)
	       #:preds (list (= (card Node) (int 2)))
	       #:expect 'sat)
(make-test #:name 'plus_minus_test-theorem
	       #:bounds (list inst-expr+)
	       #:sigs (list Node Child)
	       #:relations (list edges node-int)
	       #:preds (list (= (card Node) (int 2)))
	       #:expect 'checked)

(define inst-expr-
	(make-inst (list (= Node (+ (atom 'N1) (+ (atom 'N2) (atom 'N3))))
		             (= Child (- Node (atom 'N3))))))
(make-test #:name 'minus_minus_test-sat
	       #:bounds (list inst-expr-)
	       #:sigs (list Node Child)
	       #:relations (list edges node-int)
	       #:preds (list (one (set ([n Node])
	       	                    (! (in n Child)))))
	       #:expect 'sat)
(make-test #:name 'minus_minus_test-theorem
	       #:bounds (list inst-expr-)
	       #:sigs (list Node Child)
	       #:relations (list edges node-int)
	       #:preds (list (one (set ([n Node])
	       	                    (&& (in n Node)
	       	                    	 (! (in n Child))))))
	       #:expect 'checked)

(define inst-intersect
	(make-inst (list (= Node (+ (atom 'N1) (+ (atom 'N2) (atom 'N3))))
		             (= Child (atom 'N1))
		             (= edges (-> Node (& Node (atom 'N1)))))))
(make-test #:name 'intersect-test-sat
	       #:bounds (list inst-intersect)
	       #:sigs (list Node Child)
	       #:relations (list edges node-int)
	       #:preds (list (= Child (join Node edges)))
	       #:expect 'sat)
(make-test #:name 'intersect-test-theorem
	       #:bounds (list inst-intersect)
	       #:sigs (list Node Child)
	       #:relations (list edges node-int)
	       #:preds (list (= Child (join Node edges)))
	       #:expect 'checked)

(define inst-cross-prod
	(make-inst (list (= Node (+ (atom 'N1) (atom 'N2)))
		             (= edges (-> Node Node)))))
(make-test #:name 'cross-prod-sat
	       #:bounds (list inst-cross-prod)
	       #:sigs (list Node Child)
	       #:relations (list edges node-int)
	       #:preds (list (= edges (-> Node Node)))
	       #:expect 'sat)
(make-test #:name 'cross-prod-theorem
	       #:bounds (list inst-cross-prod)
	       #:sigs (list Node Child)
	       #:relations (list edges node-int)
	       #:preds (list (= edges (-> Node Node)))
	       #:expect 'checked)

(define inst-join
	(make-inst (list (= Node (+ (atom 'Node1) (atom 'Node2)))
		             (= edges (+ (-> (atom 'Node1) (atom 'Node1))
		             	         (-> (atom 'Node2) (atom 'Node1))))
		             (= Child (join Node
		             	            (+ (-> (atom 'Node1) (atom 'Node1))
		             	               (-> (atom 'Node2) (atom 'Node1))))))))
(make-test #:name 'join-sat
	       #:bounds (list inst-join)
	       #:sigs (list Node Child)
	       #:relations (list edges node-int)
	       #:preds (list (= Child (join Node edges))
	       	             (one Child))
	       #:expect 'sat)
(make-test #:name 'join-theorem
	       #:bounds (list inst-join)
	       #:sigs (list Node Child)
	       #:relations (list edges node-int)
	       #:preds (list (= Child (join Node edges))
	       	             (one Child))
	       #:expect 'checked)

(define inst-transitive-closure
	(make-inst (list (= Node (+ (atom 'N1) (+ (atom 'N2) (atom 'N3))))
		             (= edges (+ (-> (atom 'N1) (atom 'N2))
		             	         (-> (atom 'N2) (atom 'N3))))
		             (= Child (join (atom 'N1) (^ edges))))))
(make-test #:name '^-sat
	       #:bounds (list inst-transitive-closure)
	       #:sigs (list Node Child)
	       #:relations (list edges node-int)
	       #:preds (list (one (set ([n Node])
	       	                    (! (in n Child))))
	                     (= Child (join Node edges)))
	       #:expect 'sat)
(make-test #:name '^-theorem
	       #:bounds (list inst-transitive-closure)
	       #:sigs (list Node Child)
	       #:relations (list edges node-int)
	       #:preds (list (one (set ([n Node])
	       	                    (! (in n Child))))
	                     (= Child (join Node edges)))
	       #:expect 'checked)

(define inst-reflexive-transitive-closure
	(make-inst (list (= Node (+ (atom 'N1) (+ (atom 'N2) (atom 'N3))))
		             (= edges (+ (-> (atom 'N1) (atom 'N2))
		             	         (-> (atom 'N2) (atom 'N3))))
		             (= Child (join (atom 'N1) (* edges))))))
(make-test #:name '*-sat
	       #:bounds (list inst-reflexive-transitive-closure)
	       #:sigs (list Node Child)
	       #:relations (list edges node-int)
	       #:preds (list (= Node Child)
	       	             (= Child (join Node (* edges))))
	       #:expect 'sat)
(make-test #:name '*-theorem
	       #:bounds (list inst-reflexive-transitive-closure)
	       #:sigs (list Node Child)
	       #:relations (list edges node-int)
	       #:preds (list (= Node Child)
	       	             (= Child (join Node (* edges))))
	       #:expect 'checked)

(define inst-transpose
	(make-inst (list (= Node (+ (atom 'N1) (+ (atom 'N2) (atom 'N3))))
	                 (= edges (+ (+ (-> (atom 'N1) (atom 'N2))
	                 	            (-> (atom 'N2) (atom 'N3)))
	                             (~ (+ (-> (atom 'N1) (atom 'N2))
	                             	   (-> (atom 'N2) (atom 'N3)))))))))
(make-test #:name 'transpose-sat
	       #:bounds (list inst-transpose)
	       #:sigs (list Node Child)
	       #:relations (list edges node-int)
	       #:preds (list (= edges (~ edges))
	       	             (= (join Node edges) (join edges Node)))
	       #:expect 'sat)
(make-test #:name 'transpose-theorem
	       #:bounds (list inst-transpose)
	       #:sigs (list Node Child)
	       #:relations (list edges node-int)
	       #:preds (list (= edges (~ edges))
	       	             (= (join Node edges) (join edges Node)))
	       #:expect 'checked)

(define inst-constant
	(make-inst (list (= Node (+ (atom 'N1) (+ (atom 'N2) (atom 'N3))))
		             (= Child (& Node univ))
		             (= node-int (-> none none))
		             (= edges (-> Node (join Node iden))))))
(make-test #:name 'constant-sat
	       #:bounds (list inst-constant)
	       #:sigs (list Node Child)
	       #:relations (list edges node-int)
	       #:preds (list (= (card Node) (int 3))
	       	             (no node-int)
	       	             (= Node Child)
	       	             (= edges (-> Node Node)))
	       #:expect 'sat)
(make-test #:name 'constant-theorem
	       #:bounds (list inst-constant)
	       #:sigs (list Node Child)
	       #:relations (list edges node-int)
	       #:preds (list (= (card Node) (int 3))
	       	             (no node-int)
	       	             (= Node Child)
	       	             (= edges (-> Node Node)))
	       #:expect 'checked)

(define inst-atom
	(make-inst (list (= Node (atom 'Node0)))))
(make-test #:name 'atom-sat
	       #:bounds (list inst-atom)
	       #:sigs (list Node Child)
	       #:relations (list edges node-int)
	       #:preds (list (one Node))
	       #:expect 'sat)
(make-test #:name 'atom-theorem
	       #:bounds (list inst-atom)
	       #:sigs (list Node Child)
	       #:relations (list edges node-int)
	       #:preds (list (one Node))
	       #:expect 'checked)
