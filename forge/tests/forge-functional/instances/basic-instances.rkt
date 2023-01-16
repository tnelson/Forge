#lang forge/core

option run_sterling off
(set-option! 'verbose 0)
; (set-verbosity 10)

(define A (make-sig 'A))
(define B (make-sig 'B))
(define R (make-relation 'R (list A B)))

(define inst1
  (make-inst (list
              (= A (+ (atom 'Aaron) (+ (atom 'Alice) (atom 'Andy))))
              (= B (+ (atom 'Betty) (+ (atom 'Bob) (atom 'Bennett))))
              (= R (+ (-> (+ (atom 'Aaron) (atom 'Alice)) (atom 'Bob))
                      (-> (atom 'Andy) (+ (atom 'Betty) (atom 'Bennett))))))))

(make-test #:name 'basic1
           #:bounds (list inst1)
           #:sigs (list A B)
           #:relations (list R)
           #:expect 'sat)
(make-test #:name 'basic-sizes
           #:preds (list (int= (card A) (int 3)))
           #:bounds (list inst1)
           #:sigs (list A B)
           #:relations (list R)
           #:expect 'theorem)


(define inst2
  (make-inst (list
              (= A (+ (atom 'A1) (+ (atom 'A2) (atom 'A3)))))))

(make-test #:name 'basic2
           #:bounds (list inst2)
           #:sigs (list A B)
           #:relations (list R)
           #:expect 'sat)
(make-test #:name 'basic-set-size
           #:preds (list (int= (card A) (int 3)))
           #:bounds (list inst2)
           #:sigs (list A B)
           #:relations (list R)
           #:expect 'theorem)
(make-test #:name 'basic-unset-size
           #:preds (list (int= (card B) (int 2))
                         (int= (card R) (int 5)))
           #:bounds (list inst2)
           #:sigs (list A B)
           #:relations (list R)
           #:expect 'sat)

(define inst3
  (make-inst (list
              (= A (atom 'John))
              (= B (atom 'John)))))

; (test basic3 #:bounds [inst3] #:expect unsat) ; BUGGED
