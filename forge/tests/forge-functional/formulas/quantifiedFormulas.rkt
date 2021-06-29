#lang forge/core

(set-option! 'verbose 0)

(define Node (make-sig 'Node))

(define Color (make-sig 'Color #:abstract #t))
(define Red (make-sig 'Red #:one #t #:extends Color))
(define Green (make-sig 'Green #:one #t #:extends Color))
(define Blue (make-sig 'Blue #:one #t #:extends Color))

(define edges (make-relation 'edges (list Node Node Color)))

(define test-inst
  (make-inst (list
              (= Node (+ (atom 'N1) (+ (atom 'N2) (+ (atom 'N3) (atom 'N4)))))

              (= edges (+ (-> (+ (-> (atom 'N1) (atom 'N2)) ; cycle
                                 (+ (-> (atom 'N2) (atom 'N3))
                                    (+ (-> (atom 'N3) (atom 'N4))
                                       (-> (atom 'N4) (atom 'N1))))) (atom 'Red0))

                          (+ (-> (-> Node Node) (atom 'Green0)) ; complete

                             (-> (+ (-> (atom 'N1) (+ (atom 'N1) (+ (atom 'N2) (+ (atom 'N3) (atom 'N4))))) ; <= relation
                                    (+ (-> (atom 'N2) (+ (atom 'N2) (+ (atom 'N3) (atom 'N4))))
                                       (+ (-> (atom 'N3) (+ (atom 'N3) (atom 'N4)))
                                          (-> (atom 'N4) (atom 'N4))))) (atom 'Blue0))))))))

(define All
  (&&/func
   (all ([n Node])
        (= Node
           (join n (join edges Green))))
   (all ([n1 Node]
         [n2 Node])
        (implies (in (+ (-> n1 n2) (-> n2 n1))
                     (join edges Blue))
                 (= n1 n2)))
   (all ([n Node]
         [c Color])
        (in (-> n n)
            (^ (join edges c))))))

(define Some
  (&&/func
   (some ([n Node])
         (in (-> n Node)
             (join edges Blue)))
   (some ([n1 Node]
          [n2 Node])
         (and (!= n1 n2)
              (in (-> (-> n1 n2) Color)
                  edges)))
   (some ([n Node]
          [c Color])
         (and (in (-> (-> Node n) c)
                  edges)
              (!in (-> (-> n Node) c)
                   edges)))))

(define No
  (&&/func
   (no ([n Node])
       (in (-> (-> n Node) Red)
           edges))
   (no ([n1 Node]
        [n2 Node])
       (and (!= n1 n2)
            (= n1 n2)))
   (no ([n Node]
        [c Color])
       (= n c))))

(define (SomePred n)
  (&&/func
   (in (-> (-> n Node) Red)
       edges)))

(define Equivalences
  (&&/func
   (iff (all ([n Node])
             (SomePred n))
        (not (some ([n Node])
                   (not (SomePred n)))))
   (iff (all ([n Node])
             (SomePred n))
        (no ([n Node])
            (not (SomePred n))))
   (iff (some ([n Node])
              (SomePred n))
        (not (no ([n Node])
                 (SomePred n))))))

(make-test #:name 'AllQuant
           #:preds (list All)
           #:bounds (list test-inst)
           #:sigs (list Node Color Red Green Blue)
           #:relations (list edges)
           #:expect 'theorem)

(make-test #:name 'SomeQuant
           #:preds (list Some)
           #:bounds (list test-inst)
           #:sigs (list Node Color Red Green Blue)
           #:relations (list edges)
           #:expect 'theorem)

(make-test #:name 'NoQuant
           #:preds (list No)
           #:bounds (list test-inst)
           #:sigs (list Node Color Red Green Blue)
           #:relations (list edges)
           #:expect 'theorem)


(make-test #:name 'QuantifierEquivalences
           #:preds (list Equivalences)
           #:sigs (list Node Color Red Green Blue)
           #:relations (list edges)
           #:expect 'theorem)
