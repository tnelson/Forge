#lang forge/core

(set-option! 'verbose 0)

(sig Node)

(sig Color #:abstract)
(sig Red #:one #:extends Color)
(sig Green #:one #:extends Color)
(sig Blue #:one #:extends Color)

(relation edges (Node Node Color))

(inst test-inst
    (= Node (+ (atom 'N1) (+ (atom 'N2) (+ (atom 'N3) (atom 'N4)))))

    (= edges (+ (-> (+ (-> (atom 'N1) (atom 'N2)) ; cycle
                       (+ (-> (atom 'N2) (atom 'N3))
                          (+ (-> (atom 'N3) (atom 'N4))
                             (-> (atom 'N4) (atom 'N1)))))
                    (atom 'Red0))
                (+ (-> (-> Node Node) (atom 'Green0)) ; complete
                   (-> (+ (-> (atom 'N1) (+ (atom 'N1) (+ (atom 'N2) (+ (atom 'N3) (atom 'N4))))) ; <= relation
                          (+ (-> (atom 'N2) (+ (atom 'N2) (+ (atom 'N3) (atom 'N4))))
                             (+ (-> (atom 'N3) (+ (atom 'N3) (atom 'N4)))
                                (-> (atom 'N4) (atom 'N4)))))
                        (atom 'Blue0))))))

(pred All
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
            (^ (join edges c)))))

(pred Some
    (some ([n Node])
        (in (-> n Node)
            (join edges Blue)))

    (some ([n1 Node]
           [n2 Node])
        (&&  (!= n1 n2)
             (in (-> (-> n1 n2) Color)
                 edges)))

    (some ([n Node]
           [c Color])
        (&&  (in (-> (-> Node n) c)
                 edges)
             (!in (-> (-> n Node) c)
                  edges))))

(pred No
    (no ([n Node])
        (in (-> (-> n Node) Red)
            edges))

    (no ([n1 Node]
         [n2 Node])
        (&&  (!= n1 n2)
             (= n1 n2)))

    (no ([n Node]
         [c Color])
        (= n c)))

(pred (SomePred n)
    (in (-> (-> n Node) Red)
        edges))

(pred Equivalences
    (iff (all ([n Node]) 
             (SomePred n))
         (! (some ([n Node]) 
                  (! (SomePred n)))))

    (iff (all ([n Node])
             (SomePred n))
         (no ([n Node])
             (! (SomePred n))))

    (iff (some ([n Node])
             (SomePred n))
         (! (no ([n Node])
                  (SomePred n)))))


(test AllQuant
      #:preds [All]
      #:bounds [test-inst]
      #:expect checked)

(test SomeQuant
      #:preds [Some]
      #:bounds [test-inst]
      #:expect checked)

(test NoQuant
      #:preds [No]
      #:bounds [test-inst]
      #:expect checked)


(test QuantifierEquivalences
      #:preds [Equivalences]
      #:expect checked)

