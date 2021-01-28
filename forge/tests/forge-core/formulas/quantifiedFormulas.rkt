#lang forge/core

(set-option! 'verbose 0)

(sig Node)

(sig Color #:abstract)
(sig Red #:one #:extends Color)
(sig Green #:one #:extends Color)
(sig Blue #:one #:extends Color)

(relation edges (Node Node Color))

(inst test-inst
    (= Node (+ N1 (+ N2 (+ N3 N4))))

    (= edges (+ (-> (+ (-> N1 N2) ; cycle
                    (+ (-> N2 N3)
                    (+ (-> N3 N4)
                       (-> N4 N1)))) Red0)

             (+ (-> (-> Node Node) Green0) ; complete

                (-> (+ (-> N1 (+ N1 (+ N2 (+ N3 N4)))) ; <= relation
                    (+ (-> N2 (+ N2 (+ N3 N4)))
                    (+ (-> N3 (+ N3 N4))
                       (-> N4 N4)))) Blue0)))))

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
        (and (!= n1 n2)
             (in (-> (-> n1 n2) Color)
                 edges)))

    (some ([n Node]
           [c Color])
        (and (in (-> (-> Node n) c)
                 edges)
             (!in (-> (-> n Node) c)
                  edges))))

(pred No
    (no ([n Node])
        (in (-> (-> n Node) Red)
            edges))

    (no ([n1 Node]
         [n2 Node])
        (and (!= n1 n2)
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
         (not (some ([n Node]) 
                  (not (SomePred n)))))

    (iff (all ([n Node])
             (SomePred n))
         (no ([n Node])
             (not (SomePred n))))

    (iff (some ([n Node])
             (SomePred n))
         (not (no ([n Node])
                  (SomePred n)))))


(test AllQuant
      #:preds [All]
      #:bounds [test-inst]
      #:expect theorem)

(test SomeQuant
      #:preds [Some]
      #:bounds [test-inst]
      #:expect theorem)

(test NoQuant
      #:preds [No]
      #:bounds [test-inst]
      #:expect theorem)


(test QuantifierEquivalences
      #:preds [Equivalences]
      #:expect theorem)

