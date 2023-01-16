#lang forge/core

option run_sterling off


(set-option! 'verbose 0)

(sig Color #:abstract)
(sig Red #:one #:extends Color)
(sig Green #:one #:extends Color)
(sig Blue #:one #:extends Color)

(sig Node #:abstract)
(sig N1 #:one #:extends Node)
(sig N2 #:one #:extends Node)
(sig N3 #:one #:extends Node)

(relation edges (Node Node Color))

(inst test-inst
    (= edges (+ (-> (+ (-> (atom 'N10) (atom 'N20)) 
                    (+ (-> (atom 'N10) (atom 'N30))
                    (+ (-> (atom 'N20) (atom 'N30))
                       (-> (atom 'N30) (atom 'N30))))) (atom 'Red0))
                (-> (+ (-> (atom 'N10) (atom 'N10))
                    (+ (-> (atom 'N10) (atom 'N20))
                    (+ (-> (atom 'N10) (atom 'N30))
                    (+ (-> (atom 'N20) (atom 'N30))
                       (-> (atom 'N30) (atom 'N20)))))) (atom 'Green0)))))

(pred Some
    (some      (join N1 (join edges Red)))
    (some      (join N2 (join edges Red)))
    (some      (join N3 (join edges Red)))
    (! (some (join (join edges Red) N1)))
    (some      (join (join edges Red) N2))
    (some      (join (join edges Red) N3)))

(pred No
    (! (no (join N1 (join edges Red))))
    (! (no (join N2 (join edges Red))))
    (! (no (join N3 (join edges Red))))
    (no      (join (join edges Red) N1))
    (! (no (join (join edges Red) N2)))
    (! (no (join (join edges Red) N3))))

(pred One1
    (! (one (join N1 (join edges Red))))
    (one      (join N2 (join edges Red)))
    (one      (join N3 (join edges Red)))
    (! (one (join (join edges Red) N1)))
    (one      (join (join edges Red) N2))
    (! (one (join (join edges Red) N3))))

(pred Lone1
    (! (lone (join N1 (join edges Red))))
    (lone      (join N2 (join edges Red)))
    (lone      (join N3 (join edges Red)))
    (lone      (join (join edges Red) N1))
    (lone      (join (join edges Red) N2))
    (! (lone (join (join edges Red) N3))))

; These are treated as multiplicity formulas by ast.rkt,
; rather than quantifier formulas.

(pred One2
    (one ([n Node])
        (in (-> n Node)
            (join edges Green)))

    (one ([n1 Node]
          [n2 Node])
        (&& (!= n1 n2)
             (in (+ (-> n1 n2) (-> n2 n1))
                 (join edges Green))))

    (one ([n Node]
          [c Color])
        (in (-> n (-> Node c))
            edges)))

(pred Lone2 
    (lone ([n Node]) ; one
        (in (-> n Node)
            (join edges Green)))

    (lone ([n Node]) ; no
        (in (-> Node n)
            (join edges Green)))


    (lone ([n1 Node] ; one
           [n2 Node])
        (&& (!= n1 n2)
             (in (+ (-> n1 n2) (-> n2 n1))
                 (join edges Green))))

    (lone ([n1 Node] ; no
           [n2 Node])
        (in (+ (-> Node (+ n1 n2))
               (-> (+ n1 n2) Node))
            (join edges Green)))


    (lone ([n Node] ;o ne
           [c Color])
        (in (-> (-> n Node) c)
            edges))

    (lone ([n Node] ; no
           [c Color])
        (in (-> (+ (-> n Node) (-> Node n)) c)
            edges)))

(pred (SomePred n)
    (in (-> (-> n Node) Red)
        edges))

(pred Equivalence
    (iff (lone ([n Node]) (SomePred n))
         (|| (no ([n Node]) (SomePred n))
             (one ([n Node]) (SomePred n)))))



(test someAsMultiplicity
      #:preds [Some]
      #:bounds [test-inst]
      #:expect theorem)

(test noAsMultiplicity
      #:preds [No]
      #:bounds [test-inst]
      #:expect theorem)

(test oneAsMultiplicity
      #:preds [One1]
      #:bounds [test-inst]
      #:expect theorem)

(test loneAsMultiplicity
      #:preds [Lone1]
      #:bounds [test-inst]
      #:expect theorem)

; (test oneAsQuantifer ; CUURRENTLY BUGGED
;       #:preds [One2]
;       #:bounds [test-inst]
;       #:expect theorem)

; (test loneAsQuantifer ; CUURRENTLY BUGGED
;       #:preds [Lone2]
;       #:bounds [test-inst]
;       #:expect theorem)

(test loneEquivalentOneNo
      #:preds [Equivalence]
      #:expect theorem)
