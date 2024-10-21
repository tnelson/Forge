#lang forge/core

(set-option! 'verbose 0)
;(set-option! 'verbose 10)

(sig Node)
(relation edges (Node Node))

(pred noVarSome (some () true))
(pred noVarSomeDisj (some #:disj () true))
(test emptySome #:preds [(iff noVarSome noVarSomeDisj)] #:expect checked)

(pred oneVarSome (some ([n0 Node]) (in n0 (join Node edges))))
(pred oneVarSomeDisj (some #:disj ([n0 Node]) (in n0 (join Node edges))))
(test oneSome #:preds [(iff oneVarSome oneVarSomeDisj)] #:expect checked)

(pred twoVarSome
      (some ([n1 Node] [n2 Node])
        (&& (no (& n1 n2))
            (in n1 (join n2 edges)))))
(pred twoVarSomeDisj
      (some #:disj ([n1 Node] [n2 Node])
        (in n1 (join n2 edges))))
(test twoSome #:preds [(iff twoVarSome twoVarSomeDisj)] #:expect checked)

(pred manyVarSome
      (some ([n0 Node] [n1 Node] [n2 Node] [n3 Node])
        (&& (&& (no (& n0 n1)) (no (& n0 n2)) (no (& n0 n3)) (no (& n1 n2)) (no (& n1 n3)) (no (& n2 n3)))
            (&& (in (-> n1 n2) edges)
                (in (-> n3 n0) edges)))))
            
(pred manyVarSomeDisj 
    (some #:disj ([n0 Node] [n1 Node] [n2 Node] [n3 Node])
        (&& (in (-> n1 n2) edges)
            (in (-> n3 n0) edges))))
(test manySome #:preds [(iff manyVarSome manyVarSomeDisj)] #:expect checked)
