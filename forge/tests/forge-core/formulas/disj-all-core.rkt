#lang forge/core

option run_sterling off


(set-option! 'verbose 0)
;(set-option! 'verbose 10)

(sig Node)
(relation edges (Node Node))


(pred oneVarall (all ([n0 Node]) (in n0 (join Node edges))))
(pred oneVarallDisj (all #:disj ([n0 Node]) (in n0 (join Node edges))))
(test oneall #:preds [(iff oneVarall oneVarallDisj)] #:expect theorem)

(pred twoVarall
      (all ([n1 Node] [n2 Node])
        (=> (no (& n1 n2))
            (in n1 (join n2 edges)))))
(pred twoVarallDisj
      (all #:disj ([n1 Node] [n2 Node])
        (in n1 (join n2 edges))))
(test twoall #:preds [(iff twoVarall twoVarallDisj)] #:expect theorem)

(pred manyVarall
      (all ([n0 Node] [n1 Node] [n2 Node] [n3 Node])
        (=> (&& (no (& n0 n1)) (no (& n0 n2)) (no (& n0 n3)) (no (& n1 n2)) (no (& n1 n3)) (no (& n2 n3)))
            (&& (in (-> n1 n2) edges)
                (in (-> n3 n0) edges)))))
            
(pred manyVarallDisj 
    (all #:disj ([n0 Node] [n1 Node] [n2 Node] [n3 Node])
        (&& (in (-> n1 n2) edges)
            (in (-> n3 n0) edges))))
(test manyall #:preds [(iff manyVarall manyVarallDisj)] #:expect theorem)
