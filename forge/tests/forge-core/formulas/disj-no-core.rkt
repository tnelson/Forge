#lang forge/core

(set-option! 'verbose 0)
;(set-option! 'verbose 10)

(sig Node)
(relation edges (Node Node))

(pred oneVarno (no ([n0 Node]) (in n0 (join Node edges))))
(pred oneVarnoDisj (no #:disj ([n0 Node]) (in n0 (join Node edges))))
(test oneno #:preds [(iff oneVarno oneVarnoDisj)] #:expect theorem)

(pred twoVarno
      (no ([n1 Node] [n2 Node])
        (&& (no (& n1 n2))
            (in n1 (join n2 edges)))))
(pred twoVarnoDisj
      (no #:disj ([n1 Node] [n2 Node])
        (in n1 (join n2 edges))))
(test twono #:preds [(iff twoVarno twoVarnoDisj)] #:expect theorem)

(pred manyVarno
      (no ([n0 Node] [n1 Node] [n2 Node] [n3 Node])
        (&& (&& (no (& n0 n1)) (no (& n0 n2)) (no (& n0 n3)) (no (& n1 n2)) (no (& n1 n3)) (no (& n2 n3)))
            (&& (in (-> n1 n2) edges)
                (in (-> n3 n0) edges)))))
            
(pred manyVarnoDisj 
    (no #:disj ([n0 Node] [n1 Node] [n2 Node] [n3 Node])
        (&& (in (-> n1 n2) edges)
            (in (-> n3 n0) edges))))
(test manyno #:preds [(iff manyVarno manyVarnoDisj)] #:expect theorem)
