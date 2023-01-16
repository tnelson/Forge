#lang forge/core

(set-option! 'verbose 0)
;(set-option! 'verbose 10)

(sig onede)
(relation edges (onede onede))

(pred oneVarone (one ([n0 onede]) (in n0 (join onede edges))))
(pred oneVaroneDisj (one #:disj ([n0 onede]) (in n0 (join onede edges))))
(test oneone #:preds [(iff oneVarone oneVaroneDisj)] #:expect theorem)

(pred twoVarone
      (one ([n1 onede] [n2 onede])
        (&& (no (& n1 n2))
            (in n1 (join n2 edges)))))
(pred twoVaroneDisj
      (one #:disj ([n1 onede] [n2 onede])
        (in n1 (join n2 edges))))
(test twoVar #:preds [(iff twoVarone twoVaroneDisj)] #:expect theorem)

(pred manyVarone
      (one ([n0 onede] [n1 onede] [n2 onede] [n3 onede])
        (&& (&& (no (& n0 n1)) (no (& n0 n2)) (no (& n0 n3)) (no (& n1 n2)) (no (& n1 n3)) (no (& n2 n3)))
            (&& (in (-> n1 n2) edges)
                (in (-> n3 n0) edges)))))
            
(pred manyVaroneDisj 
    (one #:disj ([n0 onede] [n1 onede] [n2 onede] [n3 onede])
        (&& (in (-> n1 n2) edges)
            (in (-> n3 n0) edges))))
(test manyone #:preds [(iff manyVarone manyVaroneDisj)] #:expect theorem)
