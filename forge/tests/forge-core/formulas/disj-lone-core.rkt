#lang forge/core

option run_sterling off


(set-option! 'verbose 0)
;(set-option! 'verbose 10)

(sig lonede)
(relation edges (lonede lonede))

(pred loneVarlone (lone ([n0 lonede]) (in n0 (join lonede edges))))
(pred loneVarloneDisj (lone #:disj ([n0 lonede]) (in n0 (join lonede edges))))
(test lonelone #:preds [(iff loneVarlone loneVarloneDisj)] #:expect theorem)

(pred twoVarlone
      (lone ([n1 lonede] [n2 lonede])
        (&& (no (& n1 n2))
            (in n1 (join n2 edges)))))
(pred twoVarloneDisj
      (lone #:disj ([n1 lonede] [n2 lonede])
        (in n1 (join n2 edges))))
(test twoVar #:preds [(iff twoVarlone twoVarloneDisj)] #:expect theorem)

(pred manyVarlone
      (lone ([n0 lonede] [n1 lonede] [n2 lonede] [n3 lonede])
        (&& (&& (no (& n0 n1)) (no (& n0 n2)) (no (& n0 n3)) (no (& n1 n2)) (no (& n1 n3)) (no (& n2 n3)))
            (&& (in (-> n1 n2) edges)
                (in (-> n3 n0) edges)))))
            
(pred manyVarloneDisj 
    (lone #:disj ([n0 lonede] [n1 lonede] [n2 lonede] [n3 lonede])
        (&& (in (-> n1 n2) edges)
            (in (-> n3 n0) edges))))
(test manylone #:preds [(iff manyVarlone manyVarloneDisj)] #:expect theorem)
