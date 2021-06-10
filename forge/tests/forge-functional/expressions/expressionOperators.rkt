#lang forge/core

(set-option! 'verbose 0)

(sig Node)
(relation edges (Node Node))

; Unary operators

(pred Tilde
    (= (~ edges) 
       (set ([n1 Node] [n2 Node]) 
           (in (-> n2 n1) edges))))

(pred Caret
    (implies (< (card Node) (int 4))
             (= (^ edges)
                (+ (+ edges
                      (join edges edges))
                   (+ (join edges (join edges edges))
                      (join edges (join edges (join edges edges))))))))

(pred Star
    (= (* edges)
       (+ (^ edges) iden)))


; Binary operators

(pred Plus
    (all ([n1 Node]
          [n2 Node])
        (= (+ (join n1 edges)
              (join n2 edges))
           (set ([n Node])
               (or (in n (join n1 edges))
                   (in n (join n2 edges)))))))

(pred Minus
    (all ([n1 Node]
          [n2 Node])
        (= (- (join n1 edges)
              (join n2 edges))
           (set ([n Node])
               (and (in n (join n1 edges))
                    (!in n (join n2 edges)))))))

(pred Ampersand
    (all ([n1 Node]
          [n2 Node])
        (= (& (join n1 edges)
              (join n2 edges))
           (set ([n Node])
               (and (in n (join n1 edges))
                    (in n (join n2 edges)))))))

(pred Arrow
    (all ([n1 Node]
          [n2 Node])
        (= (-> (join n1 edges)
               (join n2 edges))
           (set ([n3 Node]
                 [n4 Node])
               (and (in n3 (join n1 edges))
                    (in n4 (join n2 edges)))))))

(pred Dot
    (= (join edges edges)
       (set ([n1 Node]
             [n2 Node])
           (some ([n3 Node])
               (and (in n3 (join n1 edges))
                    (in n3 (join edges n2)))))))

(pred IfThenElse1
      (no (ite (in edges edges)
                 univ
                 none)))
(pred IfThenElse2
      (some (ite (some (& Node Int))
                 univ
                 none)))



#|
CURRENTLY BUGGED?
pred LessColon {
    all n: Node |
        n.edges <: edges = {n1: Node, n2: Node | n1->n2 in edges and n1 in n.edges}
}

pred ColonGreater {
    all n: Node |
        edges :> n.edges = {n1: Node, n2: Node | n1->n2 in edges and n2 in n.edges}
}
|#

(test tilde #:preds [Tilde] #:expect theorem)
(test caret #:preds [Caret] #:expect theorem)
(test star #:preds [Star] #:expect theorem)
(test plus #:preds [Plus] #:expect theorem)
(test minus #:preds [Minus] #:expect theorem)
(test ampersandd #:preds [Ampersand] #:expect theorem)
(test arrow #:preds [Arrow] #:expect theorem)
(test dot #:preds [Dot] #:expect theorem)

(test ite1 #:preds [IfThenElse1] #:expect unsat)
(test ite2 #:preds [IfThenElse2] #:expect unsat)



; (test lessColon #:preds [LessColon] #:expect theorem)
; (test colonGreater #:preds [ColonGreater] #:expect theorem)
