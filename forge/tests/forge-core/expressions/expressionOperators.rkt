#lang forge/core

(sig Node)
(relation edges (Node Node))

; Unary operators

(pred Tilde
    (= (~ edges) 
       (set ([n1 Node] [n2 Node]) 
           (in (-> n2 n1) edges))))

(pred Caret
    (implies (< (card Node) (node/int/constant 4))
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



#| CURRENTLY BUGGED?
pred LessColon {
    all n: Node |
        n.edges <: edges = {n1: Node, n2: Node | n1->n2 in edges and n1 in n.edges}
}

pred ColonGreater {
    all n: Node |
        edges :> n.edges = {n1: Node, n2: Node | n1->n2 in edges and n2 in n.edges}
}
|#

(check tilde #:preds [Tilde])
(check caret #:preds [Caret])
(check star #:preds [Star])
(check plus #:preds [Plus])
(check minus #:preds [Minus])
(check ampersandd #:preds [Ampersand])
(check arrow #:preds [Arrow])
(check dot #:preds [Dot])
; (check lessColon #:preds [LessColon])
; (check colonGreater #:preds [ColonGreater])