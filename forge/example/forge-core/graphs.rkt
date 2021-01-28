#lang forge/core

(sig Node)

(sig Color #:abstract)
(sig Red #:one #:extends Color)
(sig Green #:one #:extends Color)
(sig Blue #:one #:extends Color)

(relation Edges (Node Color Node))

; (Color) -> (Node Node)
(fun (get-graph color)
  (set ([n1 Node] 
        [n2 Node]) 
    (some ([cx color])
      (in (-> n1 cx n2) Edges))))

; Check two different definitions of disjoint,
; meaning all color graphs are pairwise disjoint
(pred disjoint-colors
  (no ([c1 Color] 
       [c2 (- Color c1)]) 
    (some (& (get-graph c1) 
             (get-graph c2)))))

(pred disjoint-colors2
  (all ([c1 Color]
        [c2 Color])
    (=> (!= c1 c2)
        (no (& (get-graph c1)
               (get-graph c2))))))

(run equivalent-disjoint 
     #:preds [(not (<=> disjoint-colors disjoint-colors2))])
(unless (is-unsat? equivalent-disjoint)
  (displayln "The two disjoints were not equivalent!")
  (display equivalent-disjoint))


; Find a partition of the complete graph into the colors
(pred covers-graph
  (= (-> Node Node) (get-graph Color)))

(run partition-edges
     #:preds [covers-graph
              disjoint-colors]
     #:scope ([Node 2 2]))
(display partition-edges)

; Find 3 spanning trees that share no edges
(pred (spanning graph)
  (ni (* (+ graph (~ graph))) 
      (-> Node Node)))

(pred (spanning-tree color)
  (let ([graph (get-graph color)])
    (and
      (spanning graph)
      (no (& iden (^ graph)))
      (all ([n1 Node] [n2 (join n1 graph)])
        (not (spanning (- graph (-> n1 n2))))))))

(pred distinct-spanning-trees
  disjoint-colors
  (all ([c Color]) (spanning-tree c)))

(run dst
     #:preds [distinct-spanning-trees])
(display dst)


; Missing functionality: instances are buggy oops

; (inst dst-inst
;   (= Node (+ Node1 (+ Node2 (+ Node3 Node4))))
;   (= Red Red0)
;   (ni Edges  (+ (-> Node1 Red Node2)
;              (+ (-> Node1 Red Node3)
;              (+ (-> Node2 Red Node4)
;              (+ (-> Node3 Red Node4)))))))

; (run dst-check
;      #:preds [distinct-spanning-trees]
;      #:bounds [dst-inst])
; (unless (is-unsat? dst-check)
;   (displayln "The instance was sat!")
;   (display dst-check))


