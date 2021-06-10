#lang forge/core

(set-option! 'verbose 0)

(define Node (make-sig 'Node))
(define t1 (make-relation 't1 (list Node Node)))
(define t2 (make-relation 't2 (list Node Node)))
(define g (make-relation 't3 (list Node Node)))

(define (isUndirectedTree graph)
  (&& (in graph (~ graph))
      (in (-> Node Node) (* graph))
      (no (& iden graph))
      (all ([n Node])
           (all ([m Node])
                (implies (in (+ (-> n m) (-> m n)) graph)
                         (not (in (+ (-> n m) (-> m n))
                                  (^ (- graph (+ (-> n m) (-> m n)))))))))))

(define (spans graph1 graph2)
  (&& (= (+ (join Node graph1) (join graph1 Node))
         (+ (join Node graph2) (join graph2 Node)))
      (in graph1 graph2)))

(define twoSpanningTrees
  (&& (and (isUndirectedTree t1) (spans t1 g))
      (and (isUndirectedTree t2) (spans t2 g))
      (!= t1 t2)))

(define fruit
  (make-inst (list (in t2 g))))

(define thomas-happy
  (make-run #:name 'thomas-happy
            #:preds (list twoSpanningTrees (>= (card Node) (int 5)))
            #:sigs (list Node)
            #:scope (list (list Node 6))
            #:relations (list t1 t2 g)))
(display thomas-happy)
