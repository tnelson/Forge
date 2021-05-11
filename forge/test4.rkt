#lang forge/core

(println (apply +/func (list (make-sig 'A) (make-sig 'B))))
(println (nodeinfo-loc (node-info (+ (make-sig 'A) (make-sig 'B)))))
(println (nodeinfo-loc (node-info (+/func (make-sig 'A) (make-sig 'B)))))
(println (var 'a))
(println (var))

(let* ([A (make-sig 'A)]
       [B (make-sig 'B)]
       [r (make-relation 'r (list A B))])
  (let ([x (var 'x)]
        [y (var 'y)])
    (println (some-quant/func `((,x . ,A) (,y . ,B)) (=/func (join/func x r) y)))))