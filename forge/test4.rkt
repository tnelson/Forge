#lang forge/core

(println (apply +/func (list (make-sig 'A) (make-sig 'B))))
(println (nodeinfo-loc (node-info (+ (make-sig 'A) (make-sig 'B)))))
(println (nodeinfo-loc (node-info (+/func (make-sig 'A) (make-sig 'B)))))