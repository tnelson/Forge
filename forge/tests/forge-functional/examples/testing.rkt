#lang forge/core

option run_sterling off
(set-option! 'verbose 0)
;(set-verbosity 0)

(define Node (make-sig 'Node))
(define Root (make-sig 'Root #:one #t #:extends Node))

(define edges (make-relation 'edges (list Node Node)))

(define acyclic (no (& iden (^ edges))))
(define root-connected (in Node (join Root (* edges))))

(define rooted-acyclic-run (make-run #:name 'rooted-acyclic-run
                                     #:preds (list acyclic root-connected)
                                     #:scope (list (list Node 4 6))
                                     #:sigs (list Node Root)
                                     #:relations (list edges)))
;(display rooted-acyclic-run)
