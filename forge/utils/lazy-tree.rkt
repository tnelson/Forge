#lang racket/base

(require racket/contract)
(require syntax/parse/define)
(require (only-in racket empty? match cons? first thunk))

(provide node? make-node get-child get-children get-value lazy-tree-map is-evaluated?
  make-node/func
  ; temporary, until this module is typed 
  (struct-out computation) (struct-out computation/delayed) (struct-out node))

(struct computation ())
(struct computation/delayed computation (thnk))
(struct computation/evaluated computation (value))

; (struct/contract node (
;   [[datum #:mutable] computation?]
;   [child-generator (-> any/c node?)]
;   [children (hash/c any/c node?)]
;   [[ancestors #:mutable] (listof node?)]))

(struct node ([datum #:mutable] child-generator children [ancestors #:mutable]))

; Has get-value been called on this node yet? 
(define/contract (is-evaluated? a-node)
  (node? . -> . any/c)
  (match (node-datum a-node)
    [(computation/evaluated value) #t]
    [else #f]))

(define/contract (root-node? node)
  (any/c . -> . boolean?)
  (and (node? node) (empty? (node-ancestors node))))

(define-simple-macro (make-node datum child-generator)
  (node (computation/delayed (thunk datum)) child-generator (make-hash) (list)))

(define (make-node/func datum type child-generator)
  (make-node (datum type) child-generator))

(define/contract (get-value a-node)
  (node? . -> . any/c)
  (match (node-datum a-node)
    [(computation/evaluated value) value]
    [(computation/delayed thnk) 
      (when (cons? (node-ancestors a-node))
        (get-value (first (node-ancestors a-node))))
      (define result (thnk))
      (set-node-datum! a-node (computation/evaluated result))
      result]))

(define/contract (get-child a-node arg)
  (node? any/c . -> . any/c)
  (or (hash-ref (node-children a-node) arg #f)
      (let ([child ((node-child-generator a-node) arg)])
        (set-node-ancestors! child (cons a-node (node-ancestors a-node)))
        (hash-set! (node-children a-node) arg child)
        child)))

(define/contract (get-children a-node)
  (node? . -> . (hash/c any/c node?))
  (for/hash ([(key value) (node-children a-node)])
    (values key value)))

(define/contract (get-parent a-node)
  (node? . -> . (or/c node? #f))
  (let ([ancestors (node-ancestors a-node)])
    (and ancestors (first ancestors))))

(define/contract (lazy-tree-map func root [force #f])
  (->* ((-> any/c any/c) root-node?)
       (boolean?)
       root-node?)
  (unless (or force (empty? (node-ancestors root)))
    (raise-argument-error 'lazy-tree-map "root node" root))
  (define ((make-child a-node) arg)
    (define old-child (get-child a-node arg))
    (define new-child (make-node (func (get-value old-child)) (make-child old-child)))
    new-child)
  (make-node (func (get-value root)) (make-child root)))