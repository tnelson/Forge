#lang racket

(require racket/contract)
(require syntax/parse/define)

(provide node? make-node get-child get-children get-value lazy-tree-map)

(struct computation ())
(struct computation/delayed computation (thnk))
(struct computation/evaluated computation (value))

; (struct/contract node (
;   [[datum #:mutable] computation?]
;   [child-generator (-> any/c node?)]
;   [children (hash/c any/c node?)]
;   [[ancestors #:mutable] (listof node?)]))

(struct node ([datum #:mutable] child-generator children [ancestors #:mutable]))


(define/contract (root-node? node)
  (any/c . -> . boolean?)
  (and (node? node) (empty? (node-ancestors node))))

(define-simple-macro (make-node datum child-generator)
  (node (computation/delayed (thunk datum)) child-generator (make-hash) (list)))

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