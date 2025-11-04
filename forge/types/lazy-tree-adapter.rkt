#lang typed/racket/base

(provide ; (struct-out computation) 
         ; (struct-out computation/delayed) 
         (prefix-out tree: (struct-out node))
         (prefix-out tree: make-node/func)
         get-checker-hash)

; We need the tree:make-node macro, which expands using constructs from that module:
;  node, computation/delayed 
(require/typed forge/utils/lazy-tree
  [#:struct computation ()]
  [#:struct (computation/delayed computation) ([thnk : Any])]
  [make-node/func (-> Any Symbol Any node)]
  [#:struct node (
    [datum : Any] 
    [child-generator : Any] 
    [children : Any] 
    [ancestors : Any])])

(require (only-in forge/utils/lazy-tree make-node))

(require/typed forge/choose-lang-specific
  [get-checker-hash (-> (HashTable Any Any))])
