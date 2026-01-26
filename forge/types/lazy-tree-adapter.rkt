#lang typed/racket/base/optional

(provide ; (struct-out computation)
         ; (struct-out computation/delayed)
         (prefix-out tree: (struct-out node))
         (prefix-out tree: make-node/func)
         tree:get-value
         tree:get-child
         get-checker-hash)

(require/typed forge/utils/lazy-tree
  ; [#:struct computation ()]
  ; [#:struct (computation/delayed computation) ([thnk : Any])]
  [make-node/func (-> (-> String Any) String (-> String Any) node)]
  [(get-value tree:get-value) (-> node Any)]
  [(get-child tree:get-child) (-> node Any Any)]
  [#:struct node (
    [datum : Any]
    [child-generator : Any]
    [children : Any]
    [ancestors : Any])])

(require (only-in forge/utils/lazy-tree make-node))

(require/typed forge/choose-lang-specific
  [get-checker-hash (-> (HashTable Any Any))])
