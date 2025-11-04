#lang typed/racket/base/optional

(provide ; (struct-out computation) 
         ; (struct-out computation/delayed) 
         (prefix-out tree: (struct-out node))
         (prefix-out tree: make-node/func)
         get-checker-hash)

(require/typed forge/utils/lazy-tree
  ; [#:struct computation ()]
  ; [#:struct (computation/delayed computation) ([thnk : Any])]
  [make-node/func (-> (-> String Any) String (-> String Any) node)]
  [#:struct node (
    [datum : Any] 
    [child-generator : Any] 
    [children : Any] 
    [ancestors : Any])])

(require (only-in forge/utils/lazy-tree make-node))

(require/typed forge/choose-lang-specific
  [get-checker-hash (-> (HashTable Any Any))])
