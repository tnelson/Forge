#lang forge/core

(set-option! 'verbose 0)

(define Node (make-sig 'Node))
(define edges (make-relation 'edges (list Node Node)))

(define AtomsIn
  (&&/func
   (in   (atom 'Node0) Node)
   (in   (atom 'Node1) Node)    
   (in   (atom 'Node2) Node)
   (some Node)))

(define AtomsNotIn
  (&&/func
   (! (in        (atom 'Node0) Node))
   (! (in        (atom 'Node1) Node))    
   (! (in        (atom 'Node2) Node))
   (some Node)))

; Dangerous to use atom names without a concrete instance to force existence

(make-test #:name 'explicitAtomsSat3
           #:preds (list AtomsIn)
           #:scope (list (list Node 3 3))
           #:sigs (list Node)
           #:relations (list edges)
           #:expect 'sat)

(make-test #:name 'explicitAtomsUnsat3
           #:preds (list AtomsNotIn)
           #:scope (list (list Node 3 3))
           #:sigs (list Node)
           #:relations (list edges)
           #:expect 'unsat)

(make-test #:name 'explicitAtomsSat4
           #:preds (list AtomsIn)
           #:scope (list (list Node 4))
           #:sigs (list Node)
           #:relations (list edges)
           #:expect 'sat)

(make-test #:name 'explicitAtomsSat5
           #:preds (list AtomsNotIn)
           #:scope (list (list Node 4))
           #:sigs (list Node)
           #:relations (list edges)
           #:expect 'sat)
; Potential bug: [Node 3 4] doesn't mean 3--4 anymore; it means 3
