#lang forge/core

(set-option! 'verbose 0)

(sig Node)
(relation edges (Node Node))

(pred AtomsIn
    (in   (atom 'Node0) Node)
    (in   (atom 'Node1) Node)    
    (in   (atom 'Node2) Node)
    (some Node))

(pred AtomsNotIn
    (not (in        (atom 'Node0) Node))
    (not (in        (atom 'Node1) Node))    
    (not (in        (atom 'Node2) Node))
    (some Node))

; Dangerous to use atom names without a concrete instance to force existence

(test explicitAtomsSat3
      #:preds [AtomsIn]
      #:scope ([Node 3 3])
      #:expect sat)

(test explicitAtomsUnsat3
      #:preds [AtomsNotIn]
      #:scope ([Node 3 3])
      #:expect unsat)

(test explicitAtomsSat4
      #:preds [AtomsIn]
      #:scope ([Node 4])
      #:expect sat)

(test explicitAtomsSat4
      #:preds [AtomsNotIn]
      #:scope ([Node 4])
      #:expect sat)
; Potential bug: [Node 3 4] doesn't mean 3--4 anymore; it means 3
