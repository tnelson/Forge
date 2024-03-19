#lang forge/core
(require (only-in rackunit check-true check-equal?))
(require (only-in forge/last-checker infer-atom-type))
(require (only-in racket/set list->set))

(set-option! 'verbose 0)

(sig Node)
(relation edges (Node Node))

(pred AtomsIn
    (in   (atom 'Node0) Node)
    (in   (atom 'Node1) Node)    
    (in   (atom 'Node2) Node)
    (some Node))

(pred AtomsNotIn
    (! (in        (atom 'Node0) Node))
    (! (in        (atom 'Node1) Node))    
    (! (in        (atom 'Node2) Node))
    (some Node))

; Dangerous to use atom names without a concrete instance to force existence.
; These symbols are what, at the moment, Forge will name atoms for the given scope.

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

(test explicitAtomsSat5
      #:preds [AtomsNotIn]
      #:scope ([Node 4])
      #:expect sat)
; Potential bug: [Node 3 4] doesn't mean 3--4 anymore; it means 3


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test that inference for an atom's most-specific sig is working
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sig NodeA #:extends Node)
(sig NodeB #:extends Node)
(sig NodeC #:extends Node)
(sig NodeAA #:extends NodeA)
(sig NodeBA #:extends NodeA)
(sig NodeCA #:extends NodeA)
(sig Person)
(sig Apple)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test *from bounds*, no instance generated yet

; Require atom to be only in top-level Node sig
(run testingAtomSigInference_top
      #:preds [(&& (in (atom 'Node0) Node)
                   (! (in (atom 'Node0) (+ NodeA NodeB NodeC))))]
      #:scope ([Node 3]))
(check-true (forge:Run? testingAtomSigInference_top))
(check-true (is-sat? testingAtomSigInference_top))
(check-equal? (list->set (infer-atom-type testingAtomSigInference_top (atom 'Node0)))
              ; infer-atom-type is based on kodkod-bounds, not the specific instance.
              ; thus, we only know that the atom is a _Node_. We are not using the
              ; added restrictions in the constraints (yet, anyway).
              (list->set '((Node_remainder)
                           (NodeA_remainder) 
                           (NodeAA)
                           (NodeBA)
                           (NodeCA)
                           (NodeB)
                           (NodeC))))

; Require atom to be only in mid-level NodeA sig.
; This will be the same, since inference is only based on upper bounds, and only queries
; top-level sigs anyway.
(run testingAtomSigInference_middle
      #:preds [(&& (in (atom 'Node0) NodeA)
                   (! (in (atom 'Node0) (+ NodeAA NodeBA NodeAA))))]
      #:scope ([Node 3]))
(check-true (forge:Run? testingAtomSigInference_middle))
(check-true (is-sat? testingAtomSigInference_middle))
(check-equal? (list->set (infer-atom-type testingAtomSigInference_middle (atom 'Node0)))
              ; See above for rationale; this is based on upper bounds not actual instance
              (list->set '((Node_remainder)
                           (NodeA_remainder)
                           (NodeAA)
                           (NodeBA)
                           (NodeCA)
                           (NodeB)
                           (NodeC))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test *from instance* that's been generated

(run testingAtomSigInference_frominst_top
      #:preds [(&& (in (atom 'Node0) Node)
                   (! (in (atom 'Node0) (+ NodeA NodeB NodeC))))]
      #:scope ([Node 3]))
(check-true (forge:Run? testingAtomSigInference_frominst_top))
(check-true (is-sat? testingAtomSigInference_frominst_top))

; GET INSTANCE AND PASS
(define top-gen (forge:make-model-generator (forge:get-result testingAtomSigInference_frominst_top) 'next))
(define inst-top (top-gen))
(check-equal? (list->set (infer-atom-type testingAtomSigInference_frominst_top (atom 'Node0) inst-top))
              (list->set '((Node_remainder)
                           (NodeA_remainder) 
                           (NodeAA)
                           (NodeBA)
                           (NodeCA)
                           (NodeB)
                           (NodeC))))

; PASS NOT THE SAT, BUT THE HASH INSIDE
;; ADD something lower in the hierarchy
