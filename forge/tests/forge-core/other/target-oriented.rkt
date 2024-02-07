#lang forge/core
(set-option! 'verbose 0)

; Tests for target-oriented interface. The tests in ../forge/target/ test the _forge language_
; for target _types_, but not for providing a specific target. Hence this module. 

(require (prefix-in @ rackunit))

(sig Node)
(relation edges (Node Node))

(set-option! 'verbose 5)
(set-option! 'solver 'PMaxSAT4J)
(set-option! 'problem_type 'target)

; TODO: error if solver not appropriate for target-orientation
; TODO: error if solver name not valid

; TODO: parser issue

; Create a Run, but don't open a visualizer
; We cannot use "test" here; we need to view the contents of the instance itself. 
;; (run target-close-empty
;;      #:preds []
;;      #:target (hash 'Node '() 'edges '())
;;      #:target-distance close)
;; (define target-close-empty-gen (forge:make-model-generator (forge:get-result target-close-empty) 'next))
;; (define target-close-empty-inst1 (target-close-empty-gen))

(run target-close-biggest-2
     #:preds []
     #:target (hash 'Node '((Node0) (Node1))
                    'edges '( (Node0 Node1) (Node1 Node0) (Node0 Node0) (Node1 Node1)))
     #:target-distance close)
(define target-close-biggest-2-gen (forge:make-model-generator (forge:get-result target-close-biggest-2) 'next))
(define target-close-biggest-2-inst1 (target-close-biggest-2-gen))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  
;; (@check-equal?
;;  (length (forge:bound-upper (first (filter (lambda (x) (equal? A (forge:bound-relation x))) (forge:Run-kodkod-bounds myRun)))))
;;  1
;;  "#:one sigs should have exactly one element in their upper bound")
;;  
;; (@check-equal?
;;  (forge:bound-upper (first (filter (lambda (x) (equal? A (forge:bound-relation x))) (forge:Run-kodkod-bounds myRun))))
;;  (forge:bound-lower (first (filter (lambda (x) (equal? A (forge:bound-relation x))) (forge:Run-kodkod-bounds myRun))))
;;  "#:one sigs should be exact-bounded")
;; 
;;  
;; (@check-not-equal?
;;  (forge:bound-upper (first (filter (lambda (x) (equal? A (forge:bound-relation x))) (forge:Run-kodkod-bounds myRun))))
;;  (forge:bound-upper (first (filter (lambda (x) (equal? B (forge:bound-relation x))) (forge:Run-kodkod-bounds myRun))))
;;  "Upper bounds between #:one siblings should never overlap")
;; 
;; ; Safety check: Regardless of what we think bounds do, confirm that overlap is impossible
;; (test oneSigsCannotOverlap #:preds [(some (& A B))] #:expect unsat)
