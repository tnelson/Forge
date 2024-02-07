#lang forge/core


; These won't define lang checker hash
;#lang racket/base
;#lang racket 
(require syntax/parse/define (only-in racket/set list->set)
         (rename-in (only-in racket/base *) [* @*]))
(require (prefix-in @ rackunit))
(require (for-syntax racket/base racket/syntax))

; Tests for target-oriented interface. The tests in ../forge/target/ test the _forge language_
; for target _types_, but not for providing a specific target. Hence this module.

(sig Node)
(relation edges (Node Node))

; TODO: error if solver not appropriate for target-orientation
; TODO: error if solver name not valid

; Because the macro below uses forge/functional, not forge/core, stateful option setters have no
; effect; we need to create Options ourselves
(define the-options
  (struct-copy forge:Options forge:DEFAULT-OPTIONS
               [problem_type 'target]
               [solver 'PMaxSAT4J]))
  

; Create a Run, but don't open a visualizer
; We cannot use "test" here; we need to view the contents of the instance itself.
; Also, the "run" macro is a bit annoying to use here, so just use forge/functional.
(define-syntax (run-graph-test stx)
  (syntax-parse stx
    [(run-graph-test in-nodes in-edges in-distance in-preds node-scope)
     (syntax/loc stx
         (begin 
           (define target-run
             (make-run #:name (gensym)
                       #:preds '()
                       #:sigs (list Node)
                       #:relations (list edges)
                       #:scope (list (list Node node-scope))
                       #:options the-options
                       #:target (forge:Target (hash 'Node in-nodes 
                                                    'edges in-edges)
                                              in-distance)))
           
           (define target-gen (forge:make-model-generator (forge:get-result target-run) 'next))
           (define target-soln (target-gen))
           (define target-inst (first (Sat-instances target-soln)))
           (cond
             [(equal? 'close_noretarget in-distance)
              (@check-equal? (list->set (hash-ref target-inst 'Node)) (list->set in-nodes))
              (@check-equal? (list->set (hash-ref target-inst 'edges)) (list->set in-edges))]
             [else
              (@check-equal? (length (hash-ref target-inst 'Node)) node-scope)
              (@check-equal? (length (hash-ref target-inst 'edges)) (@* node-scope node-scope))])))]))


(run-graph-test '((Node0) (Node1))
                '((Node0 Node1) (Node1 Node0) (Node0 Node0) (Node1 Node1))
                'close_noretarget
                '()
                2)

(run-graph-test '()
                '()
                'close_noretarget
                '()
                3)

(run-graph-test '()
                '()
                'far_noretarget
                '()
                3)


(run-graph-test '((Node0) (Node1) (Node2))
                '((Node0 Node1) (Node1 Node2))
                'close_noretarget
                '()
                3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; One standalone check for the run macro, since the above all use make-run

(set-option! 'solver 'PMaxSAT4J)
(set-option! 'problem_type 'target)

(run target-oriented-run-macro-test
     #:preds []
     #:scope ([Node 2])
     #:target (hash 'Node '()
                    'edges '())
     #:target-distance close_noretarget)

(define target-gen (forge:make-model-generator (forge:get-result target-oriented-run-macro-test) 'next))
(define target-soln (target-gen))
(define target-inst (first (Sat-instances target-soln)))
(@check-equal? (hash-ref target-inst 'Node) empty)
(@check-equal? (hash-ref target-inst 'edges) empty)
