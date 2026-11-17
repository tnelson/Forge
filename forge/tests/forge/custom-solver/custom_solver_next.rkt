#lang forge/core

; Tests for the glue layer between a custom solver and Forge. Picosat is a known-good 
; solver, so any issues here would implicate, e.g., our Pardinus modifications.
; We check that the custom solver produces the same instances as SAT4J.

(require (only-in rackunit check-equal? check-true)
         (only-in racket/list empty? remove-duplicates)
         (only-in racket/file find-files)
         racket/runtime-path
         (prefix-in @ (only-in racket/base + >= length)))

(define-runtime-path here ".")
(define picosat-wrapper (path->string (build-path here "run_picosat.sh")))

(set-option! 'verbose 0)
(set-option! 'run_sterling 'off)

(sig Node)
(relation edges (Node Node))
(relation nxt (Node Node))

; Runs capture the solver option when declared.
(set-option! 'solver 'SAT4J)
(run refA #:preds [(some edges) (no nxt)] #:scope ([Node 2 2]))
(run refB #:preds [(all ([n Node]) (lone (join n nxt))) (no edges)] #:scope ([Node 1 2]))
(set-option! 'solver picosat-wrapper)
(run customA #:preds [(some edges) (no nxt)] #:scope ([Node 2 2]))
(run customB #:preds [(all ([n Node]) (lone (join n nxt))) (no edges)] #:scope ([Node 1 2]))

(define (generator r) (forge:make-model-generator (forge:get-result r) 'next))

; Guard against a solver that repeats an instance forever. 
(define MAX-INSTANCES 20)

; Takes up to k instances (as strings) from a generator.
(define (take-instances gen k)
  (let loop ([acc '()] [n 0])
    (if (@>= n (or k MAX-INSTANCES))
        (reverse acc)
        (let ([x (gen)])
          (if (forge:Sat? x)
              (loop (cons (format "~a" (forge:Sat-instances x)) acc) (@+ n 1))
              (reverse acc))))))

(define ref-a (take-instances (generator refA) #f))
(define ref-b (take-instances (generator refB) #f))

; Interleave the two custom-solver runs to check the streams don't interfere. 
(define gen-a (generator customA))
(define gen-b (generator customB))
(define a-first (take-instances gen-a 3))
(define b-first (take-instances gen-b 3))
(define custom-a (append a-first (take-instances gen-a #f)))
(define custom-b (append b-first (take-instances gen-b #f)))

(check-equal? (@length custom-a) (@length ref-a) "customA instance count matches SAT4J")
(check-equal? (@length custom-b) (@length ref-b) "customB instance count matches SAT4J")
(check-equal? (@length (remove-duplicates custom-a)) (@length custom-a) "customA has no duplicate instances")
(check-equal? (@length (remove-duplicates custom-b)) (@length custom-b) "customB has no duplicate instances")
(check-true (empty? (find-files (lambda (p) (regexp-match? #rx"customSolver-.*[.]temp$" (path->string p))) here))
            "no custom-solver CNF files left behind")
