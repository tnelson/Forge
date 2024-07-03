#lang racket/base

; Very basic smoke tests for Skolemization. This is its own module to avoid a cyclic
; dependency via sigs-functional requiring send-to-solver. Using the forge/functional
; library to build sigs, etc. to avoid any potential tangling of state. 

(require rackunit)
(require forge/sigs-functional)
(require forge/utils/to-skolem)
(require forge/lang/bounds)
(require forge/utils/collector
         forge/utils/substitutor)
(require (only-in racket cartesian-product first second rest empty?))

(define Node (make-sig 'Node))
(define edges (make-relation 'edges (list Node Node)))
(define x (var 'x))
(define y (var 'y))
(define z (var 'z))

(define fake-spec (Run-spec init-state '() (Scope #f #f (hash))
                            (Bound (hash) (hash) (hash) (hash)) #f))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Check "base case" -- the skolemization itself. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; In skolemize-formula, the quantvars are the _universals only_ and 
; the decls are the _existentials_ to be Skolemized.


;run-spec total-bounds formula relations atom-names
;         quantvars quantvar-types info decls form
;         tag-with-spacer

; Skolem depth = 0
(let* ([atom-names '(Node0 Node1 Node2)]
       [relations (list Node edges)]
       [inner (in x (join y edges))]
       [outer (node/formula/quantified empty-nodeinfo 'some (list (cons x Node)) inner)]
       [total-bounds (list (bound Node '() (map list atom-names))
                           (bound edges '() (cartesian-product atom-names atom-names)))])

  ; Try without tag-with-spacer
  (define-values (f1 b1)
    (skolemize-formula-helper fake-spec total-bounds outer
                             (list Node edges) atom-names '() '()
                             empty-nodeinfo (list (cons x Node)) inner #:tag-with-spacer #f))
  (check-equal? f1
                (in (rel '(Node) 'Node "$x") (join y edges)))
  (check-equal? (length (filter (lambda (b) (equal? "$x" (relation-name (bound-relation b)))) b1))
                1)

  ; Try with tag-with-spacer
  (define-values (f2 b2)
    (skolemize-formula-helper fake-spec total-bounds outer
                             relations atom-names '() '()
                             empty-nodeinfo (list (cons x Node)) inner #:tag-with-spacer #t))
  (define spacers (collect f2 (lambda (n) (if (node/expr/fun-spacer? n) n #f)) #:order 'pre-order))
  (check-equal? (length spacers) 1)
  (define edited (substitute-formula fake-spec f2 relations atom-names '()
                                     (first spacers) (node/expr/fun-spacer-expanded (first spacers))))
    
  (check-equal? edited
                (in (rel '(Node) 'Node "$x") (join y edges)))
  (check-equal? (length (filter (lambda (b) (equal? "$x" (relation-name (bound-relation b)))) b2))
                1))

;; TODO: test skolem depth > 0

;; TODO: above is brittle testing because it relies on a helper that Skolemization uses.
;;   we need a separate test suite for substitutor.


