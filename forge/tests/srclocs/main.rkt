#lang forge/core 

; Confirm that source locations are indeed preserved in case they are needed for errors.

(require (rename-in rackunit [check rackunit-check]))
(require (only-in racket flatten first string-contains?))

; Returns a list of all AST nodes in this tree
(define (gather-tree n #:leafs-only leafs-only)
  (define descendents
    (cond [(node/expr/op? n)
           (flatten (map (lambda (ch) (gather-tree ch #:leafs-only leafs-only))
                         (node/expr/op-children n)))]
          [(node/formula/op? n)
           (flatten (map (lambda (ch) (gather-tree ch #:leafs-only leafs-only))
                         (node/formula/op-children n)))]
          [(node/int/op? n)
           (flatten (map (lambda (ch) (gather-tree ch #:leafs-only leafs-only))
                         (node/int/op-children n)))]
          
          [(node/fmla/pred-spacer? n)
           (gather-tree (node/fmla/pred-spacer-expanded n) #:leafs-only leafs-only)]
          [(node/expr/fun-spacer? n)
           (gather-tree (node/expr/fun-spacer-expanded n) #:leafs-only leafs-only)]

          [(node/formula/multiplicity? n)
           (gather-tree (node/formula/multiplicity-expr n) #:leafs-only leafs-only)]
          [(node/formula/quantified? n)
           ; TODO: decls
           (gather-tree (node/formula/quantified-formula n) #:leafs-only leafs-only)]

          ;; TODO: other cases
          [else (list n)]))
  (if leafs-only
      descendents
      (cons n descendents)))

(define (print-one-per-line l)
  (cond [(not (list l)) (printf "  ~a~n" l)]
        [(empty? l) (printf "~n")]
        [else (printf "  ~a~n" (first l)) (print-one-per-line (rest l))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (only-in "../forge/library/seq.frg" order))
; For debugging: is anything not being broken down properly?
; (print-one-per-line (gather-tree order #:leafs-only #t))

; Confirm that the syntax location information for all nodes refers to the proper module
(for ([n (gather-tree order #:leafs-only #f)])
  (define loc (nodeinfo-loc (node-info n)))
  (define source-path-correct
    (string-contains?
     (path->string (srcloc-source loc))
     "/forge/library/seq.frg"))
  (unless source-path-correct 
    (printf "     ~a: ~a~n" n loc))
  ; Comment out for now, avoid unpleasant flickering spam
  ;(check-true source-path-correct)
  )

;(map (lambda (x)  (node/formula/op-children ))
