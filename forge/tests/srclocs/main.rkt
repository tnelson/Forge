#lang forge/core 

; Confirm that source locations are indeed preserved in case they are needed for errors.

(require (only-in rackunit check-equal?))
(require (only-in racket flatten first))

; Returns a list of all AST nodes in this tree
(define (gather-tree n)
  (cond [(node/expr/op? n)
         (flatten (map gather-tree (node/expr/op-children n)))]
        [(node/formula/op? n)
         (flatten (map gather-tree (node/formula/op-children n)))]
        [(node/fmla/pred-spacer? n)
         (gather-tree (node/fmla/pred-spacer-expanded n))]
        [(node/expr/fun-spacer? n)
         (gather-tree (node/expr/fun-spacer-expanded n))]
        [(node/formula/quantified? n)
         ; TODO: decls
         (gather-tree (node/formula/quantified-formula n))]
        ;; TODO: other cases
        [else (list n)]))
(define (print-one-per-line l)
  (cond [(not (list l)) (printf "  ~a~n" l)]
        [(empty? l) (printf "~n")]
        [else (printf "  ~a~n" (first l)) (print-one-per-line (rest l))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (only-in "../forge/library/seq.frg" order))
(print-one-per-line (gather-tree order))

;(map (lambda (x) (nodeinfo-loc (node-info x))) (node/formula/op-children ))
