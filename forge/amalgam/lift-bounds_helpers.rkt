#lang forge/core
(require debug/repl)
(provide transposeTup create-bitwidth-list zip)

; return list of lists inclusive of start and end
(define (create-bitwidth-list start end)
  (cond
    [(equal? start (+ end 1)) '()]
    [else
     (cons (list start) (create-bitwidth-list (+ 1 start) end))]))

; Helper used to flip the currentTupleIfAtomic in the transpose case 
(define (transposeTup tuple)
  (cond 
    [(equal? (length tuple) 2) (list (second tuple) (first tuple))]
    [else (error "transpose tuple for tup ~a isn't arity 2. It has arity ~a" tuple (length tuple))]))

; Zip method used to get the join between bounds in lift-bounds.rkt.
; This method works as follows: it takes in two lists, l1 and l2,
; where l1 = '(1 2) l2 = '(3 4) and returns '((1 3) (2 4))
(define zip (lambda (l1 l2) (map list l1 l2)))