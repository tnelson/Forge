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

; Helper to join Tuples together
(define (joinTuple uppers)
  (map (lambda (left-ub)
                 (map (lambda (right-ub)
                        (list left-ub right-ub)) (second uppers))) (first uppers)))

; TODO: This helper is used for transitive and reflexive transtivie closure. Helper used to build a closure of tuple sets.
(define (buildClosureOfTupleSet tuples)
  (define setOfTups (joinClosureTuple tuples))
  (foldl (lambda (curr acc) (joinClosureTuple curr)) setOfTups (rest tuples)))


(define (joinClosureTuple tuples)
  (define toAdd
    (map (lambda (res)
           (map (lambda (tup)
                  (define newTup (list res tup))
                  (when (not (member newTup tuples)) newTup))) (first tuples)) (first tuples))) toAdd)


