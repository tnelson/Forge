#lang forge/core
(require debug/repl)
(provide transposeTup joinTuple buildClosureOfTupleSet joinClosureTuple)

; Helper used to flip the currentTupleIfAtomic in the transpose case 
(define (transposeTup tuple)
  (cond 
    [(equal? (length tuple) 2) (list (second tuple) (first tuple))]
    [else (error "transpose tuple for tup ~a isn't arity 2. It has arity ~a" tuple (length tuple))]))


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
  (printf "in joinClosureTuple~n")
  (define toAdd
    (map (lambda (res)
           (map (lambda (tup)
                  (define newTup (list res tup))
                  (when (not (member newTup tuples)) newTup)) (first tuples))) (first tuples))) toAdd)


