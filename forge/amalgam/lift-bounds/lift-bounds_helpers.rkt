#lang forge/core
(require debug/repl)
(provide transposeTup joinTuple buildClosureOfTupleSet)

; Helper used to flip the currentTupleIfAtomic in the transpose case 
(define (transposeTup tuple)
  (cond 
    [(equal? (length tuple) 2) (list (second tuple) (first tuple))]
    [else (error "transpose tuple for tup ~a isn't arity 2. It has arity ~a"
                 tuple (length tuple))]))


; Helper to join Tuples together
; list<tuple>, list<tuple> -> list<tuple>
(define (joinTuple leftTS rightTS)
  (define allPairs (cartesian-product leftTS rightTS))
  (filter-map (lambda (combo)
                (define-values (t1 t2) (values (first combo) (second combo)))
                (cond [(equal? (last t1) (first t2))
                       (append (take t1 (- (length t1) 1)) (rest t2))]
                      [else #f]))
              allPairs))

; This helper is used for transitive and reflexive transtivie closure.
;   Helper used to build a closure of tuple sets.
(define (buildClosureOfTupleSet tuples)
  (define selfJoin (joinTuple tuples tuples))
  ; appending tuples always keep the prior-hop-count tuples in the set
  (define newCandidate (remove-duplicates (append tuples selfJoin)))
  (cond [(equal? (length newCandidate) (length tuples))
         tuples]
        [else
         (buildClosureOfTupleSet newCandidate)]))

; used by desugar. similar logic to buildClosureOfTupleSet
; given upper-bound on possible tuples, constructs all possible
; paths from <prefix> using <edges>
; TODO: account for <end>
; to build set of paths starting with t0 using upperbnd UBA: (extendPossiblePaths UBA (list t0))

; to produce desugared fmla:
;   '((1 2 3) (1 3 4) (1 2 5) (1 3 5))
; first filter to desired end point (e.g., 5)
;   '((1 2 5) (1 3 5))
; OR:
;   can follow: 1 to 2 to 5
;   can follow: 1 to 3 to 5
; OR:
;   AND: edge(1,2) edge(2,5)
;   AND: edge(1,3) edge(3,5) 

(define (extendPossiblePaths edges prefix)    
  (define newSimplePaths
    (filter-map
     (lambda (e)
       (define-values (e0 e1) (values (first e) (second e)))
       ; TODO: SO INEFFICIENT :-( but just write it
       ;  might be better to build paths in reverse from t1 than forward from t0
       ;  or use something other than lists for everything here
       (cond [(member e1 prefix) #f] ; only build simple paths
             [(equal? (last prefix) e0) (append prefix (list e1))]
             [else #f]))
     edges))
  ;(printf "new simple paths: ~a~n" newSimplePaths)
  
  (cond [(empty? newSimplePaths)
         empty]
        [else
         ; keep the new simple paths, but also try to extend them again
         (append newSimplePaths
          (apply append (map (lambda (newPrefix) (extendPossiblePaths edges newPrefix)) newSimplePaths)))]))

; (extendPossiblePaths '() '(1))
; (extendPossiblePaths '((1 2)) '(1))
; (extendPossiblePaths '((1 2) (2 3) (1 5) (4 7)) '(1))
; (extendPossiblePaths '((1 2) (2 3) (1 5) (4 7) (7 8) (2 5) (2 2)) '(1))
;(extendPossiblePaths (cartesian-product '(0 1 2 3 4 5 6 7 8 9) '(0 1 2 3 4 5 6 7 8 9)) '(1))
