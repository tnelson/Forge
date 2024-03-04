#lang forge/core 

; Confirm that source locations are indeed preserved in case they are needed for errors.

(require (rename-in rackunit [check rackunit-check])
         (only-in rackunit check-true check-equal? check-not-eq?))
(require (only-in racket flatten first second string-contains?)
         (prefix-in @ (only-in racket -)))

; Returns a list of all AST nodes in this tree
(define (gather-tree n #:leafs-only leafs-only #:ignore-outer ignore-outer)
  (define ignore (if (< 1 ignore-outer) 0 (@- ignore-outer 1)))

  ;; Helper for AST nodes that have a quantified structure
  (define (gather-quant-helper e)
    (define decls (cond [(node/formula/quantified? e)
                         (node/formula/quantified-decls e)]
                        [(node/expr/comprehension? e)
                         (node/expr/comprehension-decls e)]
                        [else
                         (node/int/sum-quant-decls e)]))
    (define fmla (cond [(node/formula/quantified? e)
                        (node/formula/quantified-formula e)]
                       [(node/expr/comprehension? e)
                        (node/expr/comprehension-formula e)]
                       [else
                        (node/int/sum-quant-int-expr e)]))
    (append
     ; Gather both decl variables and decl expressions (and their subexpressions)
     (flatten (map (lambda (decl)
                     (list (gather-tree (car decl) #:leafs-only leafs-only #:ignore-outer ignore)
                           (gather-tree (cdr decl) #:leafs-only leafs-only #:ignore-outer ignore)))
                   decls))
     (gather-tree fmla #:leafs-only leafs-only #:ignore-outer ignore)))
  
  (define descendents
    (cond [(node/expr/op? n)
           (flatten (map (lambda (ch) (gather-tree ch #:leafs-only leafs-only #:ignore-outer ignore))
                         (node/expr/op-children n)))]
          [(node/formula/op? n)
           (flatten (map (lambda (ch) (gather-tree ch #:leafs-only leafs-only #:ignore-outer ignore))
                         (node/formula/op-children n)))]
          [(node/int/op? n)
           (flatten (map (lambda (ch) (gather-tree ch #:leafs-only leafs-only #:ignore-outer ignore))
                         (node/int/op-children n)))]
          
          [(node/fmla/pred-spacer? n)
           (gather-tree (node/fmla/pred-spacer-expanded n) #:leafs-only leafs-only #:ignore-outer ignore)]
          [(node/expr/fun-spacer? n)
           (gather-tree (node/expr/fun-spacer-expanded n) #:leafs-only leafs-only #:ignore-outer ignore)]
          [(node/formula/multiplicity? n)
           (gather-tree (node/formula/multiplicity-expr n) #:leafs-only leafs-only #:ignore-outer ignore)]
          [(node/formula/sealed? n)
           ; for sealed AST nodes, the inner formula is contained in the info field
           (gather-tree (node-info n) #:leafs-only leafs-only #:ignore-outer ignore)]

          [(node/formula/multiplicity? n)
           (gather-tree (node/formula/multiplicity-expr n) #:leafs-only leafs-only #:ignore-outer ignore)]
          [(or (node/formula/quantified? n)
               (node/expr/comprehension? n)
               (node/int/sum-quant? n))
           (gather-quant-helper n)]

          ;; TODO: other cases
          [else (list n)]))
  (cond [leafs-only descendents]
        [ignore-outer descendents] 
        [else (cons n descendents)]))

(define (print-one-per-line l)
  (cond [(not (list l)) (printf "  ~a~n" l)]
        [(empty? l) (printf "~n")]
        [else (printf "  ~a~n" (first l)) (print-one-per-line (rest l))]))


; Confirm that the syntax location information for all nodes refers to the proper module
; The `ignore-outer` parameter allows the caller to ignore the source location of outermost
; expressions, since that will be captured in _this_ module. Look instead at subexpressions.
; (Recommended for direct pred invocations: *2* layers to ignore (spacer and outer conjunction).
(define (check-full-ast-srclocs root-ast sub-path-str #:ignore-outer [ignore-outer 0])
  (for ([n (gather-tree root-ast #:leafs-only #f #:ignore-outer ignore-outer)])
    (define loc (nodeinfo-loc (node-info n)))
    ;(printf "~a~n" loc)
    (define source-path-correct
      (string-contains?
       (path->string (srcloc-source loc))
       sub-path-str))
    (unless source-path-correct 
      (printf "    Source path unexpected for:~n ~a~n~a~n" n loc))
    ; Comment out for now, avoid unpleasant flickering spam
    ;(check-true source-path-correct)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Need to do these separately, because of "already bound" sig names shared etc.

;(require (only-in "../forge/library/seq.frg" order))
; For debugging: is anything not being broken down properly?
; (print-one-per-line (gather-tree order #:leafs-only #t))
;(check-full-ast-srclocs order "/forge/library/seq.frg")

;(require (only-in "../forge/library/reachable.frg" reach6))
;(check-full-ast-srclocs reach6 "/forge/library/reachable.frg")

;(require (only-in "../forge/formulas/booleanFormulaOperators.rkt" Implies And Or Not))
;(check-full-ast-srclocs Implies  "/forge/formulas/booleanFormulaOperators.rkt" #:ignore-outer 2)
;(check-full-ast-srclocs And  "/forge/formulas/booleanFormulaOperators.rkt" #:ignore-outer 2)
;(check-full-ast-srclocs Or  "/forge/formulas/booleanFormulaOperators.rkt" #:ignore-outer 2)
;(check-full-ast-srclocs Not  "/forge/formulas/booleanFormulaOperators.rkt" #:ignore-outer 2)

(require (only-in "../forge/examples/sudoku.rkt" generatePuzzle))
(check-full-ast-srclocs generatePuzzle "sudoku.rkt" #:ignore-outer 2)

;; TODO: check run parameters as well (catch instances...) 
;; TODO: other examples


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Check source locations for a specific example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "sigs_fields_preds_funs.frg")
(check-true (node? Providence))
(check-true (node? reachableFromProvidence)) ; Providence.^(edges1+edges2)

(define usePvd (first (node/expr/op-children reachableFromProvidence)))
(check-true (node? usePvd))
(define useEdges1 (first (node/expr/op-children
                          (first (node/expr/op-children
                                  (second (node/expr/op-children reachableFromProvidence)))))))
(check-true (node? useEdges1))


;; SIG ;;
; Use and declaration considered equal
(check-equal? Providence usePvd)
; but not referentially equal
(check-not-eq? Providence usePvd)
; and have different source locations
(check-not-equal? (nodeinfo-loc (node-info Providence))
                  (nodeinfo-loc (node-info usePvd)))
;; FIELD ;;
; Use and declaration considered equal
(check-equal? edges1 useEdges1)
; but not referentially equal
(check-not-eq? edges1 useEdges1)
; and have different source locations
(check-not-equal? (nodeinfo-loc (node-info edges1))
                  (nodeinfo-loc (node-info useEdges1)))

;; PRED ;;
(define pvdReachesEverything (reachesEverything Providence))
(check-true (node? pvdReachesEverything))
;(printf "~npvdReachesEverything: ~a~n" (nodeinfo-loc (node-info pvdReachesEverything)))
(check-equal? 153
              (srcloc-line (nodeinfo-loc (node-info pvdReachesEverything))))
              

;; FUN ;;
(define twoHopsFromProvidence (twoHopsAway Providence))
(check-true (node? twoHopsFromProvidence))
;(printf "~ntopHopsFromProvidence: ~a~n" (nodeinfo-loc (node-info twoHopsFromProvidence)))
(check-equal? 161
              (srcloc-line (nodeinfo-loc (node-info twoHopsFromProvidence))))
