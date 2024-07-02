#lang racket/base

; A utility function that collects values corresponding to matching AST nodes
; and returns them in a list. The traversal order can be partly controlled:
; pre-order and post-order traversals are supported. Breadth-first traversals
; are not currently supported, since the recursive structure would differ.
; Similarly, "in-order" traversal is not available since not all nodes are binary,
; and most binary operators (e.g., boolean "and") have n-ary variants.

; This utility might be used to collect all the quantifier variables used
; in a Forge constraint. It is more similar to `map`+`filter` than `fold`; to
; count the number of nodes, one would collect a marker for every node and then
; get the length of that resulting list.

; The matcher function that this utility takes should return #f if nothing
; corresponding to the given node should be collected. Any other value will
; be added to the result list, in the requested traversal order. If the matcher
; has side effects, pre/post-order call order is not guaranteed; only the list
; ordering of the results is guaranteed.

; Also, the ordering of internal traversals is consistent, but not adjustable:
; e.g., a quantified formula will always produce
;   (append <new variables> <collected from domains> <collected from inner formula>) 

; Finally, duplicates will not be removed. E.g., collecting on (& iden iden) will
; produce `iden` twice in the resulting list unless the matcher prevents it.

(require 
  forge/sigs-structs
  forge/lang/ast
  forge/shared
  (only-in racket index-of match string-join first second rest)
  (only-in racket/contract define/contract or/c listof any/c one-of/c)
  (prefix-in @ (only-in racket/contract ->))
  (prefix-in @ (only-in racket/base >=)))

(provide collect)

; TODO: At the moment, the matcher will not be given quantifier-scope information.
; TODO: There is no means to control whether the visitor descends into the record
;   portion of a spacer node; at the moment only actually-used-in-constraint nodes
;   are visited.

(define/contract (collect node matcher #:order order)
  (@-> node? (@-> node? any/c) #:order (one-of/c 'pre-order 'post-order)
       (listof any/c))
  (visit node '() matcher order '()))

(define (visit node quantvars matcher order collected)
  (define matched? (matcher node))
  (define collected-within
    (cond
      [(node/formula? node) (interpret-formula node '() matcher order collected)]
      [(node/expr? node)    (interpret-expr node '() matcher order collected)]
      [(node/int? node)     (interpret-int node '() matcher order collected)]
      [else (raise-forge-error #:msg (format "Collector was given a non-node input: ~a" node)
                               #:context node)]))
  (cond [(and matched? (equal? order 'pre-order))
         (cons matched? collected-within)]
        [(and matched? (equal? order 'post-order))
         (append collected-within (list matched?))]
        [else collected-within]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Boolean formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Translate a formula AST node
(define (interpret-formula formula quantvars matcher order collected)  
  (when (@>= (get-verbosity) 2)
    (printf "collector: interpret-formula: ~a~n" formula))
  (match formula
    [(node/formula/constant info type)
     '()]    
    [(node/fmla/pred-spacer info name args expanded)
     (visit expanded quantvars matcher order collected)]
    [(node/formula/op info args)
     (interpret-formula-op formula quantvars args)]
    [(node/formula/multiplicity info mult expr)
     (visit expr quantvars matcher order collected)]
    [(node/formula/quantified info quantifier decls form)
     
     (define new-vs-and-collected
       (for/fold ([vs-and-collected (list '() '())])
                 ([decl decls])
         (define curr-new-quantvars (first vs-and-collected))
         (define new-quantvars (cons (car decl) curr-new-quantvars))
         (define new-domain-collected
           (visit (cdr decl) (append curr-new-quantvars new-quantvars) matcher order collected))         
         (list new-quantvars new-domain-collected)))
     
     (define new-quantvars (first new-vs-and-collected))
     (define new-domain-collected (second new-vs-and-collected))
     (define form-collected (visit form (append new-quantvars quantvars) matcher order collected))
     (append new-quantvars new-domain-collected form-collected)]
    [(node/formula/sealed info)
     (visit info quantvars matcher order collected)]
    [#t '()]
    [#f '()]))

(define (interpret-formula-op formula quantvars args matcher order collected)
  (define (process-children children quantvars)
    (apply append (map (lambda (x) (visit x quantvars matcher order collected)) children)))

  (when (@>= (get-verbosity) 2)
    (printf "collector: interpret-formula-op: ~a~n" formula))
  ; We could get away with only one case here, really, since there's no distinguishing
  ; but leaving the structure here for now in case we need it for any refinement.
  (match formula
    [(node/formula/op/&& info children)
      (process-children args quantvars)]
    [(node/formula/op/|| info children)
     (process-children args quantvars)]
    [(node/formula/op/=> info children)
     (process-children args quantvars)]
    [(node/formula/op/always info children)
     (process-children args quantvars)]
    [(node/formula/op/eventually info children)
     (process-children args quantvars)]
    [(node/formula/op/next_state info children)
     (process-children args quantvars)]
    [(node/formula/op/releases info children)
     (process-children args quantvars)]
    [(node/formula/op/until info children)
     (process-children args quantvars)]
    [(node/formula/op/historically info children)
     (process-children args quantvars)]
    [(node/formula/op/once info children)
     (process-children args quantvars)]
    [(node/formula/op/prev_state info children)
     (process-children args quantvars)]
    [(node/formula/op/since info children)
     (process-children args quantvars)]
    [(node/formula/op/triggered info children)
     (process-children args quantvars)]
    [(node/formula/op/in info children)
     (process-children args quantvars)]
    [(node/formula/op/= info children)
     (process-children args quantvars)]
    [(node/formula/op/! info children)
     (process-children args quantvars)]
    [(node/formula/op/int> info children)
     (process-children args quantvars)]
    [(node/formula/op/int< info children)
     (process-children args quantvars)]
    [(node/formula/op/int= info children)
     (process-children args quantvars)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Relational expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interpret-expr expr quantvars matcher order collected)
  (when (@>= (get-verbosity) 2)
      (printf "collector: interpret-expr: ~a~n" expr))
  (match expr
    [(node/expr/relation info arity name typelist-thunk parent isvar)
     '()]
    [(node/expr/atom info arity name)
     '()]
    [(node/expr/fun-spacer info arity name args result expanded)
     (visit expanded quantvars matcher order collected)]
    [(node/expr/ite info arity a b c) 
    (let ([processed-a (visit a quantvars matcher order collected)]
          [processed-b (visit b quantvars matcher order collected)]
          [processed-c (visit c quantvars matcher order collected)])
     (append a b c))]
    [(node/expr/constant info 1 'Int)
     '()]
    [(node/expr/constant info arity type)
     '()]
    [(node/expr/op info arity args)
     (interpret-expr-op expr quantvars args matcher order collected)]
    [(node/expr/quantifier-var info arity sym name)  
     '()]
    [(node/expr/comprehension info len decls form)
     (define new-vs-and-collected
       (for/fold ([vs-and-collected (list '() '())])
                 ([decl decls])
         (define curr-new-quantvars (first vs-and-collected))
         (define new-quantvars (cons (car decl) curr-new-quantvars))
         (define new-domain-collected
           (visit (cdr decl) (append curr-new-quantvars new-quantvars) matcher order collected))         
         (list new-quantvars new-domain-collected)))
     
     (define new-quantvars (first new-vs-and-collected))
     (define new-domain-collected (second new-vs-and-collected))
     (define form-collected (visit form (append new-quantvars quantvars) matcher order collected))
     (append new-quantvars new-domain-collected form-collected)]))

(define (interpret-expr-op expr quantvars args matcher order collected)
  (define (process-children children quantvars)
    (apply append (map (lambda (x) (visit x quantvars matcher order collected)) children)))

  (when (@>= (get-verbosity) 2)
    (printf "collector: interpret-expr-op: ~a~n" expr))
  ; We could get away with only one case here, really, since there's no distinguishing
  ; but leaving the structure here for now in case we need it for any refinement.
  (match expr
    [(node/expr/op/+ info arity children)
     (process-children args quantvars)]
    [(node/expr/op/- info arity children)
     (process-children args quantvars)]
    [(node/expr/op/& info arity children)
     (process-children args quantvars)]
    [(node/expr/op/-> info arity children)
     (process-children args quantvars)]
    [(node/expr/op/prime info arity children)
     (process-children args quantvars)]
    [(node/expr/op/join info arity children)
     (process-children args quantvars)]
    [(node/expr/op/^ info arity children)
     (process-children args quantvars)]
    [(node/expr/op/* info arity children)
    (process-children args quantvars)]
    [(node/expr/op/~ info arity children)
     (process-children args quantvars)]
    [(node/expr/op/++ info arity children)
     (process-children args quantvars)]
    [(node/expr/op/sing info arity children)
     (process-children args quantvars)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Integer expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interpret-int expr quantvars matcher order collected)
  (when (@>= (get-verbosity) 2)
    (printf "collector: interpret-int: ~a~n" expr))
  (match expr
    [(node/int/constant info value)
     '()]
    [(node/int/op info args)
     (interpret-int-op expr quantvars args matcher order collected)]
    [(node/int/sum-quant info decls int-expr)
     (define new-vs-and-collected
       (for/fold ([vs-and-collected (list '() '())])
                 ([decl decls])
         (define curr-new-quantvars (first vs-and-collected))
         (define new-quantvars (cons (car decl) curr-new-quantvars))
         (define new-domain-collected
           (visit (cdr decl) (append curr-new-quantvars new-quantvars) matcher order collected))         
         (list new-quantvars new-domain-collected)))
    
     (define new-quantvars (first new-vs-and-collected))
     (define new-domain-collected (second new-vs-and-collected))     
     (define inner-collected (visit int-expr new-quantvars matcher order collected))
     (append new-quantvars new-domain-collected inner-collected)]))

(define (interpret-int-op expr quantvars args matcher order collected)
  (define (process-children children quantvars)
    (apply append (map (lambda (x) (visit x quantvars matcher order collected)) children)))

  (when (@>= (get-verbosity) 2)
    (printf "collector: interpret-int-op: ~a~n" expr))

  ; We could get away with only one case here, really, since there's no distinguishing
  ; but leaving the structure here for now in case we need it for any refinement.
  (match expr
    [(node/int/op/add info children)
     (process-children args quantvars)]
    [(node/int/op/subtract info children)
     (process-children args quantvars)]
    [(node/int/op/multiply info children)
     (process-children args quantvars)]
    [(node/int/op/divide info children)
     (process-children args quantvars)]
    [(node/int/op/sum info children)
     (process-children args quantvars)]
    [(node/int/op/card info children)
     (process-children args quantvars)]
    [(node/int/op/remainder info children)
     (process-children args quantvars)]
    [(node/int/op/abs info children)
     (process-children args quantvars)]
    [(node/int/op/sign info children)
     (process-children args quantvars)]
    [(node/int/sum-quant info decls int-expr)
     (raise-forge-error #:msg "Reached expected unreachable code." #:context expr)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Rackunit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Very basic smoke tests
(module+ test
  (require rackunit)
  ; Get all nodes, post-order
  (check-equal?
   (collect (some univ) (lambda (n) n) #:order 'post-order)
   (list univ (some univ)))
  
  ; Get all nodes, pre-order
  (check-equal?
   (collect (some univ) (lambda (n) n) #:order 'pre-order)
   (list (some univ) univ))
  
  ; Filter only expressions. Note that separate construction of quantifiers
  ; produces different variables, hence the let*.
  (let* ([fmla (all ([x univ]) (some (& (-> x x) iden)))]
         [v (car (first (node/formula/quantified-decls fmla)))])
  (check-equal?
   (collect fmla
            (lambda (n) (if (node/expr? n) n #f)) #:order 'pre-order)
   (list v univ (& (-> v v) iden) (-> v v) v v iden))))

