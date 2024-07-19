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

; If the optional stop argument returns a non-#f value for a node, that node's
; children will not be explored, although the node itself may be matched and
; retained in the return value. The stop policy is separate from the matcher.

; The optional get-new-context argument is a lambda that returns a new context value to
; be passed to the recursive call on the current node's children, if they are visited. 
; The default preserves the current context, if any. The optional context argument 
; gives the initial value of that context, defaulting to #f. 

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
  (prefix-in @ (only-in racket/contract -> ->*))
  (prefix-in @ (only-in racket/base >=)))

(provide collect)

; TODO: At the moment, the matcher will not be given quantifier-scope information.
; TODO: There is no means to control whether the visitor descends into the record
;   portion of a spacer node; at the moment only actually-used-in-constraint nodes
;   are visited.

(define/contract (collect node matcher #:order order
                          #:stop [stop (lambda (n ctxt) #f)]
                          #:context [context #f]
                          #:get-new-context [get-new-context (lambda (n ctxt) ctxt)])
  (@->* (node?                                       ; node being visited
         (@-> node? any/c any/c)                     ; matcher function (node, context) -> collected-value
         #:order (one-of/c 'pre-order 'post-order))  ; traversal order symbol
        (#:stop (@-> node? any/c boolean?)           ; (optional) stopper predicate (node, context) -> bool
         #:context any/c                             ; (optional) current context value 
         #:get-new-context (@-> node? any/c any/c))  ; (optional) context-update function (node, context) -> context
        (listof any/c))                              ; result is an arbitrary list of collected values
  
  (visit node '() matcher order '() stop context get-new-context))

(define (visit node quantvars matcher order collected stop context get-new-context)
  (define matched? (matcher node context))
  (define stop? (stop node context))
  (define new-context (get-new-context node context))
  (cond
    [stop? (if matched? (list matched?) '())]
    [else 
     (define collected-within
       (cond
         [(node/formula? node) (interpret-formula node '() matcher order collected stop new-context get-new-context)]
         [(node/expr? node)    (interpret-expr node '() matcher order collected stop new-context get-new-context)]
         [(node/int? node)     (interpret-int node '() matcher order collected stop new-context get-new-context)]
         [else (raise-forge-error #:msg (format "Collector was given a non-node input: ~a" node)
                                  #:context node)]))
     (cond [(and matched? (equal? order 'pre-order))
            (cons matched? collected-within)]
           [(and matched? (equal? order 'post-order))
            (append collected-within (list matched?))]
           [else collected-within])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Boolean formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Translate a formula AST node
(define (interpret-formula formula quantvars matcher order collected stop context get-new-context)  
  (when (@>= (get-verbosity) 2)
    (printf "collector: interpret-formula: ~a~n" formula))
  (match formula
    [(node/formula/constant info type)
     '()]    
    [(node/fmla/pred-spacer info name args expanded)
     (visit expanded quantvars matcher order collected stop context get-new-context)]
    [(node/formula/op info args)
     (interpret-formula-op formula quantvars args matcher order collected stop context get-new-context)]
    [(node/formula/multiplicity info mult expr)
     (visit expr quantvars matcher order collected stop context get-new-context)]
    [(node/formula/quantified info quantifier decls inner-form)
     (process-quant-shaped-node formula decls inner-form quantvars matcher order collected stop context get-new-context)]
    [(node/formula/sealed info)
     (visit info quantvars matcher order collected stop context get-new-context)]
    [#t '()]
    [#f '()]))

(define (interpret-formula-op formula quantvars args matcher order collected stop context get-new-context)
  (define (process-children children quantvars)
    (apply append (map (lambda (x) (visit x quantvars matcher order collected stop context get-new-context)) children)))

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

(define (interpret-expr expr quantvars matcher order collected stop context get-new-context)
  (when (@>= (get-verbosity) 2)
      (printf "collector: interpret-expr: ~a~n" expr))
  (match expr
    [(node/expr/relation info arity name typelist-thunk parent isvar)
     '()]
    [(node/expr/atom info arity name)
     '()]
    [(node/expr/fun-spacer info arity name args result expanded)
     (visit expanded quantvars matcher order collected stop context get-new-context)]
    [(node/expr/ite info arity a b c) 
    (let ([processed-a (visit a quantvars matcher order collected stop context get-new-context)]
          [processed-b (visit b quantvars matcher order collected stop context get-new-context)]
          [processed-c (visit c quantvars matcher order collected stop context get-new-context)])
     (append processed-a processed-b processed-c))]
    [(node/expr/constant info 1 'Int)
     '()]
    [(node/expr/constant info arity type)
     '()]
    [(node/expr/op info arity args)
     (interpret-expr-op expr quantvars args matcher order collected stop context get-new-context)]
    [(node/expr/quantifier-var info arity sym name)  
     '()]
    [(node/expr/comprehension info len decls inner-form)
     (process-quant-shaped-node expr decls inner-form quantvars matcher order collected stop context get-new-context)]))

(define (interpret-expr-op expr quantvars args matcher order collected stop context get-new-context)
  (define (process-children children quantvars)
    (apply append (map (lambda (x) (visit x quantvars matcher order collected stop context get-new-context)) children)))

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

(define (interpret-int expr quantvars matcher order collected stop context get-new-context)
  (when (@>= (get-verbosity) 2)
    (printf "collector: interpret-int: ~a~n" expr))
  (match expr
    [(node/int/constant info value)
     '()]
    [(node/int/op info args)
     (interpret-int-op expr quantvars args matcher order collected stop context get-new-context)]
    [(node/int/sum-quant info decls int-expr)
     (process-quant-shaped-node expr decls int-expr quantvars matcher order collected stop context get-new-context)]))

(define (process-quant-shaped-node node decls inner-node quantvars matcher order collected stop context get-new-context)
  (define new-vs-and-collected
    (for/fold ([vs-and-collected (list '() '())])
              ([decl decls])
      (define curr-new-quantvars (first vs-and-collected))
      (define new-quantvars (cons (car decl) curr-new-quantvars))
      (define new-domain-collected
        (visit (cdr decl) (append curr-new-quantvars new-quantvars) matcher order collected stop context get-new-context))         
      (list new-quantvars (append (second vs-and-collected) new-domain-collected))))
  
  (define new-quantvars (reverse (first new-vs-and-collected)))
  (define new-domain-collected (second new-vs-and-collected))
  (define inner-collected (visit inner-node new-quantvars matcher order collected stop context get-new-context))
  (append new-quantvars new-domain-collected inner-collected))

(define (interpret-int-op expr quantvars args matcher order collected stop context get-new-context)
  (define (process-children children quantvars)
    (apply append (map (lambda (x) (visit x quantvars matcher order collected stop context get-new-context)) children)))

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
   (collect (some univ) (lambda (n ctxt) n) #:order 'post-order)
   (list univ (some univ)))
  
  ; Get all nodes, pre-order
  (check-equal?
   (collect (some univ) (lambda (n ctxt) n) #:order 'pre-order)
   (list (some univ) univ))
  
  ; Filter only expressions. Note that separate construction of quantifiers
  ; produces different variables, hence the let*.
  (let* ([fmla (all ([x univ]) (some (& (-> x x) iden)))]
         [v (car (first (node/formula/quantified-decls fmla)))])
    (check-equal?
     (collect fmla
              (lambda (n ctxt) (if (node/expr? n) n #f)) #:order 'pre-order)
     (list v univ (& (-> v v) iden) (-> v v) v v iden)))
  
  ; Confirm that multi-decl extraction works for the complex quantifier-shaped cases
  ; which all invoke the process-quant-shaped-node helper
  (let* ([expr (set ([x univ][y (& univ univ)]) (some (& (-> x y) iden)))]
         [v1 (car (first (node/expr/comprehension-decls expr)))]
         [v2 (car (second (node/expr/comprehension-decls expr)))])
    (check-equal?
     (collect expr (lambda (n ctxt) (if (node/expr/quantifier-var? n) n #f)) #:order 'pre-order)
     (list v1 v2 v1 v2))
    (check-equal?
     (collect expr (lambda (n ctxt) (if (node/expr? n) n #f)) #:order 'pre-order)
     (list expr
           v1 v2
           univ (& univ univ) univ univ
           (& (-> v1 v2) iden) (-> v1 v2) v1 v2 iden)))

  ; Confirm that the stop policy is respected
  (check-equal?
   (collect (&& (some (-> univ univ))) (lambda (n ctxt) n)
            #:order 'pre-order
            #:stop (lambda (n ctxt) (not (node/formula/op? n))))
   (list (&& (some (-> univ univ)))
         (some (-> univ univ))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;; Check context ;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ; Test: gather the outermost "sum" nodes (without a stop argument)
  ; Recall the need to wrap raw numbers in "int"...
  (check-equal?
   (collect (add (int 1) (int 5))
            (lambda (n ctxt)
              (if (and ctxt (node/int/op/sum? n)) n #f))
            #:order 'pre-order
            #:context #t
            #:get-new-context (lambda (n ctxt) (if (node/int/op/sum? n) #f ctxt)))
   (list ))
  ; Test: gather any immediate child of a "sum" node
  (check-equal?
   (collect (add (sum (sing (sum (sing (int 1))))) (int 5))
            (lambda (n ctxt) (if ctxt n #f))
            #:order 'pre-order
            #:context #f
            #:get-new-context (lambda (n ctxt) (if (node/int/op/sum? n) #t #f)))
   (list (sing (sum (sing (int 1))))
         (sing (int 1))))

  )
  

