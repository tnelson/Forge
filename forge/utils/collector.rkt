#lang typed/racket/base/optional

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
  forge/types/ast-adapter
  forge/shared
  (only-in typed/racket match first second rest append apply map format cons equal?)
  (prefix-in @ (only-in typed/racket >=)))

(provide collect)

; TODO: At the moment, the matcher will not be given quantifier-scope information.
; TODO: There is no means to control whether the visitor descends into the record
;   portion of a spacer node; at the moment only actually-used-in-constraint nodes
;   are visited.

(: collect (->* (node (-> node Any Any)
                      #:order (U 'pre-order 'post-order))
                (#:stop (-> node Any Boolean)
                 #:context Any
                 #:get-new-context (-> node Any Any))
                (Listof Any)))
(define (collect node matcher #:order order
                          #:stop [stop (lambda (n ctxt) #f)]
                          #:context [context #f]
                          #:get-new-context [get-new-context (lambda (n ctxt) ctxt)])
  (visit node '() matcher order '() stop context get-new-context))

(: visit (-> node (Listof node/expr/quantifier-var) (-> node Any Any) Symbol
             (Listof Any) (-> node Any Boolean) Any
             (-> node Any Any) (Listof Any)))
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
(: interpret-formula (-> node/formula (Listof node/expr/quantifier-var) (-> node Any Any) Symbol
                         (Listof Any) (-> node Any Boolean) Any
                         (-> node Any Any) (Listof Any)))
(define (interpret-formula formula quantvars matcher order collected stop context get-new-context)  
  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "collector: interpret-formula: ~a~n" formula))
  (match formula
    [(node/formula/constant info type)
     '()]    
    [(node/fmla/pred-spacer info name args expanded)
     (visit expanded quantvars matcher order collected stop context get-new-context)]
    [(? node/formula/op?)
     (interpret-formula-op formula quantvars (node/formula/op-children formula) matcher order collected stop context get-new-context)]
    [(node/formula/multiplicity info mult expr)
     (visit expr quantvars matcher order collected stop context get-new-context)]
    [(node/formula/quantified info quantifier decls inner-form)
     (process-quant-shaped-node formula decls inner-form quantvars matcher order collected stop context get-new-context)]
    ; node/formula/sealed smuggles the actual formula in its info field, which complicates typing.
    ; Commenting out for now as it may no longer be used.
    ;[(node/formula/sealed info)
    ; (visit info quantvars matcher order collected stop context get-new-context)]
    [#t '()]
    [#f '()]))

(: interpret-formula-op (-> node/formula/op (Listof node/expr/quantifier-var) (Listof node) (-> node Any Any) Symbol
                            (Listof Any) (-> node Any Boolean) Any
                            (-> node Any Any) (Listof Any)))
(define (interpret-formula-op formula quantvars args matcher order collected stop context get-new-context)
  (: process-children (-> (Listof node) (Listof node/expr/quantifier-var) (Listof Any)))
  (define (process-children children quantvars)
    (apply append (map (lambda ([x : node]) (visit x quantvars matcher order collected stop context get-new-context)) children)))

  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "collector: interpret-formula-op: ~a~n" formula))
  ; We could get away with only one case here, really, since there's no distinguishing
  ; but leaving the structure here for now in case we need it for any refinement.
  (match formula
    [(? node/formula/op-on-formulas/&&?) (process-children args quantvars)]
    [(? node/formula/op-on-formulas/||?) (process-children args quantvars)]
    [(? node/formula/op-on-formulas/=>?) (process-children args quantvars)]
    [(? node/formula/op-on-formulas/always?) (process-children args quantvars)]
    [(? node/formula/op-on-formulas/eventually?) (process-children args quantvars)]
    [(? node/formula/op-on-formulas/next_state?) (process-children args quantvars)]
    [(? node/formula/op-on-formulas/releases?) (process-children args quantvars)]
    [(? node/formula/op-on-formulas/until?) (process-children args quantvars)]
    [(? node/formula/op-on-formulas/historically?) (process-children args quantvars)]
    [(? node/formula/op-on-formulas/once?) (process-children args quantvars)]
    [(? node/formula/op-on-formulas/prev_state?) (process-children args quantvars)]
    [(? node/formula/op-on-formulas/since?) (process-children args quantvars)]
    [(? node/formula/op-on-formulas/triggered?) (process-children args quantvars)]
    [(? node/formula/op-on-exprs/in?) (process-children args quantvars)]
    [(? node/formula/op-on-exprs/=?) (process-children args quantvars)]
    [(? node/formula/op-on-formulas/!?) (process-children args quantvars)]
    [(? node/formula/op-on-ints/int>?) (process-children args quantvars)]
    [(? node/formula/op-on-ints/int<?) (process-children args quantvars)]
    [(? node/formula/op-on-ints/int=?) (process-children args quantvars)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Relational expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: interpret-expr (-> node/expr (Listof node/expr/quantifier-var) (-> node Any Any) Symbol
                      (Listof Any) (-> node Any Boolean) Any
                      (-> node Any Any) (Listof Any)))
(define (interpret-expr expr quantvars matcher order collected stop context get-new-context)
  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
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
    [(? node/expr/op?)
     (interpret-expr-op expr quantvars (node/expr/op-children expr) matcher order collected stop context get-new-context)]
    [(node/expr/quantifier-var info arity sym name)  
     '()]
    [(node/expr/comprehension info len decls inner-form)
     (process-quant-shaped-node expr decls inner-form quantvars matcher order collected stop context get-new-context)]))

(: interpret-expr-op (-> node/expr/op (Listof node/expr/quantifier-var) (Listof node) (-> node Any Any) Symbol
                         (Listof Any) (-> node Any Boolean) Any
                         (-> node Any Any) (Listof Any)))
(define (interpret-expr-op expr quantvars args matcher order collected stop context get-new-context)
  (: process-children (-> (Listof node) (Listof node/expr/quantifier-var) (Listof Any)))
  (define (process-children children quantvars)
    (apply append (map (lambda ([x : node]) (visit x quantvars matcher order collected stop context get-new-context)) children)))

  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "collector: interpret-expr-op: ~a~n" expr))
  ; We could get away with only one case here, really, since there's no distinguishing
  ; but leaving the structure here for now in case we need it for any refinement.
  (match expr
    [(? node/expr/op-on-exprs/+?) (process-children args quantvars)]
    [(? node/expr/op-on-exprs/-?) (process-children args quantvars)]
    [(? node/expr/op-on-exprs/&?) (process-children args quantvars)]
    [(? node/expr/op-on-exprs/->?) (process-children args quantvars)]
    [(? node/expr/op-on-exprs/prime?) (process-children args quantvars)]
    [(? node/expr/op-on-exprs/join?) (process-children args quantvars)]
    [(? node/expr/op-on-exprs/^?) (process-children args quantvars)]
    [(? node/expr/op-on-exprs/*?) (process-children args quantvars)]
    [(? node/expr/op-on-exprs/~?) (process-children args quantvars)]
    [(? node/expr/op-on-exprs/++?) (process-children args quantvars)]
    [(? node/expr/op-on-ints/sing?) (process-children args quantvars)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Integer expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: interpret-int (-> node/int (Listof node/expr/quantifier-var) (-> node Any Any) Symbol
                     (Listof Any) (-> node Any Boolean) Any
                     (-> node Any Any) (Listof Any)))
(define (interpret-int expr quantvars matcher order collected stop context get-new-context)
  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "collector: interpret-int: ~a~n" expr))
  (match expr
    [(node/int/constant info value)
     '()]
    [(? node/int/op?)
     (interpret-int-op expr quantvars (node/int/op-children expr) matcher order collected stop context get-new-context)]
    [(node/int/sum-quant info decls int-expr)
     (process-quant-shaped-node expr decls int-expr quantvars matcher order collected stop context get-new-context)]))

(: process-quant-shaped-node (-> node Decls node (Listof node/expr/quantifier-var) (-> node Any Any) Symbol
                                 (Listof Any) (-> node Any Boolean) Any
                                 (-> node Any Any) (Listof Any)))
(define (process-quant-shaped-node node decls inner-node quantvars matcher order collected stop context get-new-context)
  (define new-vs-and-collected
    (for/fold ([vs-and-collected : (List (Listof node/expr/quantifier-var) (Listof Any)) (list '() '())])
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

(: interpret-int-op (-> node/int/op (Listof node/expr/quantifier-var) (Listof node) (-> node Any Any) Symbol
                        (Listof Any) (-> node Any Boolean) Any
                        (-> node Any Any) (Listof Any)))
(define (interpret-int-op expr quantvars args matcher order collected stop context get-new-context)
  (: process-children (-> (Listof node) (Listof node/expr/quantifier-var) (Listof Any)))
  (define (process-children children quantvars)
    (apply append (map (lambda ([x : node]) (visit x quantvars matcher order collected stop context get-new-context)) children)))

  (when (@>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "collector: interpret-int-op: ~a~n" expr))

  ; We could get away with only one case here, really, since there's no distinguishing
  ; but leaving the structure here for now in case we need it for any refinement.
  (match expr
    [(? node/int/op-on-ints/add?) (process-children args quantvars)]
    [(? node/int/op-on-ints/subtract?) (process-children args quantvars)]
    [(? node/int/op-on-ints/multiply?) (process-children args quantvars)]
    [(? node/int/op-on-ints/divide?) (process-children args quantvars)]
    [(? node/int/op-on-exprs/sum?) (process-children args quantvars)]
    [(? node/int/op-on-exprs/card?) (process-children args quantvars)]
    [(? node/int/op-on-ints/remainder?) (process-children args quantvars)]
    [(? node/int/op-on-ints/abs?) (process-children args quantvars)]
    [(? node/int/op-on-ints/sign?) (process-children args quantvars)]
    [(node/int/sum-quant info decls int-expr)
     (raise-forge-error #:msg "Reached expected unreachable code." #:context expr)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Rackunit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Very basic smoke tests
(module+ test
  (require typed/rackunit)
  ; Get all nodes, post-order
  (check-equal?
   (collect (some/func univ) (lambda (n ctxt) n) #:order 'post-order)
   (list univ (some/func univ)))

  ; Get all nodes, pre-order
  (check-equal?
   (collect (some/func univ) (lambda (n ctxt) n) #:order 'pre-order)
   (list (some/func univ) univ))

  ; Filter only expressions. Note that separate construction of quantifiers
  ; produces different variables, hence the let*.
  (let* ([varx (var 'x)]
         [fmla (quantified-formula empty-nodeinfo 'all (list (cons varx univ))
                                   (some/func (&/func (->/func varx varx) iden)))]
         [v (car (first (node/formula/quantified-decls fmla)))])
    (check-equal?
     (collect fmla
              (lambda (n ctxt) (if (node/expr? n) n #f)) #:order 'pre-order)
     (list v univ (&/func (->/func v v) iden) (->/func v v) v v iden)))

  ; Confirm that multi-decl extraction works for the complex quantifier-shaped cases
  ; which all invoke the process-quant-shaped-node helper
  (let* ([x (var 'x)]
         [y (var 'y)]
         [expr (set/func (list (cons x univ) (cons y (&/func univ univ))) (some/func (&/func (->/func x y) iden)))]
         [v1 (car (first (node/expr/comprehension-decls expr)))]
         [v2 (car (second (node/expr/comprehension-decls expr)))])
    (check-equal?
     (collect expr (lambda (n ctxt) (if (node/expr/quantifier-var? n) n #f)) #:order 'pre-order)
     (list v1 v2 v1 v2))
    (check-equal?
     (collect expr (lambda (n ctxt) (if (node/expr? n) n #f)) #:order 'pre-order)
     (list expr
           v1 v2
           univ (&/func univ univ) univ univ
           (&/func (->/func v1 v2) iden) (->/func v1 v2) v1 v2 iden)))

  ; Confirm that the stop policy is respected.
  ; (This should collect the outer conjunction and the inner atomic formula only.)
  ; Note that, since && and || will short-circuit in AST-node construction, this sort of test isn't well-suited to _those_ operators.
  (check-equal?
   (collect (!/func (some/func (->/func univ univ)))
            (lambda (n ctxt) n)
            #:order 'pre-order
            #:stop (lambda (n ctxt) (not (node/formula/op? n))))
   (list (!/func (some/func (->/func univ univ)))
         (some/func (->/func univ univ))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;; Check context ;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Test: gather the outermost "sum" nodes (without a stop argument)
  ; Recall the need to wrap raw numbers in "int"...
  (check-equal?
   (collect (add/func (int/func 1) (int/func 5))
            (lambda (n ctxt)
              (if (and ctxt (node/int/op-on-exprs/sum? n)) n #f))
            #:order 'pre-order
            #:context #t
            #:get-new-context (lambda (n ctxt) (if (node/int/op-on-exprs/sum? n) #f ctxt)))
   (list ))
  ; Test: gather any immediate child of a "sum" node
  (check-equal?
   (collect (add/func (sum/func (sing/func (sum/func (sing/func (int/func 1))))) (int/func 5))
            (lambda (n ctxt) (if ctxt n #f))
            #:order 'pre-order
            #:context #f
            #:get-new-context (lambda (n ctxt) (if (node/int/op-on-exprs/sum? n) #t #f)))
   (list (sing/func (sum/func (sing/func (int/func 1))))
         (sing/func (int/func 1))))

  )
  