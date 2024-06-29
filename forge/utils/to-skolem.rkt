#lang racket/base

; This file is intended to take in the Forge AST and return a new AST with all existential quantifiers skolemized.
; It also returns the kodkod-bounds object. 

(require 
  forge/sigs-structs
  ;forge/sigs-functional ; For KM: OK to remove this to avoid dependency cycle?
  forge/lang/ast
  forge/shared
  forge/lang/bounds
  forge/utils/substitutor
  (only-in racket index-of match string-join first second rest flatten cartesian-product thunk empty?)
  (only-in racket/contract define/contract or/c listof any/c)
  (prefix-in @ (only-in racket/contract ->))
  (prefix-in @ (only-in racket/base >= +)))
(provide interpret-formula)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Boolean formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (create-run-with-bounds run-statement new-bounds)
;;   (Run (Run-name run-statement) (Run-command run-statement) (Run-run-spec run-statement) 
;;                          (Run-result run-statement) (Run-server-ports run-statement)
;;                         (Run-atoms run-statement) (Run-kodkod-currents run-statement) new-bounds
;;                         (Run-last-sterling-instance run-statement)))

(define (find-upper-bound kodkod-bounds type)
  (for/or ([bound kodkod-bounds])
    (if (equal? (bound-relation bound) type)
      (bound-upper bound)
      #f)))

(define (create-join-expr quantvars skolem-relation info)
  (define join-rhs skolem-relation)
  (for/fold ([join-rhs join-rhs])
            ([quantvar quantvars])
    (define new-join-expr (node/expr/op/join info 2 (list quantvar join-rhs)))
    new-join-expr))

(define (skolemize-formula run-spec total-bounds formula relations atom-names quantvars quantvar-types info quantifier decls form)
    (printf "skolemize-formula: ~a ~a~n" formula quantvars)
  ; RESTRICTION: Because we are mapping top-level sigs to sorts, we have no "univ".
  ; Discuss. May be better to not use sorts (or worse).
  ; TODO: *top level* only! Right now, all sorts get added (For KM: discuss)
  (unless (Sig? (cdr (first decls)))
    (raise-forge-error #:msg (format "Skolemization required a sig for quantifier domain of variable ~a; got ~a" (car (first decls)) (cdr (first decls)))
                       #:context #f))
  (define qdomain-sig (cdr (first decls)))
  
    ; 1. check to see which universals we are in scope of from quantvars
      ; AMENDMENT: this is just everything in quantvars.
    ; 2. define a new relation, which is a function from (universals) -> (type of the existential)
    ; for name, could just do string-append $ <variable name that has gensym>

    ;; For KM: We needed to add the codomain as well.
    (define codomain-type qdomain-sig)
    (define codomain-upper-bound (find-upper-bound total-bounds codomain-type))
  
    ; Fields for node/expr/relation: info arity name typelist-thunk parent is-variable
    (define skolem-relation (node/expr/relation
                             info
                             (@+ (length quantvars) 1)
                             (string-append "$" (symbol->string (node/expr/quantifier-var-sym (car (first decls)))))
                             
                             ; For KM: what happens if there are multiple variables in the decl? This code seems
                             ; to only get the first of them.
                             
                             ; Note: this *may* cause issues because 'univ is not a sig name. But we are already
                             ; past the last-checker etc.
                             (lambda () (append (map Sig-name quantvar-types) (list (Sig-name codomain-type))))
                             (if (empty? quantvar-types) codomain-type (list-ref quantvar-types 0))
                             #f))
    ; (define skolem-relation (make-relation 'temp-name (map (lambda (s) (thunk s)) quantvar-types)))
    ; 3. fetch the upper bounds and the product them
    (define kodkod-bounds total-bounds)

    (define upper-bound-list
      (append
       ; The domain types' upper bounds; each is a list-of-lists-of-atoms.
       (for/fold ([upper-bounds '()])
                 ([type quantvar-types])
         (define type-upper-bound (find-upper-bound kodkod-bounds type))
         (cons type-upper-bound upper-bounds))
       (list codomain-upper-bound)))
  
    ; For KM: just applying flatten here will eliminate the tuples entirely.
    ; We have something like this: '( (1 2 3) (4 5 6) )
    ;   - outer list: set of tuples
    ;   - inner lists: ordered tuples of atoms
    ; This will work for arity > 1:
    ;   > (apply cartesian-product '( (1 2 3) (4 5 6)) )
    ; because it's equivalent to (cartesian-product '(1 2 3) '(4 5 6))
    ;   '((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6))
    ; But not for arity 1:
    ;  > (apply cartesian-product '( (1 2 3)) )
    ;  '((1) (2) (3))
    ;  > (apply cartesian-product '( (1) (2) (3)) )
    ;  '((1 2 3))
    ; ...because cartesian-product can also take a single list as its argument.
    ; Fortunately, if arity = 1, then we don't need to do the cartesian product at all.
  
    ;(define skolem-upper-bound (apply cartesian-product (map flatten upper-bound-list)))
    (define skolem-upper-bound
      (if (empty? quantvar-types)
          ; only one type
          (first upper-bound-list)
          ; otherwise, build product
          (apply cartesian-product upper-bound-list)))
    (printf "~nskolem-upper-bound: ~a~n" skolem-upper-bound)
  
    ; 4. add that new relation to the bounds
    (define skolem-bounds (make-bound skolem-relation '() skolem-upper-bound))
    ; Skolem bounds _at end_, so that CVC5 conversion doesn't need to hoist bounds for sigs
    (define new-bounds (append total-bounds (list skolem-bounds)))
    ;(define new-run (create-run-with-bounds run-spec new-bounds))

    ; 5. invoke the substitutor on the formula, replacing the existential with the new relation applied to the universals
    (define target (car (first decls)))
    (define value (create-join-expr quantvars skolem-relation info))
    (values (substitute-formula run-spec form relations atom-names quantvars target value) new-bounds))

(define current-bounds '())

; Translate a formula AST node
(define/contract (interpret-formula run-spec total-bounds formula relations atom-names quantvars quantvar-types)  
  (@-> (or/c Run? State? Run-spec?)
      any/c
      node/formula?
      list?
      list?
      list?
      list?
      (values node? list?))
  (when (@>= (get-verbosity) 2)
    (printf "to-skolem: interpret-formula: ~a~n" formula))
  (set! current-bounds total-bounds) ; no "kodkod-bounds" yet; the Run hasn't been created
  (let ([resulting-formula 
    (match formula
      [(node/formula/constant info type)
      (node/formula/constant info type)]    
      [(node/fmla/pred-spacer info name args expanded)
      (define-values (fmla bounds) (interpret-formula run-spec total-bounds expanded relations atom-names quantvars quantvar-types))
      (set! current-bounds bounds)
      fmla]
      [(node/formula/op info args)
      (interpret-formula-op run-spec total-bounds formula relations atom-names quantvars quantvar-types args)]
      [(node/formula/multiplicity info mult expr)
      (let ([processed-expr (interpret-expr run-spec total-bounds expr relations atom-names quantvars)])
      (node/formula/multiplicity info mult processed-expr))]
      [(node/formula/quantified info quantifier decls form)
      ; if it is ALL, do the below as normal.
      ; if it is SOME, we want to skolemize the formula. this involves a few steps which are listed in the helper function
      (match quantifier
      ['some
          ; Make sure to skolemize the _inner_ formula as well, in case of multiple nested existentials.
          (define-values (new-inner-form new-inner-bounds)
            (interpret-formula run-spec total-bounds form relations atom-names quantvars quantvar-types))
          ; Now skolemize, using the skolemized inner formula as the baseline
          (define-values (fmla bounds)
            (skolemize-formula run-spec new-inner-bounds formula relations atom-names quantvars quantvar-types info quantifier decls new-inner-form))
          (set! current-bounds bounds)
          fmla]
      [_ (define new-vs-decls-types
        (for/fold ([vs-decls-types (list quantvars '() quantvar-types)])
                  ([decl decls])
          (define curr-quantvars (first vs-decls-types))
          (define curr-decls (second vs-decls-types))
          (define new-quantvars (cons (car decl) quantvars))
          (define new-decl-domain (interpret-expr run-spec total-bounds (cdr decl) relations atom-names new-quantvars))
          (define new-decls (cons (cons (car decl) new-decl-domain) curr-decls))
          (define new-quantvar-types (cons (cdr decl) quantvar-types))
          (list new-quantvars new-decls new-quantvar-types)))
      (define new-quantvars (list-ref new-vs-decls-types 0))
      (define new-quantvar-types (list-ref new-vs-decls-types 2))
      (let-values ([(processed-form bounds) (interpret-formula run-spec total-bounds form relations atom-names new-quantvars new-quantvar-types)])
        (set! current-bounds bounds)
        (define new-decls (list-ref new-vs-decls-types 1))
        (node/formula/quantified info quantifier new-decls processed-form))])]
      [(node/formula/sealed info)
      (node/formula/sealed info)]
      [#t "true"]
      [#f "false"])]) 
    (values resulting-formula current-bounds)))

(define (process-children-formula run-spec total-bounds children relations atom-names quantvars quantvar-types)
  (map (lambda (x) (define-values (fmla bounds) (interpret-formula run-spec total-bounds x relations atom-names quantvars quantvar-types)) (set! current-bounds bounds) fmla) children))

(define (process-children-expr run-spec total-bounds children relations atom-names quantvars)
  (map (lambda (x) (interpret-expr run-spec total-bounds x relations atom-names quantvars)) children))

(define (process-children-int run-spec total-bounds children relations atom-names quantvars)
  (map (lambda (x) (interpret-int run-spec total-bounds x relations atom-names quantvars)) children))

(define (interpret-formula-op run-spec total-bounds formula relations atom-names quantvars quantvar-types args)
  (when (@>= (get-verbosity) 2)
    (printf "to-skolem: interpret-formula-op: ~a~n" formula))
  (match formula
    [(node/formula/op/&& info children)
      (node/formula/op/&& info (process-children-formula run-spec total-bounds args relations atom-names quantvars quantvar-types))]
    [(node/formula/op/|| info children)
     (node/formula/op/|| info (process-children-formula run-spec total-bounds args relations atom-names quantvars quantvar-types))]
    [(node/formula/op/=> info children)
     (node/formula/op/=> info (process-children-formula run-spec total-bounds args relations atom-names quantvars quantvar-types))]
    [(node/formula/op/always info children)
     (node/formula/op/always info (process-children-formula run-spec total-bounds args relations atom-names quantvars quantvar-types))]
    [(node/formula/op/eventually info children)
     (node/formula/op/eventually info (process-children-formula run-spec total-bounds args relations atom-names quantvars quantvar-types))]
    [(node/formula/op/next_state info children)
      (node/formula/op/next_state info (process-children-formula run-spec total-bounds args relations atom-names quantvars quantvar-types))]
    [(node/formula/op/releases info children)
      (node/formula/op/releases info (process-children-formula run-spec total-bounds args relations atom-names quantvars quantvar-types))]
    [(node/formula/op/until info children)
     (node/formula/op/until info (process-children-formula run-spec total-bounds args relations atom-names quantvars quantvar-types))]
    [(node/formula/op/historically info children)
      (node/formula/op/historically info (process-children-formula run-spec total-bounds args relations atom-names quantvars quantvar-types))]
    [(node/formula/op/once info children)
      (node/formula/op/once info (process-children-formula run-spec total-bounds args relations atom-names quantvars quantvar-types))]
    [(node/formula/op/prev_state info children)
      (node/formula/op/prev_state info (process-children-formula run-spec total-bounds args relations atom-names quantvars quantvar-types))]
    [(node/formula/op/since info children)
      (node/formula/op/since info (process-children-formula run-spec total-bounds args relations atom-names quantvars quantvar-types))]
    [(node/formula/op/triggered info children)
      (node/formula/op/triggered info (process-children-formula run-spec total-bounds args relations atom-names quantvars quantvar-types))]
    [(node/formula/op/in info children)
      (node/formula/op/in info (process-children-expr run-spec total-bounds args relations atom-names quantvars))]
    [(node/formula/op/= info children)
      (node/formula/op/= info (process-children-expr run-spec total-bounds args relations atom-names quantvars))]
    [(node/formula/op/! info children)
      (node/formula/op/! info (process-children-formula run-spec total-bounds args relations atom-names quantvars quantvar-types))]
    [(node/formula/op/int> info children)
      (node/formula/op/int> info (process-children-int run-spec total-bounds args relations atom-names quantvars))]
    [(node/formula/op/int< info children)
      (node/formula/op/int< info (process-children-int run-spec total-bounds args relations atom-names quantvars))]
    [(node/formula/op/int= info children)
     (node/formula/op/int= info (process-children-int run-spec total-bounds args relations atom-names quantvars))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Relational expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interpret-expr run-spec total-bounds expr relations atom-names quantvars)
  (when (@>= (get-verbosity) 2)
      (printf "to-skolem: interpret-expr: ~a~n" expr))
  (match expr
    [(node/expr/relation info arity name typelist-thunk parent isvar)
     expr]
    [(node/expr/atom info arity name)
     (node/expr/atom info arity name)]
    [(node/expr/fun-spacer info arity name args result expanded)
     (interpret-expr run-spec expanded relations atom-names quantvars)]
    [(node/expr/ite info arity a b c)  
    (let-values ([(processed-a bounds) (interpret-formula run-spec total-bounds a relations atom-names quantvars '())]
          [(processed-b) (interpret-expr run-spec total-bounds b relations atom-names quantvars)]
          [(processed-c) (interpret-expr run-spec total-bounds c relations atom-names quantvars)])
     (set! current-bounds bounds)
     (node/expr/ite info arity processed-a processed-b processed-c))]
    [(node/expr/constant info 1 'Int)
     (node/expr/constant info 1 'Int)]
    [(node/expr/constant info arity type)
     (node/expr/constant info arity type)]
    [(node/expr/op info arity args)
     (interpret-expr-op run-spec total-bounds expr relations atom-names quantvars args)]
    [(node/expr/quantifier-var info arity sym name)  
     (node/expr/quantifier-var info arity sym name)]
    [(node/expr/comprehension info len decls form)   
     (define new-vs-and-decls
       (for/fold ([vs-and-decls (list quantvars '())])
                 ([decl decls])
         (define curr-quantvars (first vs-and-decls))
         (define curr-decls (second vs-and-decls))
         (define new-quantvars (cons (car decl) quantvars))
         (define new-decl-domain (interpret-expr run-spec total-bounds (cdr decl) relations atom-names new-quantvars))
         (define new-decls (cons (cons (car decl) new-decl-domain) curr-decls))
         (list new-quantvars new-decls)))
     (define new-quantvars (first new-vs-and-decls))
     (let-values ([(processed-form bounds) (interpret-formula run-spec total-bounds form relations atom-names new-quantvars '())])
       (define new-decls (second new-vs-and-decls))
       (set! current-bounds bounds)
     (node/expr/comprehension info len new-decls processed-form))]))

(define (interpret-expr-op run-spec total-bounds expr relations atom-names quantvars args)
    (when (@>= (get-verbosity) 2)
      (printf "to-skolem: interpret-expr-op: ~a~n" expr))
  (match expr
    [(node/expr/op/+ info arity children)
     (node/expr/op/+ info arity (process-children-expr run-spec total-bounds args relations atom-names quantvars))]
    [(node/expr/op/- info arity children)
     (node/expr/op/- info arity (process-children-expr run-spec total-bounds args relations atom-names quantvars))]
    [(node/expr/op/& info arity children)
     (node/expr/op/& info arity (process-children-expr run-spec total-bounds args relations atom-names quantvars))]
    [(node/expr/op/-> info arity children)
     (node/expr/op/-> info arity (process-children-expr run-spec total-bounds args relations atom-names quantvars))]
    [(node/expr/op/prime info arity children)
     (node/expr/op/prime info arity (process-children-expr run-spec total-bounds args relations atom-names quantvars))]
    [(node/expr/op/join info arity children)
     (node/expr/op/join info arity (process-children-expr run-spec total-bounds args relations atom-names quantvars))]
    [(node/expr/op/^ info arity children)
     (node/expr/op/^ info arity (process-children-expr run-spec total-bounds args relations atom-names quantvars))]
    [(node/expr/op/* info arity children)
    (node/expr/op/* info arity (process-children-expr run-spec total-bounds args relations atom-names quantvars))]
    [(node/expr/op/~ info arity children)
     (node/expr/op/~ info arity (process-children-expr run-spec total-bounds args relations atom-names quantvars))]
    [(node/expr/op/++ info arity children)
     (node/expr/op/++ info arity (process-children-expr run-spec total-bounds args relations atom-names quantvars))]
    [(node/expr/op/sing info arity children)
     (node/expr/op/sing info arity (process-children-int run-spec total-bounds args relations atom-names quantvars))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Integer expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interpret-int run-spec total-bounds expr relations atom-names quantvars)
  (when (@>= (get-verbosity) 2)
    (printf "to-skolem: interpret-int: ~a~n" expr))
  (match expr
    [(node/int/constant info value)
     (node/int/constant info value)]
    [(node/int/op info args)
     (interpret-int-op run-spec total-bounds expr relations atom-names quantvars args)]
    [(node/int/sum-quant info decls int-expr)
     (define new-vs-and-decls
       (for/fold ([vs-and-decls (list quantvars '())])
                 ([decl decls])
         (define curr-quantvars (first vs-and-decls))
         (define curr-decls (second vs-and-decls))
         (define new-quantvars (cons (car decl) quantvars))
         (define new-decl-domain (interpret-expr run-spec total-bounds (cdr decl) relations atom-names new-quantvars))
         (define new-decls (cons (cons (car decl) new-decl-domain) curr-decls))
         (list new-quantvars new-decls)))
     (define new-quantvars (first new-vs-and-decls))
     (let ([processed-int (interpret-int run-spec total-bounds int-expr relations atom-names new-quantvars)])
       (define new-decls (second new-vs-and-decls))
      (node/int/sum-quant info new-decls processed-int))]))

(define (interpret-int-op run-spec total-bounds expr relations atom-names quantvars args)
  (when (@>= (get-verbosity) 2)
    (printf "to-skolem: interpret-int-op: ~a~n" expr))
  (match expr
    [(node/int/op/add info children)
      (node/int/op/add info (process-children-int run-spec total-bounds args relations atom-names quantvars))]
    [(node/int/op/subtract info children)
    (node/int/op/subtract info (process-children-int run-spec total-bounds args relations atom-names quantvars))]
    [(node/int/op/multiply info children)
    (node/int/op/multiply info (process-children-int run-spec total-bounds args relations atom-names quantvars))]
    [(node/int/op/divide info children)
    (node/int/op/divide info (process-children-int run-spec total-bounds args relations atom-names quantvars))]
    [(node/int/op/sum info children)
    (node/int/op/sum info (process-children-expr run-spec total-bounds args relations atom-names quantvars))]
    [(node/int/op/card info children)
    (node/int/op/card info (process-children-expr run-spec total-bounds args relations atom-names quantvars))]
    [(node/int/op/remainder info children)
     (node/int/op/remainder info (process-children-int run-spec total-bounds args relations atom-names quantvars))]
    [(node/int/op/abs info children)
     (node/int/op/abs info (process-children-int run-spec total-bounds args relations atom-names quantvars))]
    [(node/int/op/sign info children)
     (node/int/op/sign info (process-children-int run-spec total-bounds args relations atom-names quantvars))]
    [(node/int/sum-quant info decls int-expr)
     (raise-forge-error #:msg "Reached expected unreachable code." #:context expr)]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;