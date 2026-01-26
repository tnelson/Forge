#lang racket/base 

(require forge/lang/ast)
(require racket/string 
         (only-in racket first empty? rest)
         (except-in racket/contract ->)
         (prefix-in @ (only-in racket/contract ->))
         
         (prefix-in @ (only-in racket +))
         (only-in forge/shared get-verbosity VERBOSITY_HIGH))
(provide pretty-print-core-formula process-core-formula pretty-format-core-formula)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Note on cores: if core granularity is high, Kodkod may return a formula we do not have an ID for.
  ; In these cases, the engine should be passing something like "f:0,0" which indexes _child_ formulas.
  ; (In *very* rare cases, Kodkod may not be able to blame a subformula, and just give us a string.)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (traverse-path-list path core-map [fmla #f])
    ;; Cannot use for/fold or fold here, because we need to _not_ move down the list for
    ;; a node/fmla/pred-spacer node (and any other node that is invisible to Pardinus).
    (cond
      ; Base case: no more indexes to process
      [(empty? path) fmla]
      ; We have an index, descend as appropriate
      [else 
       (define idx-str (first path))
       (define idx (string->number idx-str))
       (cond [(not fmla)
              (traverse-path-list (rest path) 
                                  core-map
                                  (hash-ref core-map idx))]
             [(node/formula/quantified? fmla)
              ; Quantified: decls formulas first, then sub-formula last
              (cond [(>= idx (length (node/formula/quantified-decls fmla)))
                     (traverse-path-list (rest path)
                                         core-map
                                         (node/formula/quantified-formula fmla))]
                    [else
                     (define decl (list-ref (node/formula/quantified-decls fmla) idx))
                     (traverse-path-list (rest path)
                                         core-map
                                         (car decl))])]
             [(node/formula/op? fmla)
              ; Operator formula: sub-formulas in order. Note that this layer isn't shown
              ; to Pardinus, so we cannot move down the path index list for this.
              (traverse-path-list (rest path) 
                                  core-map
                                  (list-ref (node/formula/op-children fmla) idx))]
             [(node/fmla/pred-spacer? fmla)
              ; Predicate spacer, just use internal formula, and don't move forward in the path
              (traverse-path-list path
                                  core-map
                                  (node/fmla/pred-spacer-expanded fmla))]
             [else
              (raise-user-error (format "Unsupported formula type in core: ~a" fmla))])])) 

(define (find-core-formula id core-map)
  (unless (string-prefix? id "f:")
    (raise-user-error (format "Unexpected error: invalid formula path ID: ~a" id)))
  (define path (string-split (first (string-split id "f:")) ","))
  (when (>= (get-verbosity) VERBOSITY_HIGH)
    (printf "core path: ~a~n" path))
  (unless (and (> (length path) 0) (member (string->number (first path)) (hash-keys core-map)))
    (raise-user-error (format "Unexpected error: solver path ID prefix was invalid: ~a; valid prefixes: ~a" id (hash-keys core-map))))
  (traverse-path-list path core-map #f))

; A processed core formula will hopefully be a node, but it might also be a string
; describing a formula that Kodkod couldn't map back. (This happens sometimes when using 
; temporal operators, but it's rare.)  
(define/contract (process-core-formula fmla-or-id core-map) 
  (@-> (or/c node? string?) hash? (or/c string? node?))
  ; (printf "***~n*** process-core-formula ~a ~a~n" fmla-or-id core-map)
  ;(define fmla-num (if (string-prefix? id "f:") (string->number (substring id 2)) #f)]
  (cond [(and (string? fmla-or-id) (string-prefix? fmla-or-id "f:")) 
         (find-core-formula fmla-or-id core-map)]
         ; Otherwise, it might be a node already, or else a string with a formula 
         ; Kodkod couldn't find a blame path for. 
        [else fmla-or-id]))

(define (pretty-format-core-formula idx max fmla-or-id core-map)
  (define fmla-or-string (process-core-formula fmla-or-id core-map))
  (cond [(node? fmla-or-string)
         (format "Core(part ~a/~a): [~a] ~a~n" (@+ idx 1) max
                  (pretty-loc fmla-or-string) (deparse fmla-or-string))]
        [else
          (format "Core(part ~a/~a): [UNKNOWN] ~a~n" (@+ idx 1) max
                  fmla-or-string)]))

(define (pretty-print-core-formula idx max fmla-or-id core-map)
  (fprintf (current-error-port) (pretty-format-core-formula idx max fmla-or-id core-map)))
  