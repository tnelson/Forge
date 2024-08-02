#lang racket/base

; Pardinus/Kodkod specific translation functions. Generally called from send-to-solver.rkt.

#| Print to KodKod-CLI order of operations:
    print configure
    declare univ size
    declare ints
    print Int sig (r0)
    print other sigs (r2 ... rm)
    print succ relation (r(m + 1))
    print other relations (r(m + 2) ... rn)
    print formula / assert formula (f0 ... fk)
    print solve
  |#
  
; (define-syntax-rule (kk-print lines ...)
;   (kodkod:cmd 
;     [stdin]
;     lines ...))


(require forge/sigs-structs
         forge/lang/ast
         forge/shared)
(require (prefix-in pardinus: forge/pardinus-cli/server/kks))
(require forge/translate-to-kodkod-cli
         forge/translate-from-kodkod-cli)

(require (prefix-in @ (only-in racket/base >= not - = and or max > < +))
         (only-in racket match first rest empty empty? set->list list->set set-intersect set-union
                         curry range index-of pretty-print filter-map string-prefix? string-split thunk*
                         remove-duplicates subset? cartesian-product match-define cons? set-subtract)
          racket/hash)

(provide send-to-kodkod get-next-kodkod-model)

(define (send-to-kodkod run-name run-spec bitwidth all-atoms solverspec total-bounds bound-lower bound-upper run-constraints stdin stdout stderr)
  
  ; Print targets
  (define-syntax-rule (pardinus-print lines ...)
    (pardinus:cmd [stdin] lines ...)) 

  (pardinus-print (pardinus:print-cmd (format "(with ~a" run-name)))
  (pardinus-print
   (pardinus:configure (format ":bitwidth ~a :solver ~a :max-solutions 1 :verbosity ~a :skolem-depth ~a :sb ~a :core-gran ~a :core-minimization ~a :log-trans ~a :no-overflow ~a ~a ~a"                               
                               bitwidth 
                               solverspec
                               (get-option run-spec 'engine_verbosity) ; see the Wiki for levels
                               (get-option run-spec 'skolem_depth)
                               (get-option run-spec 'sb) 
                               (get-option run-spec 'coregranularity)
                               (get-option run-spec 'core_minimization)
                               (get-option run-spec 'logtranslation)
                               (get-option run-spec 'no_overflow)
                               (if (equal? 'temporal (get-option run-spec 'problem_type))
                                   (format ":min-trace-length ~a" (get-option run-spec 'min_tracelength))
                                   "")
                               (if (equal? 'temporal (get-option run-spec 'problem_type))
                                   (format ":max-trace-length ~a" (get-option run-spec 'max_tracelength))
                                   "")))
   (pardinus:declare-univ (length all-atoms)))

  ; Declare ints
  (define num-ints (expt 2 bitwidth))
  (pardinus-print
   (pardinus:declare-ints (range (@- (/ num-ints 2)) (/ num-ints 2)) ; ints
                          (range num-ints)))                        ; indexes

  ; to-tupleset :: List<List<int>>, int -> tupleset
  (define (to-tupleset arity eles)
    (if (empty? eles)
        (if (@= arity 1)
            'none
            (pardinus:product 'none (to-tupleset (sub1 arity) eles)))
        (pardinus:tupleset #:tuples eles)))

  (define (get-atoms rel atom-names)
    (define atoms 
      (for/list ([tup atom-names])
        (for/list ([atom tup])
          (unless (member atom all-atoms)
            (raise (format "atom (~a) not in all-atoms (~a)"
                           atom all-atoms)))
          (index-of all-atoms atom))))
    (define ret (to-tupleset (relation-arity rel) atoms))
    ret)

  (for ([rel (get-all-rels run-spec)]
        [bound total-bounds])
    (pardinus-print
     (pardinus:declare-rel
      (if (node/expr/relation-is-variable rel)
          (pardinus:x (relation-name rel))
          (pardinus:r (relation-name rel)))
      (get-atoms rel (bound-lower bound))
      (get-atoms rel (bound-upper bound)))))

  ; Declare assertions
  (define all-rels (get-all-rels run-spec))
  
  ; Keep track of which formula corresponds to which CLI assert
  ; for highlighting unsat cores. TODO: map back from CLI output
  ; constraints later
  (define core-map (make-hash))
  
  (for ([p run-constraints]
        [assertion-number (in-naturals)])
    (hash-set! core-map assertion-number p)
    (pardinus-print
     (pardinus:print-cmd-cont "(~a " (pardinus:f assertion-number))
     (translate-to-kodkod-cli run-spec p all-rels all-atoms '())
     (pardinus:print-cmd ")")
     (pardinus:assert (pardinus:f assertion-number))))

  ; target-oriented model finding may not impose an initial target, but might
  ; be used just to implement custom "next" strategies
  (when (equal? 'target (get-option run-spec 'problem_type))
    (define target (Run-spec-target run-spec))    
    (when target
      (for ([(rel-name atoms) (Target-instance target)])
        (define relation (hash-ref (get-relation-map run-spec) (symbol->string rel-name)))
        (define sig-or-rel
          (if (@= (relation-arity relation) 1)
              (get-sig run-spec relation)
              (get-relation run-spec relation)))
        
        (pardinus-print
         (pardinus:declare-target 
          (pardinus:r (relation-name relation))
          (get-atoms relation atoms)))))

    ; Always say what mode; admittedly this won't always make sense if untargeted
    ; Conflate "target distance" declared with a concrete target and global mode.
    ;    Note well: the space of possible options should mirror the contract on this field.
    (pardinus-print
     (pardinus:print-cmd "(target-option target-mode ~a)"
                         (if target
                             (Target-distance target)
                             (get-option run-spec 'target_mode)))))

  ; Close the "with" scope.
  (pardinus-print (pardinus:print-cmd ")"))
  (pardinus-print (pardinus:print-eoi))
  ; Wait for the acknowledgement before sending more messages.
  (pardinus:read-ack stdout stderr)

  ; Done with the problem spec. Return any needed shared data specific to this backend.
  (values all-rels core-map))




(define (format-statistics stats)
  (let* ([vars (assoc 'size-variables stats)]
         [prim (assoc 'size-primary stats)]
         [clauses (assoc 'size-clauses stats)]
         [tt (assoc 'time-translation stats)]
         [ts (assoc 'time-solving stats)]
         [tcx (assoc 'time-core stats)]
         [tcstr (if tcx (format " Core min (ms): ~a" tcx) "")])
    (format "#vars: ~a; #primary: ~a; #clauses: ~a~nTransl (ms): ~a; Solving (ms): ~a~a"
            vars prim clauses tt ts tcstr)))
  
; Print solve
(define (get-next-kodkod-model is-running? run-name all-rels all-atoms core-map stdin stdout stderr [mode ""])

  ; Print targets
  (define-syntax-rule (pardinus-print lines ...)
    (pardinus:cmd [stdin] lines ...)) 
  
  ; If the solver isn't running at all, error:
  (unless (is-running?)
    (raise-user-error "KodKod server is not running."))
  ; If the solver is running, but this specific run ID is closed, user error
  (when (is-run-closed? run-name)
    (raise-user-error (format "Run ~a has been closed." run-name)))
    
  (pardinus-print (pardinus:solve run-name mode))
  (define result (translate-from-kodkod-cli
                  'run 
                  (pardinus:read-solution stdout stderr) 
                  all-rels 
                  all-atoms))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Note on cores: if core granularity is high, Kodkod may return a formula we do not have an ID for.
  ; In these cases, the engine should be passing something like "f:0,0" which indexes _child_ formulas.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (traverse-path-list path [fmla #f])
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
                                  (hash-ref core-map idx))]
             [(node/formula/quantified? fmla)
              ; Quantified: decls formulas first, then sub-formula last
              (cond [(>= idx (length (node/formula/quantified-decls fmla)))
                     (traverse-path-list (rest path)
                                         (node/formula/quantified-formula fmla))]
                    [else
                     (define decl (list-ref (node/formula/quantified-decls fmla) idx))
                     (traverse-path-list (rest path)
                                         (car decl))])]
             [(node/formula/op? fmla)
              ; Operator formula: sub-formulas in order. Note that this layer isn't shown
              ; to Pardinus, so we cannot move down the path index list for this.
              (traverse-path-list (rest path)
                                  (list-ref (node/formula/op-children fmla) idx))]
             [(node/fmla/pred-spacer? fmla)
              ; Predicate spacer, just use internal formula, and don't move forward in the path
              (traverse-path-list path
                                  (node/fmla/pred-spacer-expanded fmla))]
             [else
              (raise-user-error (format "Unsupported formula type in core: ~a" fmla))])]))
    
  (define (find-core-formula id)
    (unless (string-prefix? id "f:")
      (raise-user-error (format "Unexpected error: invalid formula path ID: ~a" id)))
    (define path (string-split (first (string-split id "f:")) ","))
    (printf "core path: ~a~n" path)
    (unless (and (> (length path) 0) (member (string->number (first path)) (hash-keys core-map)))
      (raise-user-error (format "Unexpected error: solver path ID prefix was invalid: ~a; valid prefixes: ~a" id (hash-keys core-map))))
    (traverse-path-list path #f))
    
  (define (pretty-core idx max known? fmla-or-id)
    (cond [known?
           (define fmla fmla-or-id)
           (fprintf (current-error-port) "Core(part ~a/~a): [~a] ~a~n" (@+ idx 1) max
                    (pretty-loc fmla) (deparse fmla))]
          [(string-prefix? fmla-or-id "f:")
           (define fmla (find-core-formula fmla-or-id))
           (fprintf (current-error-port) "Core(part ~a/~a): [~a] ~a~n" (@+ idx 1) max
                    (pretty-loc fmla) (deparse fmla))]
          [else
           (fprintf (current-error-port) "Core(part ~a/~a): [UNKNOWN] ~a~n" (@+ idx 1) max
                    fmla-or-id)]))
    
    
  (when (and (Unsat? result) (Unsat-core result)) ; if we have a core
    (when (@>= (get-verbosity) VERBOSITY_DEBUG)
      (printf "core-map: ~a~n" core-map)
      (printf "core: ~a~n" (Unsat-core result)))
    (when (@>= (get-verbosity) VERBOSITY_LOW) 
      (printf "Unsat core available (~a formulas):~n" (length (Unsat-core result))))
    (for ([id (Unsat-core result)]
          [idx (range (length (Unsat-core result)))])
      (let ([fmla-num (if (string-prefix? id "f:") (string->number (substring id 2)) #f)])
        (cond [(member fmla-num (hash-keys core-map))
               ; This is a formula ID and we know it immediately; it's a top-level constraint
               (pretty-core idx (length (Unsat-core result)) fmla-num (hash-ref core-map fmla-num))]
              [else
               ; This is NOT a known top-level constraint, but it's part of the core
               (pretty-core idx (length (Unsat-core result)) #f id)]))))
    
  (when (@>= (get-verbosity) VERBOSITY_LOW)
    (displayln (format-statistics (if (Sat? result) (Sat-stats result) (Unsat-stats result)))))
  result)