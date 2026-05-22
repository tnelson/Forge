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
(require forge/solver-specific/translate-to-kodkod-cli
         forge/solver-specific/translate-from-kodkod-cli)

(require (prefix-in @ (only-in racket/base >= not - = and or max > < +))
         (only-in racket match first rest empty empty? set->list list->set set-intersect set-union
                         curry range index-of pretty-print filter-map string-prefix? string-split thunk*
                         remove-duplicates subset? cartesian-product match-define cons? set-subtract
                         findf third sort)
          racket/hash)

(require forge/solver-specific/pardinus-cores)

(provide send-to-kodkod get-next-kodkod-model)

(define (send-to-kodkod run-name run-spec bitwidth all-atoms solverspec total-bounds bound-lower bound-upper run-constraints stdin stdout stderr)

  ; Print targets
  (define-syntax-rule (pardinus-print lines ...)
    (pardinus:cmd [stdin] lines ...))

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
            (raise-forge-error
              #:msg (format "Atom `~a in bounds for ~a is not a member of any sig. Ensure all atoms in field or relation bounds are also declared in a sig bound. (all-atoms: ~a)"
                            atom (relation-name rel) all-atoms)
              #:context rel))
          (index-of all-atoms atom))))
    (define ret (to-tupleset (relation-arity rel) atoms))
    ret)

  (define all-rels (get-all-rels run-spec))

  ; Keep track of which formula corresponds to which CLI assert
  ; for highlighting unsat cores and for counterfactual relaxation.
  ; The mapping is the same across the original run and any relaxation
  ; re-issues, since we keep assertion numbering stable.
  (define core-map (make-hash))
  (for ([p run-constraints]
        [assertion-number (in-naturals)])
    (hash-set! core-map assertion-number p))

  ; Emit a full (with <name> ...) problem to the solver. If dropped-ids
  ; contains an assertion-number, that assertion's body is replaced with
  ; `true` (so Pardinus keeps the f:N identifier in scope but the formula
  ; doesn't constrain anything). Used both for the initial problem and for
  ; counterfactual relaxation re-issues, which need to drop a subset of
  ; the original assertions and re-solve under a fresh run-name.
  (define (emit-problem effective-run-name dropped-ids include-target?)
    (pardinus-print (pardinus:print-cmd (format "(with ~a" effective-run-name)))
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

    (for ([rel all-rels]
          [bound total-bounds])
      (pardinus-print
       (pardinus:declare-rel
        (if (node/expr/relation-is-variable rel)
            (pardinus:x (relation-name rel))
            (pardinus:r (relation-name rel)))
        (get-atoms rel (bound-lower bound))
        (get-atoms rel (bound-upper bound)))))

    ; Emit assertion definitions and assertions. Drop a formula by emitting
    ; `(f:N true)` in place of the real formula, keeping the identifier in
    ; scope so any other references stay valid.
    (for ([p run-constraints]
          [assertion-number (in-naturals)])
      (cond
        [(memv assertion-number dropped-ids)
         (pardinus-print
          (pardinus:print-cmd "(~a true)" (pardinus:f assertion-number))
          (pardinus:assert (pardinus:f assertion-number)))]
        [else
         (pardinus-print
          (pardinus:print-cmd-cont "(~a " (pardinus:f assertion-number))
          (translate-to-kodkod-cli run-spec p all-rels all-atoms '())
          (pardinus:print-cmd ")")
          (pardinus:assert (pardinus:f assertion-number)))]))

    ; target-oriented model finding: only attach the target to the *original*
    ; run, not to relaxation re-issues. The relaxed problem inherits the
    ; user's bounds and constraints (minus dropped) but is otherwise a
    ; vanilla satisfiability problem.
    (when (and include-target?
               (equal? 'target (get-option run-spec 'problem_type)))
      (define target (Run-spec-target run-spec))
      (when target
        (for ([(rel-name atoms) (Target-target target)])
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
    (pardinus:read-ack stdout stderr))

  ; Emit the initial problem under the user's run-name. Dropped-ids is a
  ; list (treated as a set, with at most `budget` entries); we use list ops
  ; to avoid colliding with `set` already exported by forge/lang/ast.
  (emit-problem run-name '() #t)

  ; Build the relaxation context. If counterfactual mode is off, this is
  ; #f and `get-next-kodkod-model` short-circuits any UNSAT into the
  ; normal UNSAT response. If on, it's a hash carrying:
  ;   'reissue      - a closure (set-of-int -> symbol) that re-emits the
  ;                   whole problem with the given assertion-IDs replaced
  ;                   by `true`, under a freshly-generated run-name, and
  ;                   returns that new run-name.
  ;   'budget       - max relaxation iterations.
  ;   'current-name - a box holding the most recently issued relax-name
  ;                   (or #f if we haven't entered relaxation yet). Used
  ;                   by `get-next-kodkod-model` to keep "next-instance"
  ;                   requests pointed at the relaxed run after the first
  ;                   counterfactual is produced.
  (define relax-context
    (cond
      [(equal? 'on (get-option run-spec 'counterfactual))
       (define relax-counter (box 0))
       (define current-name-box (box #f))
       (define budget-opt (get-option run-spec 'counterfactual_budget))
       (define budget
         (if (exact-nonnegative-integer? budget-opt) budget-opt 5))
       (define (reissue dropped-ids)
         (set-box! relax-counter (add1 (unbox relax-counter)))
         (define relax-name
           (string->symbol (format "~a__cf~a" run-name (unbox relax-counter))))
         (emit-problem relax-name dropped-ids #f)
         (set-box! current-name-box relax-name)
         relax-name)
       (hash 'reissue reissue
             'budget budget
             'current-name current-name-box)]
      [else #f]))

  ; Done with the problem spec. Return shared data plus the relax context
  ; (which is #f when counterfactual mode is off; callers must tolerate that).
  (values all-rels core-map relax-context))




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

; Pull the top-level assertion ID out of a raw Pardinus core entry.
; Pardinus emits entries shaped like "f:3" or "f:3,1,2" — where "3" is the
; top-level assertion (matches a key in core-map) and the comma-separated
; suffix descends into sub-formulas. For relaxation we need the top-level
; piece so we can drop the corresponding (assert f:N) in the next re-issue.
; Returns #f if the entry doesn't have the expected shape (which can
; happen for opaque cores Pardinus couldn't trace back; those entries
; just get skipped by the relaxation picker).
(define (top-level-assertion-id raw-core-entry)
  (cond
    [(string? raw-core-entry)
     (cond
       [(string-prefix? raw-core-entry "f:")
        (define rest-str (substring raw-core-entry 2))
        (define head (first (string-split rest-str ",")))
        (string->number head)]
       [else #f])]
    [else #f]))

; Counterfactual relaxation loop.
;
; Preconditions: `initial-unsat` is an Unsat? value with kind 'unsat and a
; non-#f core; `relax-context` is a non-#f hash from send-to-kodkod.
;
; Strategy (naive drop-and-retry): each iteration, look at the *raw* core
; path-IDs Pardinus gave us, extract their top-level assertion IDs, pick
; the smallest one we haven't already dropped, add it to the dropped set,
; and ask `relax-context` to re-issue the problem under a fresh run-name
; with the dropped assertions replaced by `true`. Solve the new run; if
; SAT, wrap as RelaxedSat; if still UNSAT with a fresh core, loop. Bail
; out after `budget` iterations or when there are no new IDs to drop.
;
; Sorting top-level IDs ascending makes the relaxation deterministic — the
; same UNSAT spec always produces the same counterfactual (assuming the
; underlying Pardinus core itself is deterministic, which with
; `:core-minimization fast` it generally is).
(define (run-relaxation-loop initial-unsat initial-raw relax-context core-map all-rels all-atoms
                             stdin stdout stderr)
  (define-syntax-rule (pardinus-print lines ...)
    (pardinus:cmd [stdin] lines ...))

  (define reissue (hash-ref relax-context 'reissue))
  (define budget (hash-ref relax-context 'budget))
  (define original-core (Unsat-core initial-unsat))

  (let loop ([current-raw initial-raw]
             [dropped-ids '()]
             [dropped-formulas '()]
             [iters 0])
    (cond
      [(@>= iters budget)
       ; Budget exhausted — return the original UNSAT untouched so the
       ; user still sees the conflict report, just without a counterfactual.
       (when (@>= (get-verbosity) VERBOSITY_LOW)
         (printf "Counterfactual relaxation hit budget (~a) without reaching SAT.~n" budget))
       initial-unsat]
      [else
       ; current-raw is the raw read-solution result for the most recent
       ; UNSAT. Shape: (list 'unsat run-name (list path-id ...) stats)
       (define raw-core (third current-raw))
       (cond
         [(or (not raw-core) (null? raw-core))
          ; No actionable core — nothing left to drop.
          initial-unsat]
         [else
          ; Pick the smallest top-level ID we haven't already dropped.
          (define candidate-ids
            (sort (filter exact-nonnegative-integer?
                          (remove-duplicates (map top-level-assertion-id raw-core)))
                  @<))
          (define next-id
            (findf (lambda (id) (@not (memv id dropped-ids))) candidate-ids))
          (cond
            [(@not next-id)
             ; Core only references assertions we've already dropped or
             ; opaque entries we can't map back. Give up.
             (when (@>= (get-verbosity) VERBOSITY_LOW)
               (printf "Counterfactual relaxation could not pick a new constraint to drop; giving up after ~a iter(s).~n" iters))
             initial-unsat]
            [else
             (define new-dropped-ids (cons next-id dropped-ids))
             (define dropped-formula (hash-ref core-map next-id))
             (define new-dropped-formulas (cons dropped-formula dropped-formulas))
             (when (@>= (get-verbosity) VERBOSITY_LOW)
               (printf "Counterfactual: dropping assertion ~a to relax (iter ~a/~a).~n"
                       next-id (add1 iters) budget))
             (define relax-name (reissue new-dropped-ids))
             (pardinus-print (pardinus:solve relax-name ""))
             (define new-raw (pardinus:read-solution stdout stderr))
             (define new-result (translate-from-kodkod-cli
                                 'run new-raw all-rels all-atoms core-map))
             (cond
               [(Sat? new-result)
                ; Success — wrap as RelaxedSat. Reverse the list so callers
                ; see drops in the order we made them.
                (RelaxedSat (Sat-instances new-result)
                            (Sat-stats new-result)
                            (Sat-metadata new-result)
                            (reverse new-dropped-formulas)
                            original-core)]
               [(and (Unsat? new-result)
                     (Unsat-core new-result)
                     (equal? (Unsat-kind new-result) 'unsat))
                ; Still UNSAT — keep relaxing using the *new* core, which
                ; may surface assertions outside the original core.
                (loop new-raw new-dropped-ids new-dropped-formulas (add1 iters))]
               [else
                ; UNSAT without a usable core, or some other state — bail.
                initial-unsat])])])])))

; Print solve
(define (get-next-kodkod-model is-running? run-name all-rels all-atoms core-map relax-context stdin stdout stderr [mode ""])

  ; Print targets
  (define-syntax-rule (pardinus-print lines ...)
    (pardinus:cmd [stdin] lines ...))

  ; If the solver process isn't running at all, error:
  (unless (is-running?)
    (raise-user-error "KodKod server is not running."))
  ; If the solver is running, but this specific run ID is closed, user error
  (when (is-run-closed? run-name)
    (raise-user-error (format "Run ~a has been closed." run-name)))

  ; If we've already done one round of counterfactual relaxation in this
  ; session, subsequent "next instance" requests should query the *relaxed*
  ; run, not the original UNSAT one. Otherwise the user clicks "next" on
  ; the counterfactual diagram and is silently dropped back to UNSAT.
  (define already-relaxed-name
    (and relax-context (unbox (hash-ref relax-context 'current-name))))
  (define effective-run-name (or already-relaxed-name run-name))

  (pardinus-print (pardinus:solve effective-run-name mode))
  (define raw-solution (pardinus:read-solution stdout stderr))
  (define result (translate-from-kodkod-cli
                  'run
                  raw-solution
                  all-rels
                  all-atoms
                  core-map))

  ; Counterfactual relaxation: only enter on a fresh UNSAT (not when the
  ; relaxed run itself returns UNSAT — that just means "no more instances
  ; of the counterfactual"). The 'unsat kind check excludes 'no-counterexample
  ; and 'no-more-instances, which are not "the problem contradicts itself"
  ; conditions and have no useful relaxation semantics.
  (define final-result
    (cond
      [(and relax-context
            (@not already-relaxed-name)
            (Unsat? result)
            (Unsat-core result)
            (equal? (Unsat-kind result) 'unsat))
       (run-relaxation-loop result raw-solution relax-context core-map all-rels all-atoms
                            stdin stdout stderr)]
      [else result]))

  (when (and (Unsat? final-result) (Unsat-core final-result)) ; if we have a core
    (when (@>= (get-verbosity) VERBOSITY_DEBUG)
      (printf "core-map: ~a~n" core-map)
      (printf "core: ~a~n" (Unsat-core final-result)))
    (when (@>= (get-verbosity) VERBOSITY_LOW)
      (printf "Unsat core available (~a formulas):~n" (length (Unsat-core final-result))))
    (for ([id (Unsat-core final-result)]
          [idx (range (length (Unsat-core final-result)))])
               (pretty-print-core-formula idx (length (Unsat-core final-result)) id core-map)))

  ; Briefly summarize a counterfactual result so it's visible in the
  ; console alongside the existing core display for plain UNSAT.
  (when (and (RelaxedSat? final-result) (@>= (get-verbosity) VERBOSITY_LOW))
    (define dropped (RelaxedSat-dropped final-result))
    (printf "Counterfactual instance found by dropping ~a constraint(s):~n"
            (length dropped))
    (for ([fmla dropped]
          [idx (range (length dropped))])
      (pretty-print-core-formula idx (length dropped) fmla core-map)))

  (when (@>= (get-verbosity) VERBOSITY_LOW)
    (displayln (format-statistics (if (Sat? final-result) (Sat-stats final-result) (Unsat-stats final-result)))))
  final-result)
