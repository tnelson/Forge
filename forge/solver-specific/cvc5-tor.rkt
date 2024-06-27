#lang racket/base

; CVC5/theory-of-relations specific translation functions. Generally called from send-to-solver.rkt.

(require forge/sigs-structs
         forge/lang/ast
         forge/shared
         forge/lang/bounds)

(require (prefix-in @ (only-in racket/base >= not - = and or max > < +))
         (only-in racket match first rest empty empty? set->list list->set set-intersect set-union
                         curry range index-of pretty-print filter-map string-prefix? string-split thunk*
                         remove-duplicates subset? cartesian-product match-define cons? set-subtract)
          racket/hash)

; TODO: connect w/ translation in and translation out
; TODO: is it possible to have multiple simultaneous runs in cvc5?

(provide send-to-cvc5-tor get-next-cvc5-tor-model)

; Assumes the backend server is already running.
(define (send-to-cvc5-tor run-name run-spec bitwidth all-atoms solverspec
                      total-bounds bound-lower bound-upper run-constraints stdin stdout stderr)
   
  ; Declare assertions
  (define all-rels (get-all-rels run-spec))
  
  ; Keep track of which formula corresponds to which CLI assert
  ; for highlighting unsat cores. (TODO: this is not high priority yet re: CVC5)
  (define core-map (make-hash))
    
  ; Send the problem spec to cvc5 via the stdin/out/err connection already opened
  ;; TODO: other arguments may be needed also
  (define cvc5-command (translate-to-cvc5-tor total-bounds run-constraints))

  ; TODO: not yet implemented
  ;(cvc5-send stdin stdout stderr cvc5-command)
  
  ; Done with the problem spec. Return any needed shared data specific to this backend.
  (values all-rels core-map))


(define (translate-to-cvc5-tor total-bounds run-constraints)
  ; For now, just print constraints, etc. 
  (printf "Translating to CVC5 theory-of-relations~nConstraints:~n")
  (for ([constraint run-constraints])
    (printf "  ~a~n" constraint))
  (printf "Bounds:~n")
  (for ([bound total-bounds])
    (printf "  ~a/lower: ~a~n" (bound-relation bound) (bound-lower bound))
    (printf "  ~a/upper: ~a~n" (bound-relation bound) (bound-upper bound)))

  ; Here is where I'd plug in the conversion pipeline, based on the real solver problem.

  )


; duplicated from pardinus code
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

; No core support yet, see pardinus for possible approaches
(define (get-next-cvc5-tor-model is-running? run-name all-rels all-atoms core-map stdin stdout stderr [mode ""])
 
  ; If the solver isn't running at all, error:
  (unless (is-running?)
    (raise-user-error "CVC5 server is not running."))

  ; If the solver is running, but this specific run ID is closed, user error
  (when (is-run-closed? run-name)
    (raise-user-error (format "Run ~a has been closed." run-name)))
    
  ;(define sat? (smtlib-check-sat))
  ;(define soln (if sat? (smtlib-get-model) #f))
  ;(define result (translate-from-cvc5 'run soln all-rels all-atoms))

  (define result (Unknown #f #f)) ; no stats, no metadata
  
  ;(when (@>= (get-verbosity) VERBOSITY_LOW)
  ;  (displayln (format-statistics (if (Sat? result) (Sat-stats result) (Unsat-stats result)))))
  result)