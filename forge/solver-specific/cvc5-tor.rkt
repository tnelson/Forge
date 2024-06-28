#lang racket/base

; CVC5/theory-of-relations specific translation functions. Generally called from send-to-solver.rkt.

(require forge/sigs-structs
         forge/lang/ast
         forge/shared
         forge/lang/bounds
         forge/solver-specific/smtlib-shared)

(require (prefix-in @ (only-in racket/base >= not - = and or max > < +))
         (only-in racket match first rest empty empty? set->list list->set set-intersect set-union
                         curry range index-of pretty-print filter-map string-prefix? string-split thunk*
                         remove-duplicates subset? cartesian-product match-define cons? set-subtract)
          racket/hash
          racket/port)

; TODO: connect w/ translation in and translation out
; TODO: is it possible to have multiple simultaneous runs in cvc5?

(provide send-to-cvc5-tor get-next-cvc5-tor-model smtlib-display)

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
  ;(smt5-display stdin cvc5-command)
  
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

  ; Mock an SMT-LIB input using theory of relations. Keep the port open!
  (define mock-problem (port->string (open-input-file "cvc5.smt") #:close? #f))
  (smtlib-display stdin mock-problem)
  ; ASSUME: reply format is a single line for the result type, then
  ;   a paren-delimited s-expression with any output of that 
  (smtlib-display stdin "(check-sat)")
  (define sat-answer (read stdout))

  ;; TODO: stderr handling
  ;; TODO: extract instance
  ;; TODO: close-run not via pardinus!!
  ;; TODO: multiple runs? (start with just (clear)?)
  
  (define result
    (match sat-answer
    ['sat
     ; No statistics or metadata yet
     (begin
       (smtlib-display stdin "(get-model)")
       (define model-s-expression (read stdout))
       (for ([s-expr model-s-expression])
         (printf "~a~n" s-expr))
       (Sat '() #f #f))]
    ['unsat
     ; No cores or statistics yet
     (Unsat #f #f 'unsat)]
    ['unknown
     ; No statistics yet
     (Unknown #f #f)]
    [else (raise-forge-error #:msg (format "Received unexpected response from CVC5: ~a" sat-answer)
                             #:context #f)]))
  result)