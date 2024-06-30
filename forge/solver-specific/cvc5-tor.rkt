#lang racket/base

; CVC5/theory-of-relations specific translation functions. Generally called from send-to-solver.rkt.

(require forge/sigs-structs
         forge/lang/ast
         forge/shared
         forge/lang/bounds
         forge/solver-specific/smtlib-shared
         (prefix-in nnf: forge/utils/to-nnf)
         (prefix-in boxed-int: forge/utils/integer-converter)
         (prefix-in skolemize: forge/utils/to-skolem)
         (prefix-in smt-tor: forge/utils/to-smtlib-tor)
         )

(require (prefix-in @ (only-in racket/base >= not - = and or max > < +))
         (only-in racket match first rest empty empty? set->list list->set set-intersect set-union
                         curry range index-of pretty-print filter-map string-prefix? string-split thunk*
                         remove-duplicates subset? cartesian-product match-define cons? set-subtract
                         string-replace second string-join)
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
  (define cvc5-command (translate-to-cvc5-tor run-spec all-atoms all-rels total-bounds run-constraints))
  (printf "~nSENDING TO CVC5:~n~a~n-------------------------~n" cvc5-command)
  (smtlib-display stdin cvc5-command)
  
  ; Done with the problem spec. Return any needed shared data specific to this backend.
  (values all-rels core-map))

(define (deparen lst)
  (string-join lst " "))

(define (convert-bound b)
  ; TODO: for now, assume we have exact bounds, and just use the upper
  ; For KM: let's discuss this!
  ;(printf "convert-bound: ~a~n" b)
  (define name (relation-name (bound-relation b)))
  (define arity (relation-arity (bound-relation b)))
  (define typenames ((relation-typelist-thunk (bound-relation b))))
  (cond
    ; Don't declare Int at all
    [(equal? name "Int")
     ""]
    ; Sigs: unary, and not a skolem name
    [(and (equal? arity 1) (not (equal? (string-ref name 0) #\$)))
     ; If we are declaring sigs as _sorts_, we need a separate relation to store
     ; atoms of that sort that appear in the instance. (Note "Sort" suffix below)
     ;; TODO: we should only declare top-level sigs as sorts
     (format "(declare-sort ~aSort 0)~n~a~n(declare-fun ~a () (Relation ~aSort))"
             ; Declare sort for this sig
             name
             ; Declare constant atoms for this sig
             (deparen (map (lambda (tup)
                             (format "(declare-const ~a ~aSort)~n" (first tup) name))
                           (bound-upper b)))
             ; Declare the "used" relation for this sig
             name name)]
    ; Skolem relation of arity 1
    [(equal? (string-ref name 0) #\$)
     ; Temporarily disable situations where Skolem relations would need to be non-nullary
     ; this "as uninterpreted function" approach required us to wrap references to these
     ; in (singleton (tuple ...)) in to-smtlib-tor.rkt.
     (unless (equal? arity 1)
       (raise-forge-error #:msg (format "SMT backend does not currently support problems that require Skolemization depth > 0; ~a had arity ~a." name arity)
                          #:context #f))
     (format "(declare-const ~a ~a)~n" name (deparen (map sort-name-of typenames)))]
    ; Fields
    [else
     (format "(declare-fun ~a () (Relation ~a))~n" name (deparen (map sort-name-of typenames)))]))

(define (translate-to-cvc5-tor run-spec all-atoms relations total-bounds step0)
  ; For now, just print constraints, etc.
  (printf "~n********************************~n")
  (printf "Translating to CVC5 theory-of-relations~nConstraints:~n")

  ; Last version pre-solver, pre-SMT conversion
  (when (@> (get-verbosity) VERBOSITY_LOW)
    (printf "~nStep 0 (from Forge):~n")
    (for ([constraint step0])
      (printf "  ~a~n" constraint)))

  ; Convert to negation normal form
  (define step1 (map (lambda (f) (nnf:interpret-formula run-spec f relations all-atoms '())) step0))
  (when (@> (get-verbosity) VERBOSITY_LOW)
    (printf "~nStep 1 (post NNF conversion):~n")
    (for ([constraint step1])
      (printf "  ~a~n" constraint)))

  ; Convert boxed integer references to existential quantifiers
  (define step2 (map (lambda (f) (boxed-int:interpret-formula run-spec f relations all-atoms '())) step1))
  (when (@> (get-verbosity) VERBOSITY_LOW)
    (printf "~nStep 2 (post boxed-integer translation):~n")
    (for ([constraint step2])
      (printf "  ~a~n" constraint)))

  ; Skolemize (2nd empty list = types for quantified variables, unneeded in other descents)
  ; Note that Skolemization changes the *final* bounds. There is no Run struct for this run yet;
  ; it is only created after send-to-solver returns. So there is no "kodkod-bounds" field to start with.
  ; Instead, start with the total-bounds produced.
  (define step3-both
    (for/fold ([fs-and-bs (list '() total-bounds)])
              ([f step2])
      ; Accumulator starts with: no formulas, original total-bounds
      (define-values (resulting-formula new-bounds)
        (skolemize:interpret-formula run-spec (second fs-and-bs) f relations all-atoms '() '()))
      ; Accumulator adds the skolemized formula, updated bounds
      (list (cons resulting-formula (first fs-and-bs))
            new-bounds)))
  
  (define step3 (first step3-both))
  (define step3-bounds (second step3-both))
  (when (@> (get-verbosity) VERBOSITY_LOW)
    (printf "~nStep 3 (post Skolemization):~n")
    (for ([constraint step3])
      (printf "  ~a~n" constraint)))
  
  (define step4 (map (lambda (f) (smt-tor:convert-formula run-spec f relations all-atoms '())) step3))
  (when (@> (get-verbosity) VERBOSITY_LOW)
    (printf "~nStep 4 (post SMT-LIB conversion):~n")
    (for ([constraint step4])
      (printf "  ~a~n" constraint))
    (printf "~nBounds (post Skolemization):~n")
    (for ([bound step3-bounds])
      (printf "  ~a/lower: ~a~n" (bound-relation bound) (bound-lower bound))
      (printf "  ~a/upper: ~a~n" (bound-relation bound) (bound-upper bound))))

  ; Now, convert bounds into SMT-LIB (theory of relations) and assert all formulas

  ; preamble: the (reset) makes (set-logic ...) etc. not give an error on 2nd run
  ; Note some other options are set in cvc5-server.rkt!
  ; TODO: we shouldn't need to explicitly reset, but it isn't always being done
  (define preamble-str (format "(reset)~n(set-logic ALL)~n"))

  ; converted bounds:
  (define bounds-str (string-join (map convert-bound step3-bounds) "\n"))

  ; converted formula:
  (define assertions-str (string-join (map (lambda (s) (format "(assert ~a)" s)) step4) "\n"))

  ; DO NOT (check-sat) yet.
  
  (format "~a~n~a~n~a~n" preamble-str bounds-str assertions-str))

; No core support yet, see pardinus for possible approaches
(define (get-next-cvc5-tor-model is-running? run-name all-rels all-atoms core-map stdin stdout stderr [mode ""])
  (printf "Getting next model from CVC5...~n")
  
  ; If the solver isn't running at all, error:
  (unless (is-running?)
    (raise-user-error "CVC5 server is not running."))

  ; If the solver is running, but this specific run ID is closed, user error
  (when (is-run-closed? run-name)
    (raise-user-error (format "Run ~a has been closed." run-name)))
    
  ; Mock an SMT-LIB input using theory of relations. Keep the port open!
  ;(define mock-problem (port->string (open-input-file "own-grandpa.smt2" #;"cvc5.smt") #:close? #f))
  ;(smtlib-display stdin mock-problem)
  ; ASSUME: reply format is a single line for the result type, then
  ;   a paren-delimited s-expression with any output of that 
  (smtlib-display stdin "(check-sat)")
  (define sat-answer (read stdout))

  ;; TODO: statistics: https://cvc5.github.io/docs/cvc5-1.0.2/statistics.html
  ;; TODO: extract core

  ;; Note: CVC5 interactive mode won't process newlines properly;
  ;; https://github.com/cvc5/cvc5/issues/10414
  ;; As of Jun 28 2024, need one command per line
  
  (define result
    (match sat-answer
    ['sat
     ; No statistics or metadata yet 
     (begin
       (smtlib-display stdin "(get-model)")
       (define model-s-expression (read stdout))
       (when (@> (get-verbosity) VERBOSITY_LOW)
         (printf "----- RECEIVED -----~n")
         (for ([s-expr model-s-expression])
           (printf "~a~n" s-expr))
         (printf "--------------------~n"))
         
       ; Forge still uses the Alloy 6 modality overall: Sat structs contain a _list_ of instances,
       ; each corresponding to the state of the instance at a given time index. Here, just 1.
       (define response (Sat (list (smtlib-tor-to-instance model-s-expression)) #f '()))
       (when (@> (get-verbosity) VERBOSITY_LOW)
         (printf "~nSAT: ~a~n" response))
       response)]
    ['unsat
     (when (@> (get-verbosity) VERBOSITY_LOW)
       (printf "~nUNSAT~n"))
     ; No cores or statistics yet
     (Unsat #f #f 'unsat)]
    ['unknown
     ; No statistics yet
     (Unknown #f #f)]
    [else
     (printf "STDERR: ~a~n" (port->string stderr)) ;; TODO: will this block?
     (raise-forge-error #:msg (format "Received unexpected response from CVC5: ~a" sat-answer)
                        #:context #f)]))
  result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Converting SMT-LIB response expression into a Forge instance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Building this up from running examples, so the cases may be a bit over-specific
(define (smtlib-tor-to-instance model-s-expression)
  (for/fold ([inst (hash)])
            ([part model-s-expression])
    (define-values (key value)
      (match part

        ; Relational value: empty set
        [(list (quote define-fun) ID (list) TYPE (list (quote as) (quote set.empty) ATOMTYPE))
         (values ID '())]
        
        ; Constant bindings to atoms: return unary relation of one tuple
        [(list (quote define-fun) ID (list) TYPE (list (quote as) ATOMID ATOMTYPE))
         (values ID (list (list (process-atom-id ATOMID))))]

        ; Uninterpreted function w/ constant value
        [(list (quote define-fun) ID (list ARGS-WITH-TYPES) TYPE (list (quote as) ATOMID ATOMTYPE))
         ;; TODO: look at bounds given and assemble the domain of this function, cross-product with value
         (values ID (list (list (process-atom-id ATOMID))))]
        
        ; Relational value: union (may contain any number of singletons)
        [(list (quote define-fun) ID (list) TYPE (list (quote set.union) ARGS ...))
         ; A union may contain an inductive list built from singletons and unions,
         ;   or multiple singletons within a single list. So pre-process unions
         ;   and flatten to get a list of singletons.
         (define singletons (apply append (map process-union-arg ARGS)))
         (values ID (process-singleton-list singletons))]
        
        ; Relational value: singleton (should contain a single tuple)
        [(list (quote define-fun) ID (list) TYPE (list (quote set.singleton) ARG))
         ; Wrap this tuple, because this is a singleton _set_
         (values ID (list (process-tuple ARG)))]
        
        ; Catch-all; unknown format
        [else
         (raise-forge-error #:msg (format "Unsupported response s-expression from SMT-LIB: ~a" part)
                            #:context #f)]))
    (hash-set inst key value)))

; Return a list containing singletons
(define (process-union-arg arg)
  (match arg
    [(list (quote set.singleton) TUPLE)
     (list arg)]
    [(list (quote set.union) ARGS ...)
     (apply append (map process-union-arg ARGS))]
    [else
     (raise-forge-error #:msg (format "Unsupported response s-expression from SMT-LIB (process-union-arg): ~a" arg)
                        #:context #f)]))
(define (process-singleton-list args)
  (match args
    ; a list of singletons (unions should be removed prior)
    [(list (list (quote set.singleton) TUPLES) ...)
     (map process-tuple TUPLES)]
    [else
     (raise-forge-error #:msg (format "Unsupported response s-expression from SMT-LIB (process-singleton-list): ~a" args)
                        #:context #f)]))

(define (process-tuple tup)
  (match tup
    ; Each atom may be an SMT-LIB "qualified identifier" or may be a raw value (e.g., an integer)
    [(list (quote tuple) ATOMS ...)
     (map process-atom ATOMS)]))
(define (process-atom atom-expr)
 (match atom-expr
   [(list (quote as) ATOMID ATOMTYPE)
     (process-atom-id ATOMID)]
   [ATOMID
     (process-atom-id ATOMID)]))

(define (process-atom-id atom-id)
  (define string-atom-id (format "~a" atom-id))
  (string->symbol
   (string-replace (string-replace string-atom-id "@" "") "_" "$")))

;(smtlib-tor-to-instance
;'((define-fun spouse () (Set (Tuple Person Person)) (set.union (set.singleton (tuple (as @Person_0 Person) (as @Person_3 Person))) (set.union (set.singleton (tuple (as @Person_2 Person) (as @Person_1 Person))) (set.union (set.singleton (tuple (as @Person_3 Person) (as @Person_0 Person))) (set.singleton (tuple (as @Person_1 Person) (as @Person_2 Person)))))))
;))
