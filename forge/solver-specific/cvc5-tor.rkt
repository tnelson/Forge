#lang racket/base

; CVC5/theory-of-relations specific translation functions. Generally called from send-to-solver.rkt.

(require forge/sigs-structs
         forge/lang/ast
         forge/shared
         forge/lang/bounds
         forge/solver-specific/smtlib-shared
         forge/last-checker
         (prefix-in nnf: forge/utils/to-nnf)
         (prefix-in boxed-int: forge/utils/integer-converter)
         (prefix-in skolemize: forge/utils/to-skolem)
         (prefix-in smt-tor: forge/utils/to-smtlib-tor)
         (prefix-in quant-grounding: forge/utils/quantifier-grounding)
         (prefix-in lazy-tree: forge/lazy-tree)
         )

(require (prefix-in @ (only-in racket/base >= not - = and or max > < +))
         (only-in racket match first rest empty empty? set->list list->set set-intersect set-union
                         curry range index-of pretty-print filter-map string-prefix? string-split thunk*
                         remove-duplicates subset? cartesian-product match-define cons? set-subtract
                         string-replace second string-join take last)
          racket/hash
          racket/port)

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

;; Helper function to declare constants for each upper bound element
(define (const-declarations bound)
         (deparen (map (lambda (tup)
                         (format "(declare-const ~a Atom)~n" (first tup)))
                       (bound-upper bound))))

;; Helper function to create membership assertions between each upper bound element and relation
(define (create-bound-membership bound rel-name)
    (format "~a" 
      (deparen (map (lambda (tup)
                      (format "(assert (set.member (tuple ~a) ~a))~n" (first tup) rel-name))
                    (bound-upper bound)))))

;; Function to create a constraint asserting that the relation is the union of the constants as singletons
(define (relation-constraint bound rel-name one?)
  (if (not one?)
  (format "(assert (= ~a (set.union ~a)))~n"
    rel-name
    (deparen (map (lambda (tup)
                      (format "(set.singleton (tuple ~a))" (first tup)))
                    (bound-upper bound))))
  (format "(assert (= ~a (set.singleton (tuple ~a))))~n" rel-name (car (first (bound-upper bound))))))

(define (child-constraint run-or-state bound)
  (define name (relation-name (bound-relation bound)))
  (define arity (relation-arity (bound-relation bound)))
  (cond 
    [(equal? name "Int")
    ""]
    ; we only want this to operate on arity 1 non-skolem relations, aka sigs
    [(and (equal? arity 1) (not (equal? (string-ref name 0) #\$)))
      (define primsigs (if (Sig? (bound-relation bound)) (primify run-or-state (Sig-name (bound-relation bound))) '()))
      (define remove-remainder-lambda (lambda (sig-name) (regexp-replace #rx"_.*$" (symbol->string sig-name) "")))
      (cond [(or (equal? 1 (length primsigs)) (empty? primsigs)) ""]
            [else
            (format "(assert (= ~a (set.union ~a)))~n"
                    (relation-name (bound-relation bound))
                    (deparen (map remove-remainder-lambda primsigs)))])
    ]
    [(equal? (string-ref name 0) #\$)
    ""]
    [else ""]
  )
)

(define (convert-bound b)
  (define name (relation-name (bound-relation b)))
  (define arity (relation-arity (bound-relation b)))
  (define typenames ((relation-typelist-thunk (bound-relation b))))
  (define parent (relation-parent (bound-relation b)))
  (define one? (if (Sig? (bound-relation b)) (Sig-one (bound-relation b)) #f))
  (define abstract? (if (Sig? (bound-relation b)) (Sig-abstract (bound-relation b)) #f))
  (cond
    ; Don't declare Int at all
    [(equal? name "Int")
     ""]
    ; Sigs: unary, and not a skolem name
    [(and (equal? arity 1) (not (equal? (string-ref name 0) #\$)))
     (format "~a~n(declare-fun ~a () (Relation Atom))~n~a~n"
             ; Only instantiate constants for top level sigs
             (if (equal? parent "univ") (const-declarations b) "")
             name
             ; Only create membership assertions for top level sigs or one sigs
             (if (or one? (equal? parent "univ")) (create-bound-membership b name) ""))]
    ; Skolem relation
    [(equal? (string-ref name 0) #\$)
     (cond [(equal? arity 1)
            (format "(declare-const ~a ~a)~n" name (deparen (map atom-or-int typenames)))]
           [else
            (define domain-typenames (take typenames (@- (length typenames) 1)))
            (define codomain-typename (last typenames))
            (format "(declare-fun ~a (~a) ~a)" name
                    (deparen (map atom-or-int domain-typenames))
                    (atom-or-int codomain-typename))])]
    ; Fields
    [else
    ; Fields are declared as relations of the appropriate arity of atoms or ints
     (format "(declare-fun ~a () (Relation ~a))~n" name (deparen (map atom-or-int typenames)))]))

(define (form-disjoint-string relations)
  (define top-level '())
  (for/fold ([top-level top-level])
            ([relation relations])
    (cond 
          [(or (not (equal? (relation-parent relation) "univ")) (equal? (relation-name relation) "Int")) top-level]
          [(Sig? relation)          
            (define new-top-level (cons (relation-name relation) top-level)) 
            new-top-level]
          [else top-level]
    )
  )
)

(define (disjoint-relations rel-names)
  (define (pairwise-disjoint relation1 relation2)
      (if (equal? relation1 relation2) ""
      (format "(assert (= (set.inter ~a ~a) (as set.empty (Relation Atom))))"
        relation1 relation2)))
  (define pairs (cartesian-product rel-names rel-names))
  (string-join (map (lambda (pair) (pairwise-disjoint (first pair) (second pair))) pairs) "\n"))

(define (translate-to-cvc5-tor run-spec all-atoms relations total-bounds step0)
  ; For now, just print constraints, etc.
  (printf "~n********************************~n")
  (printf "Translating to CVC5 theory-of-relations~nConstraints:~n")

  ; First concept of turning run-spec into run statement:
  (define fake-solution-tree (lazy-tree:make-node #f #f))
  (define fake-server-ports (Server-ports #f #f #f #f #f))
  (define fake-kodkod-current (Kodkod-current 1 2 3))
  (define fake-run (Run 'fake #'fake run-spec fake-solution-tree 
                        fake-server-ports all-atoms fake-kodkod-current total-bounds (box #f)))

  ; Last version pre-solver, pre-SMT conversion
  (when (@> (get-verbosity) VERBOSITY_LOW)
    (printf "~nStep 0 (from Forge):~n")
    (for ([constraint step0])
      (printf "  ~a~n" constraint)))

  ; Convert to negation normal form
  (define step1 (map (lambda (f) (nnf:interpret-formula fake-run f relations all-atoms '())) step0))
  (when (@> (get-verbosity) VERBOSITY_LOW)
    (printf "~nStep 1 (post NNF conversion):~n")
    (for ([constraint step1])
      (printf "  ~a~n" constraint)))

  ; Skolemize (2nd empty list = types for quantified variables, unneeded in other descents)
  ; Note that Skolemization changes the *final* bounds. There is no Run struct for this run yet;
  ; it is only created after send-to-solver returns. So there is no "kodkod-bounds" field to start with.
  ; Instead, start with the total-bounds produced.
  (define step2-both
    (for/fold ([fs-and-bs (list '() total-bounds)])
              ([f step1])
      ; Accumulator starts with: no formulas, original total-bounds
      (define-values (resulting-formula new-bounds)
        (skolemize:interpret-formula fake-run (second fs-and-bs) f relations all-atoms '() '()
                                     #:tag-with-spacer #t))
      ; Accumulator adds the skolemized formula, updated bounds
      (list (cons resulting-formula (first fs-and-bs))
            new-bounds)))
  
  (define step2 (first step2-both))
  (define step2-bounds (second step2-both))
  (when (@> (get-verbosity) VERBOSITY_LOW)
    (printf "~nStep 2 (post Skolemization):~n")
    (for ([constraint step2])
      (printf "  ~a~n" constraint)))
    
  ; Create a new fake run with the new bounds
  (define new-fake-run (Run 'fake #'fake (Run-run-spec fake-run) fake-solution-tree 
                            fake-server-ports (Run-atoms fake-run) fake-kodkod-current step2-bounds (box #f)))

  ; 7/25: Commented out quantifier grounding for now.
  ; (define step3 (map (lambda (f) (quant-grounding:interpret-formula new-fake-run f relations all-atoms '() '() step2-bounds)) step2))
  
  (define step4 (map (lambda (f) (smt-tor:convert-formula new-fake-run f relations all-atoms '() '() step2-bounds)) step2))
  (when (@> (get-verbosity) VERBOSITY_LOW)
    (printf "~nStep 3 (post SMT-LIB conversion):~n")
    (for ([constraint step4])
      (printf "  ~a~n" constraint))
    (printf "~nBounds (post Skolemization):~n")
    (for ([bound step2-bounds])
      (printf "  ~a/lower: ~a~n" (bound-relation bound) (bound-lower bound))
      (printf "  ~a/upper: ~a~n" (bound-relation bound) (bound-upper bound))))

  ; Now, convert bounds into SMT-LIB (theory of relations) and assert all formulas

  ; Preamble: the (reset) makes (set-logic ...) etc. not give an error on 2nd run
  ; Note some other options are set in cvc5-server.rkt! We explicitly reset here just in case.
  ; Also declare Atom sort as the top level sort, and define various helper SMT functions.
  
  (define defined-funs (list
     "(define-fun sign ((x__sign Int)) Int (ite (< x__sign 0) -1 (ite (> x__sign 0) 1 0)))"
     "(define-fun reconcile-int ((aset (Relation Int))) Int (ite (= (as set.empty (Relation Int)) aset) 0 ((_ tuple.select 0) (set.choose aset))))"))
  (define preamble-str (format "(reset)~n~a~n(set-logic ALL)~n(declare-sort Atom 0)~n"
                               (string-join defined-funs "\n")))

  ; converted bounds:
  (define bounds-str (string-join (map convert-bound step2-bounds) "\n"))
  (define bounds-str-2 (string-join (map (lambda (b) (child-constraint new-fake-run b)) step2-bounds) "\n"))
  ; bound disjointness
  (define top-level-disjoint-str (form-disjoint-string (map bound-relation step2-bounds)))
  (define disjointness-constraint-str (disjoint-relations top-level-disjoint-str))

  ; converted formula:
  (define assertions-str (string-join (map (lambda (s) (format "(assert ~a)" s)) step4) "\n"))

  ; DO NOT (check-sat) yet.
  
  (format "~a~n~a~n~a~n~a~n" preamble-str bounds-str disjointness-constraint-str assertions-str))

; No core support yet, see pardinus for possible approaches
(define (get-next-cvc5-tor-model is-running? run-name all-rels all-atoms core-map stdin stdout stderr [mode ""]
                                 #:run-command run-command)
  (printf "Getting next model from CVC5...~n")

  ; If the solver isn't running at all, error:
  (unless (is-running?)
    ; Note: we are not currently emptying stderr in the normal flow of operation.
    (printf "Unexpected error: worker process was not running. Printing process output:~n")
    (printf "  STDOUT: ~a~n" (port->string stdout #:close? #f)) 
    (printf "  STDERR: ~a~n" (port->string stderr #:close? #f))
    (raise-forge-error #:msg "CVC5 server is not running. Could not get next instance."
                       #:context run-command))

  ; If the solver is running, but this specific run ID is closed, user error
  (when (is-run-closed? run-name)
    (raise-forge-error #:msg (format "Run ~a has been closed." run-name)
                       #:context run-command))
    
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
       (when (@> (get-verbosity) 0)
         (printf "----- RECEIVED -----~n")
         (for ([s-expr model-s-expression])
           (printf "~a~n" s-expr))
         (printf "--------------------~n"))
         
       ; Forge still uses the Alloy 6 modality overall: Sat structs contain a _list_ of instances,
       ; each corresponding to the state of the instance at a given time index. Here, just 1.
       (define response (Sat (list (smtlib-tor-to-instance model-s-expression run-command)) #f '()))
       (when (@> (get-verbosity) VERBOSITY_LOW)
         (printf "~nSAT: ~a~n" response))
       response)]
    ['unsat
     (when (@> (get-verbosity) VERBOSITY_LOW)
       (printf "~nUNSAT~n"))
     ; No cores or statistics yet
     (Unsat #f #f 'unsat)]
    ['unknown
     (when (@> (get-verbosity) VERBOSITY_LOW)
       (printf "~nUNKNOWN~n"))
     ; No statistics yet
     (Unknown #f #f)]
    [else
     (printf "Unexpected error: bad response from CVC5. Printing process output:~n")
     (printf "  STDOUT: ~a~n" (port->string stdout #:close? #f)) 
     (printf "  STDERR: ~a~n" (port->string stderr #:close? #f)) 
     (raise-forge-error #:msg (format "Received unexpected response from CVC5: ~a" sat-answer)
                        #:context run-command)]))
  result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Converting SMT-LIB response expression into a Forge instance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Building this up from running examples, so the cases may be a bit over-specific.
; Accept the `run-command` for better syntax-location information if the run fails.
(define (smtlib-tor-to-instance model-s-expression run-command)
  (for/fold ([inst (hash)])
            ([part model-s-expression])
    (define-values (key value)
      (match part

        ; Relational value: empty set
        [(list (quote define-fun) ID (list) TYPE (list (quote as) (quote set.empty) ATOMTYPE))
         (values ID '())]
        
        ; Constant bindings to atoms: return unary relation of one tuple
        [(list (quote define-fun) ID (list) TYPE (list (quote as) ATOMID ATOMTYPE))
         (values ID (list (list (process-atom-id ATOMID run-command))))]

        ; Uninterpreted function w/ constant value
        [(list (quote define-fun) ID (list ARGS-WITH-TYPES) TYPE (list (quote as) ATOMID ATOMTYPE))
         (values ID (list (list (process-atom-id ATOMID run-command))))]
        ; Uninterpreted function w/ constant Int value, no parameters; e.g., an Int-valued Skolem function
        [(list (quote define-fun) ID (list) (quote Int) ATOMID)
         (values ID (list (list (process-atom-id ATOMID run-command))))]

        ; Uninterpreted function with if-then-else value (likely a Skolem function)
        ; TODO: sending this back empty to avoid blocking development; need to evaluate it properly
        [(list (quote define-fun) ID (list ARGS-WITH-TYPES ...) CODOMAIN (list (quote ite) COND T F))
         (values ID '())]
        
        ; Relational value: union (may contain any number of singletons)
        [(list (quote define-fun) ID (list) TYPE (list (quote set.union) ARGS ...))
         ; A union may contain an inductive list built from singletons and unions,
         ;   or multiple singletons within a single list. So pre-process unions
         ;   and flatten to get a list of singletons.
         (define singletons (apply append (map (lambda (a) (process-union-arg a run-command)) ARGS)))
         (values ID (process-singleton-list singletons run-command))]
        
        ; Relational value: singleton (should contain a single tuple)
        [(list (quote define-fun) ID (list) TYPE (list (quote set.singleton) ARG))
         ; Wrap this tuple, because this is a singleton _set_
         (values ID (list (process-tuple ARG run-command)))]
        
        ; Catch-all; unknown format
        [else
         (raise-forge-error #:msg (format "Unsupported response s-expression from SMT-LIB: ~a" part)
                            #:context run-command)]))
    (hash-set inst key value)))

; Return a list containing singletons
(define (process-union-arg arg run-command)
  (match arg
    [(list (quote set.singleton) TUPLE)
     (list arg)]
    [(list (quote set.union) ARGS ...)
     (apply append (map (lambda (a) (process-union-arg a run-command)) ARGS))]
    [else
     (raise-forge-error #:msg (format "Unsupported response s-expression from SMT-LIB (process-union-arg): ~a" arg)
                        #:context run-command)]))
(define (process-singleton-list args run-command)
  (match args
    ; a list of singletons (unions should be removed prior)
    [(list (list (quote set.singleton) TUPLES) ...)
     (map (lambda (t) (process-tuple t run-command)) TUPLES)]
    [else
     (raise-forge-error #:msg (format "Unsupported response s-expression from SMT-LIB (process-singleton-list): ~a" args)
                        #:context run-command)]))

(define (process-tuple tup run-command)
  (match tup
    ; Each atom may be an SMT-LIB "qualified identifier" or may be a raw value (e.g., an integer)
    [(list (quote tuple) ATOMS ...)
     (map (lambda (a) (process-atom a run-command)) ATOMS)]))
(define (process-atom atom-expr run-command)
 (match atom-expr
   [(list (quote as) ATOMID ATOMTYPE)
     (process-atom-id ATOMID run-command)]
   [ATOMID
     (process-atom-id ATOMID run-command)]))

(define (process-atom-id atom-id run-command)
  (define string-atom-id (format "~a" atom-id))
  (string->symbol
   (string-replace (string-replace string-atom-id "@" "") "_" "$")))

;(smtlib-tor-to-instance
;'((define-fun spouse () (Set (Tuple Person Person)) (set.union (set.singleton (tuple (as @Person_0 Person) (as @Person_3 Person))) (set.union (set.singleton (tuple (as @Person_2 Person) (as @Person_1 Person))) (set.union (set.singleton (tuple (as @Person_3 Person) (as @Person_0 Person))) (set.singleton (tuple (as @Person_1 Person) (as @Person_2 Person)))))))
;))