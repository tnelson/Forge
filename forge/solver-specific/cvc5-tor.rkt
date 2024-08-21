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
         (for-syntax racket/base))

(require (prefix-in @ (only-in racket/base >= not - = and or max > < +))
         (only-in racket match first rest empty empty? set->list list->set set-intersect set-union
                         curry range index-of pretty-print filter-map string-prefix? string-split thunk*
                         remove-duplicates subset? cartesian-product match-define cons? set-subtract
                         string-replace second string-join take last flatten)
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
  ; (smtlib-display stdin cvc5-command)
  (define matcher (lambda (exp) 
    (match exp 
      [(list (quote reconcile-int_atom) 
             (list (quote set.singleton) 
                   (list (quote tuple)
                         X))) X]
      [else #f]    
    )
  ))
  (define optimized-cvc5-command (descend-s-exp cvc5-command matcher))

  (for ([line optimized-cvc5-command])
    (if (not (empty? line)) 
      ; (begin (printf "~nSENDING TO CVC5:~n~a~n-------------------------~n" line)
        (smtlib-display stdin line)
      ; )
      (void)
    )
  )
  
  ; Done with the problem spec. Return any needed shared data specific to this backend.
  (values all-rels core-map))

(define (deparen lst)
  (string-join lst " "))

;; Helper function to declare constants for each upper bound element
(define (const-declarations bound)
         `(map (lambda (tup)
            (string->symbol (format "(declare-const ~a Atom)~n" (first tup))))
                       (bound-upper bound)))

(define (child-constraint run-or-state bound)
  (define name (relation-name (bound-relation bound)))
  (define arity (relation-arity (bound-relation bound)))
  (cond 
    [(equal? name "Int")
    '()]
    ; we only want this to operate on arity 1 non-skolem relations, aka sigs
    [(and (equal? arity 1) (not (equal? (string-ref name 0) #\$)))
      (define primsigs (if (Sig? (bound-relation bound)) (primify run-or-state (Sig-name (bound-relation bound))) '()))
      (define remove-remainder-lambda (lambda (sig-name) (string->symbol (regexp-replace #rx"_.*$" (symbol->string sig-name) ""))))
      (cond [(or (equal? 1 (length primsigs)) (empty? primsigs)) '()]
            [else
            ; (format "(assert (= ~a (set.union ~a)))~n~a~n"
            ;         (relation-name (bound-relation bound))
            ;         (deparen (map remove-remainder-lambda primsigs))
            ;         (child-disjointness run-or-state (bound-relation bound)))
            (let ([assertion `(assert (= ,(string->symbol (relation-name (bound-relation bound))) (set.union ,@(map remove-remainder-lambda primsigs))))])
                (cons assertion (child-disjointness run-or-state (bound-relation bound)))
            )])
    ]
    [(equal? (string-ref name 0) #\$)
    '()]
    [else '()]
  )
)

(define (child-disjointness run-or-state sig) 
  ; get the children of the sig that has at least 1 level of children
  (define children (get-children run-or-state sig))
  ; get the names of the children
  (define child-names (map relation-name children))
  ; pairwise disjoint constraint them
  (define (pairwise-disjoint relation1 relation2)
      (if (equal? relation1 relation2) '()
      `(assert (= (set.inter ,(string->symbol relation1) ,(string->symbol relation2)) (as set.empty (Relation Atom))))))
  (define pairs (cartesian-product child-names child-names))
  (filter-map (lambda (pair) 
                (let ([result (pairwise-disjoint (first pair) (second pair))])
                  (if (empty? result) #f result))) 
              pairs)
)

(define (declare-sigs b)
  (define name (relation-name (bound-relation b)))
  (define arity (relation-arity (bound-relation b)))
  (define typenames ((relation-typelist-thunk (bound-relation b))))
  (define parent (relation-parent (bound-relation b)))
  (define one? (if (Sig? (bound-relation b)) (Sig-one (bound-relation b)) #f))
  (define abstract? (if (Sig? (bound-relation b)) (Sig-abstract (bound-relation b)) #f))
  (cond
    ; Don't declare Int at all
    [(equal? name "Int")
     (list '())]
    ; Sigs: unary, and not a skolem name
    [(and (equal? arity 1) (not (equal? (string-ref name 0) #\$)))
     (if one? 
     (list `(declare-fun ,(string->symbol name) () (Relation Atom)) `(declare-const ,(string->symbol (format "~a_atom" name)) Atom) `(assert (= ,(string->symbol name) (set.singleton (tuple ,(string->symbol (format "~a_atom" name)))))))
     (list `(declare-fun ,(string->symbol name) () (Relation Atom))))]
    ; Skolem relation
    [(equal? (string-ref name 0) #\$)
     (cond [(equal? arity 1)
            (list `(declare-const ,name ,@(map atom-or-int typenames)))]
           [else
            (define domain-typenames (take typenames (@- (length typenames) 1)))
            (define codomain-typename (last typenames))
            (list `(declare-fun ,(string->symbol name) (,@(map atom-or-int domain-typenames)) ,(atom-or-int codomain-typename)))])]
    ; Fields
    [else
    ; Fields are declared as relations of the appropriate arity of atoms or ints
     (list `(declare-fun ,(string->symbol name) () (Relation ,@(map atom-or-int typenames))))]))
     
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
      (if (equal? relation1 relation2) '()
      `(assert (= (set.inter ,(string->symbol relation1) ,(string->symbol relation2)) (as set.empty (Relation Atom))))))
  (define pairs (cartesian-product rel-names rel-names))
  (map (lambda (pair) (pairwise-disjoint (first pair) (second pair))) pairs))

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
  ; (define step2-both
  ;   (for/fold ([fs-and-bs (list '() total-bounds)])
  ;             ([f step1])
  ;     ; Accumulator starts with: no formulas, original total-bounds
  ;     (define-values (resulting-formula new-bounds)
  ;       (skolemize:interpret-formula fake-run (second fs-and-bs) f relations all-atoms '() '()
  ;                                    #:tag-with-spacer #t))
  ;     ; Accumulator adds the skolemized formula, updated bounds
  ;     (list (cons resulting-formula (first fs-and-bs))
  ;           new-bounds)))
  
  ; (define step2 (first step2-both))
  ; (define step2-bounds (second step2-both))
  ; (when (@> (get-verbosity) VERBOSITY_LOW)
  ;   (printf "~nStep 2 (post Skolemization):~n")
  ;   (for ([constraint step2])
  ;     (printf "  ~a~n" constraint)))
    
  ; Create a new fake run with the new bounds
  (define new-fake-run (Run 'fake #'fake (Run-run-spec fake-run) fake-solution-tree 
                            fake-server-ports (Run-atoms fake-run) fake-kodkod-current total-bounds (box #f)))

  ; 7/25: Commented out quantifier grounding for now.
  ; (define step3 (map (lambda (f) (quant-grounding:interpret-formula new-fake-run f relations all-atoms '() '() step2-bounds)) step2))

  (printf "Step 1: ~a~n" step1)
  
  (define step4 (map (lambda (f) (smt-tor:convert-formula new-fake-run f relations all-atoms '() '() total-bounds)) step1))
  (when (@> (get-verbosity) VERBOSITY_LOW)
    (printf "~nStep 3 (post SMT-LIB conversion):~n")
    (for ([constraint step4])
      (printf "  ~a~n" constraint))
    (printf "~nBounds (post Skolemization):~n")
    ; (for ([bound step2-bounds])
    ;   (printf "  ~a/lower: ~a~n" (bound-relation bound) (bound-lower bound))
    ;   (printf "  ~a/upper: ~a~n" (bound-relation bound) (bound-upper bound)))
      )

  ; Now, convert bounds into SMT-LIB (theory of relations) and assert all formulas

  ; Preamble: the (reset) makes (set-logic ...) etc. not give an error on 2nd run
  ; Note some other options are set in cvc5-server.rkt! We explicitly reset here just in case.
  ; Also declare Atom sort as the top level sort, and define various helper SMT functions.
  
  (define defined-funs (list
     `(define-fun sign ((x__sign Int)) Int (ite (< x__sign 0) -1 (ite (> x__sign 0) 1 0)))
     `(define-fun reconcile-int_atom ((aset (Relation IntAtom))) IntAtom ((_ tuple.select 0) (set.choose aset)))
     `(assert (forall ((x1 IntAtom) (x2 IntAtom)) (=> (not (= x1 x2)) (not (= (IntAtom-to-Int x1) (IntAtom-to-Int x2))))))
     `(declare-fun univInt () (Relation IntAtom))
     ; IntAtom, because those are what appear in sets. Just Int could end up empty.
     `(assert (= univInt (as set.universe (Relation IntAtom))))))
  (define preamble-str (append (list `(reset) `(declare-sort Atom 0) `(declare-sort IntAtom 0) `(declare-fun IntAtom-to-Int (IntAtom) Int)) defined-funs))
  (define bounds-str (apply append (map (lambda (b) (declare-sigs b)) total-bounds)))
  (define bounds-constraint-strs (apply append (filter-map (lambda (b) 
                                                (let ([result (child-constraint new-fake-run b)])
                                                  (if (empty? result) #f result)
                                                )) total-bounds)))
  ; bound disjointness
  (define top-level-disjoint-str (form-disjoint-string (map bound-relation total-bounds)))
  (define disjointness-constraint-str (disjoint-relations top-level-disjoint-str))
  (define comprehension-strs (smt-tor:get-new-top-level-strings))
  ; converted formula:
  (define assertion-strs (map (lambda (s) `(assert ,s)) step4))

  ; DO NOT (check-sat) yet.
  ; list rather than format for s-exp
  (append preamble-str bounds-str bounds-constraint-strs disjointness-constraint-str comprehension-strs assertion-strs))

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
  ;(smtlib-display stdin "(exit)") ; FOR DEBUGGING
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
     (printf "Unexpected error: bad response from CVC5.~n")
     (cond
       [(not (is-running?))
        (printf "Worker process closed. Printing remaining data in output ports:~n")
        ; these will block, hence calling only if the process is closed.
        (printf "  STDOUT: ~a~n" (port->string stdout #:close? #f)) 
        (printf "  STDERR: ~a~n" (port->string stderr #:close? #f))]
       [else
        (printf "Worker process is still running, but received unexpected respose.~n")])
     (raise-forge-error #:msg (format "Received unexpected response from CVC5: ~a" sat-answer)
                        #:context run-command)]))
  result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Converting SMT-LIB response expression into a Forge instance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Rename or exclude these entries in the resulting instance
(define (maybe-rename-id relname)
  (cond [(equal? relname 'univInt) 'Int]
        [else relname]))
(define EXCLUDE_KEYS '(IntAtom-to-Int))

;;;;;;;;;;;;;;;;;;;;;;
; Helpers to handle the nested (ite cond truebranch falsebranch) pattern
; E.g., (ite (= (as @IntAtom_1 IntAtom) $x1) 1 0)
;;;;;;;;;;;;;;;;;;;;;;

(define (eval-cond COND #:env env)
  (match COND
    [(list (quote =) (list (quote as) aid atype) vid)
     ;(printf "     **** eval-cond: ~a ~a ~a~n" (hash-ref env vid) aid (equal? (hash-ref env vid) aid))
     (equal? (hash-ref env vid) (process-atom-id aid))]
    [else (raise-forge-error #:msg (format "unrecognized ite match: ~a" COND)
                             #:context #f)]))

(define (eval-ite s-expr lookup)
  ;(printf "eval-ite: ~a; ~a~n" s-expr lookup)
  (define (match-branch-helper VAL)
    (match VAL
      [(list (quote ite) C T F) (eval-ite VAL lookup)]
      [(list (quote -) v) (@- v)]
      [v v]))
  
  (match s-expr
    [(list (quote ite) COND T F)
     (if (eval-cond COND #:env lookup)
         (match-branch-helper T)
         (match-branch-helper F))]
    [else (raise-forge-error #:msg (format "bad ite: ~a" s-expr)
                             #:context #f)]))

(define-syntax (ite->lambda stx)
  (syntax-case stx ()
    [(_ AWT COND T F)
     ; We cannot use AWT's value in the macro, because it doesn't exist yet.
     ; Thus, accept an arbitrary list of arguments and order them after-the-fact.
     #'(lambda (args)
         (define lookup (for/hash ([v (map car AWT)]
                                   [arg args])
                          (values v arg)))
         ;(printf "lookup: ~a~n" lookup)
         (eval-ite (list 'ite COND T F) lookup))]))


; Building this up from running examples, so the cases may be a bit over-specific.
; Accept the `run-command` for better syntax-location information if the run fails.
(define (smtlib-tor-to-instance model-s-expression run-command)
  (define raw-result
    (for/fold ([inst (hash)])
              ([part model-s-expression])
      ;(printf "smtlib-tor-to-instance: s-expr: ~a~n" part)
      
      ; Results come as a series of s-expressions ("parts"), each of which defines the 
      ; value of a specific SMT solver variable. Usually, these define relation-valued 
      ; functions, but sometimes the functions are Int valued, as in IntAtom-to-Int. These
      ; may or may not have arguments, in which case the function may or may not be defined
      ; in if-then-else style. Thus, we have a number of cases to handle.  
                       
    (define-values (key value)
      (match part

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ; Relational uninterpreted functions
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        ; Relational value: empty set
        [(list (quote define-fun) ID (list VARS_WITH_TYPES ...) CODOMAIN (list (quote as) (quote set.empty) ATOMTYPE))
         (values (maybe-rename-id ID) '())]
        
        ; Relational value: singleton (should contain a single tuple)
        [(list (quote define-fun) ID (list VARS_WITH_TYPES ...) CODOMAIN (list (quote set.singleton) ARG))
         (values (maybe-rename-id ID) (list (process-tuple ARG)))]
                
        ; Relational value: union (may contain any number of singletons)
        ; This includes relation-valued helper functions, so we need to support arguments.
        [(list (quote define-fun) ID (list VARS_WITH_TYPES ...) CODOMAIN (list (quote set.union) ARGS ...))
         ; A union may contain an inductive list built from singletons and unions,
         ;   or multiple singletons within a single list. So pre-process unions
         ;   and flatten to get a list of singletons.
         (define singletons (apply append (map (lambda (a) (process-union-arg a run-command)) ARGS)))
         (values (maybe-rename-id ID) (process-singleton-list singletons run-command))]

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ; Uninterpreted functions that might be either relational or not
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ; Function with if-then-else definition. This may be relational or non-relational.
        [(list (quote define-fun) ID (list ARGS-WITH-TYPES ...) CODOMAIN (list (quote ite) COND T F))
         (values (maybe-rename-id ID) (ite->lambda ARGS-WITH-TYPES COND T F))]
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ; Non-relational uninterpreted functions
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ; Non-relational UFs should never be "primary variables" in the Pardinus
        ; sense, and don't feature directly in an instance. But we must handle them
        ; because (a) they are part of the result from the solver and (b) some of them
        ; are used for post-processing (e.g., IntAtom-to-Int).
        
        ; Uninterpreted function w/ constant, non-relational value
        [(list (quote define-fun) ID (list ARGS-WITH-TYPES ...) TYPE (list (quote as) ATOMID ATOMTYPE))
         (values (maybe-rename-id ID) (lambda (args) (process-atom-id ATOMID)))]
        
        ; Uninterpreted function with non-relational constant value
        ; NOTE: this must go last, because of the generic "VAL".
        [(list (quote define-fun) ID (list ARGS-WITH-TYPES ...) CODOMAIN VAL)
         (values (maybe-rename-id ID) (lambda (args) (process-atom-id VAL)))]
        
        ; Catch-all; unknown format
        [else
         (raise-forge-error #:msg (format "Unsupported response s-expression from SMT-LIB: ~a" part)
                            #:context run-command)]))
    ; We need IntAtom-to-Int long enough to substitute all references to IntAtoms
    ; in other relations.
    (hash-set inst key value)))

  (define keys (hash-keys raw-result))

  ; Substitute out IntAtoms via IntAtom-to-Int
  (define ia2i (hash-ref raw-result 'IntAtom-to-Int))

  (define (mapper inst key)
    (define initial-tuples (hash-ref inst key))
    (define renamed-tuples
      ; for each tuple in the initial instance
      (for/list ([tup initial-tuples])
        ;(printf "processing tuple: ~a~n" tup)
        (map (lambda (a)
               (cond [(string-prefix? (symbol->string a) "IntAtom")
                      (ia2i (list a))]
                     [else a])) tup)))
    ;(printf "renamed: ~a~n" renamed-tuples)
    (hash-set inst key renamed-tuples))
  
  (define substituted-result
    (for/fold ([inst raw-result])
              ([k keys])
      (define tups (hash-ref inst k))
      ; If this is a non-empty relational variable, substitute out IntAtoms
      (cond [(and (list? tups) (not (empty? tups)) (list? (first tups))) (mapper inst k)]
            [else inst])))

  ; Clean up the instance. hash-filter-keys would make this easy, but don't want a
  ; dependency on Racket 8.12+ yet.
  ;(hash-filter-keys result (lambda (k) (not (member k EXCLUDE_KEYS))))
  
  (for/fold ([inst substituted-result])
            ([k keys])
    ; Don't retain any excluded keys, entries mapping to lambdas, or translation-helper
    ; relations (which start with underscore). 
    (if (or (member k EXCLUDE_KEYS)
            (procedure? (hash-ref inst k))
            (string-prefix? (symbol->string k) "_"))
        (hash-remove inst k)
        inst)))

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
     (map (lambda (t) (process-tuple t)) TUPLES)]
    [else
     (raise-forge-error #:msg (format "Unsupported response s-expression from SMT-LIB (process-singleton-list): ~a" args)
                        #:context run-command)]))

(define (process-tuple tup)
  (match tup
    ; Each atom may be an SMT-LIB "qualified identifier" or may be a raw value (e.g., an integer)
    [(list (quote tuple) ATOMS ...)
     (map (lambda (a) (process-atom a)) ATOMS)]))
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