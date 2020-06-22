#lang racket

(require (prefix-in @ racket) 
         "lang/ast.rkt")
(require "server/forgeserver.rkt" ; v long
         "kodkod-cli/server/kks.rkt" 
         "kodkod-cli/server/server.rkt"
         "kodkod-cli/server/server-common.rkt"
         "translate-to-kodkod-cli.rkt"
         "translate-from-kodkod-cli.rkt")
(require "shared.rkt")

(provide sig relation fun const pred run test display-run)
(provide (struct-out Sig)
         (struct-out Relation)
         (struct-out Range)
         (struct-out Bound)
         (struct-out Inst)
         (struct-out Options)
         (struct-out State)
         (struct-out Run-info)
         (struct-out Run))
(provide Int iden univ none)
(provide no some one lone all) ; two)
(provide + - ^ & ~ join !)
(provide set in )
(provide = -> * => not and or)
(provide < > int=)
(provide add subtract multiply divide sign abs remainder)
(provide card sum sing succ max min)
(provide true false)
(provide set-verbosity VERBOSITY_LOW VERBOSITY_HIGH)

(require (prefix-in @ racket/set))
(require (for-syntax syntax/parse))
(require racket/struct)

; Define data structures

(struct Sig (
  name       ; String
  rel        ; node/expr/relation
  one        ; Boolean
  abstract   ; Boolean
  extends    ; String | #f
  extenders  ; List<String>
  ) #:transparent)

(struct Relation (
  name  ; String
  rel   ; node/expr/relation
  sigs  ; List<String>
  ) #:transparent)

(struct Range (
  lower ; int
  upper ; int
  ) #:transparent)

(struct Bound (
  default-bound  ; Range | #f
  bitwidth       ; int | #f
  sig-bounds     ; Map<String, Range>
  ) #:transparent)

(struct Inst (
  
  ) #:transparent)

(struct Options (
  solver          ; symbol
  verbosity       ; int
  sb              ; int
  coregranularity ; int
  logtranslation  ; int
  ) #:transparent)

(struct State (
  sigs        ; Map<String, Sig>
  relations   ; Map<String, Relation>
  predicates  ; Set<String>
  functions   ; Set<String>
  constants   ; Set<String>
  bounds      ; Map<String, Bound>
  insts       ; Map<String, Inst>
  options     ; Options
  ) #:transparent)

(struct Run-info (
  state   ; State
  preds   ; Set<String>
  bounds  ; Bound
  ) #:transparent)

(struct Run (
  name     ; String
  command  ; String (syntax)
  run-info ; Run-info
  result   ; Stream
  ) #:transparent
  #:methods gen:custom-write
  [(define (write-proc run port mode)
     (if (@not mode)
         (when (string=? (format "~a" port) "#<output-port:redirect>") (display-run run))
         ((make-constructor-style-printer
            (lambda (obj) 'Run)
            (lambda (obj) (list (Run-name obj) (Run-command obj) (Run-run-info obj) (Run-result obj))))
          run port mode)))])

; Define initial state
(define init-sigs (hash))
(define init-relations (hash))
(define init-predicates (@set))
(define init-functions (@set))
(define init-constants (@set))
(define init-bounds (hash))
(define init-insts (hash))
(define init-options (Options 'SAT4J 5 5 0 0))
(define init-state (State init-sigs init-relations 
                          init-predicates init-functions init-constants 
                          init-bounds init-insts
                          init-options))


; Defaults
(define DEFAULT-BITWIDTH 4)
(define DEFAULT-SIG-BOUND (Range 0 4))

;; Functions for state accessing

; get-state :: (|| Run-info State) -> State
; If run-or-state is a State, returns it;
; if it is a Run-info, then returns its state.
(define (get-state run-or-state)
  (if (Run-info? run-or-state)
      (Run-info-state run-or-state)
      run-or-state))

; get-sig :: (|| Run-info State), String -> Sig
; Returns the Sig of a given name from state.
(define (get-sig run-or-state sig-name)
  (hash-ref (State-sigs (get-state run-or-state)) sig-name))

; get-sigs :: (|| Run-info State), Relation? -> List<Sig>
; If a relation is provided, returns the column sigs;
; otherwise, returns the Sigs in a run/state.
(define (get-sigs run-or-state [relation #f])
  (if relation
      (map (curry get-sig (get-state run-or-state)) (Relation-sigs relation))
      (hash-values (State-sigs (get-state run-or-state)))))

; get-top-level-sigs :: (|| Run-info State) -> List<Sig>
; Returns the Sigs in a run/state that do not extend another Sig.
(define (get-top-level-sigs run-or-state)
  (filter (compose @not Sig-extends) (get-sigs run-or-state)))

; get-relations :: (|| Run-info State) -> List<Relation>
; Returns the Relations in a run/state.
(define (get-relations run-or-state)
  (hash-values (State-relations (get-state run-or-state))))

; get-children :: (|| Run-info State), Sig -> List<Sig>
; Returns the children Sigs of a Siig.
(define (get-children run-or-state sig)
  (define sig-map (State-sigs (get-state run-or-state)))
  (map (curry hash-ref sig-map) (Sig-extenders sig)))

; get-bound :: Run-info, Sig -> Range
; Returns the run bound of a Sig, in order:
; - if it is a one sig, returns (Range 1 1)
; - if an explicit bound is given, returns it;
; - if a default bound is given; returns it;
; - return DEFAULT-SIG-BOUND
(define (get-bound run sig)
  (if (Sig-one sig)
      (Range 1 1)
      (let* ([bounds (Run-info-bounds run)]
             [bounds-map (Bound-sig-bounds bounds)]
             [sig-name (Sig-name sig)]
             [default-bounds (or (Bound-default-bound bounds) DEFAULT-SIG-BOUND)])
        (hash-ref bounds-map sig-name default-bounds))))

; get-bitwidth :: Run-info -> int
; Returns the bitwidth for a run, returning the
; DEFAULT-BITWIDTH if none is provided.
(define (get-bitwidth run)
  (or (Bound-bitwidth (Run-info-bounds run)))
      DEFAULT-BITWIDTH)


;; Functions for state updating

; sig-add-extender :: Sig, String -> Sig
; Adds a new extender to the given Sig.
(define (sig-add-extender sig extender)
  (match sig [(Sig name rel one abstract extends old-extenders)
    (Sig name rel one abstract extends (append old-extenders (list extender)))]))

; state-add-sig :: State, String, bool, bool, (String | #f) -> State
; Adds a new Sig to the given State; if new Sig extends some
; other Sig, then updates that Sig with extension.
(define (state-add-sig state name rel one abstract extends)
  (match state [(State sigs relations predicates functions constants bounds insts options)
    (define new-sig (Sig name rel one abstract extends '()))
    ; assert extends in State-sigs

    ; update State-sigs
    (define sigs-with-new-sig (hash-set sigs name new-sig))
    (define new-state-sigs
      (if extends
          (hash-set sigs-with-new-sig extends 
                                      (sig-add-extender (hash-ref sigs extends) name))
          sigs-with-new-sig))

    (State new-state-sigs relations predicates functions constants bounds insts options)]))

; state-add-relation :: State, String, List<Sig> -> State
; Adds a new relation to the given State.
(define (state-add-relation state name rel rel-sigs)
  (match state [(State sigs relations predicates functions constants bounds insts options)
    (define new-relation (Relation name rel rel-sigs))
    (define new-state-relations (hash-set relations name new-relation))
    (State sigs new-state-relations predicates functions constants bounds insts options)]))

; state-add-predicate :: State, String -> State
; Adds a new predicate to the given State.
(define (state-add-predicate state name)
  (match state [(State sigs relations predicates functions constants bounds insts options)
    (define new-state-predicates (set-add predicates name))
    (State sigs relations new-state-predicates functions constants bounds insts options)]))

; state-add-function :: State, String -> State
; Adds a new function to the given State.
(define (state-add-function state name)
  (match state [(State sigs relations predicates functions constants bounds insts options)
    (define new-state-functions (set-add functions name))
    (State sigs relations predicates new-state-functions constants bounds insts options)]))

; state-add-constant :: State, String -> State
; Adds a new constant to the given State.
(define (state-add-constant state name)
  (match state [(State sigs relations predicates functions constants bounds insts options)
    (define new-state-constants (set-add constants name))
    (State sigs relations predicates functions new-state-constants bounds insts options)]))


;; AST macros
(require syntax/parse/define)
(define-simple-macro (iff a b) (and (=> a b) (=> b a)))
(define-simple-macro (ifte a b c) (and (=> a b) (=> (not a) c)))
(define-simple-macro (>= a b) (or (> a b) (int= a b)))
(define-simple-macro (<= a b) (or (< a b) (int= a b)))
(define-simple-macro (ni a b) (in b a))

;; Forge-core command macros

; The environment threaded through commands
(define curr-state init-state)
(define (update-state! new-state) 
  (set! curr-state new-state))


; Declare a new sig.
; (sig name [|| [#:one] [#:abstract]] [#:extends parent])
(define-syntax (sig stx)
  (syntax-parse stx
    [(sig name:id (~alt (~optional (~seq #:extends parent:expr))
                        (~optional (~or (~seq (~and #:one one-kw))
                                        (~seq (~and #:abstract abstract-kw))))) ...)
    #'(begin
        (define true-name (symbol->string 'name))
        (define true-one (~? (~@ (or #t 'one-kw)) (~@ #f)))
        (define true-abstract (~? (~@ (or #t 'abstract-kw)) (~@ #f)))
        (define true-parent (~? (~@ (symbol->string 'parent)) (~@ #f)))
        (define name (declare-relation (list true-name) 
                                       (or true-parent "univ")
                                       true-name))
        (update-state! (state-add-sig curr-state true-name name true-one true-abstract true-parent)))]))

; Declare a new relation
; (relation name (sig sig sig ...))
(define-syntax (relation stx)
  (syntax-parse stx
    [(relation name:id (sig1:id sig2:id sigs ...))
     #'(begin
       (define true-name (symbol->string 'name))
       (define true-sigs (map symbol->string (list 'sig1 'sig2 'sigs ...)))
       (define name (declare-relation true-sigs (symbol->string 'sig1) true-name))
       (update-state! (state-add-relation curr-state true-name name true-sigs)))]))

; Declare a new predicate
; (pred name cond ...)
; (pred (name var ...) cond ...)
(define-syntax (pred stx)
  (syntax-parse stx
    [(pred name:id conds:expr ...+) 
      #'(begin 
        (define name (&& conds ...))
        (update-state! (state-add-predicate curr-state (symbol->string 'name))))]
    [(pred (name:id args:id ...+) conds:expr ...+) 
      #'(begin 
        (define (name args ...) (&& conds ...))
        (update-state! (state-add-predicate curr-state (symbol->string 'name))))]))

; Declare a new function
; (fun (name var ...) result)
(define-syntax (fun stx)
  (syntax-parse stx
    [(fun (name:id args:id ...+) result:expr) 
      #'(begin 
        (define (name args ...) result)
        (update-state! (state-add-function curr-state (symbol->string 'name))))]))

; Declare a new constant
; (const name value)
(define-syntax (const stx)
  (syntax-parse stx
    [(const name:id value:expr) 
      #'(begin 
        (define name value)
        (update-state! (state-add-constant curr-state (symbol->string 'name))))]))

; Run a given spec
; (run [(pred ...)] [((sig [lower 0] upper) ...)])
(define-syntax (run stx)
  (define command (format "~a" stx))

  (syntax-parse stx
    [(run name:id
          (~optional (pred:id ...))
          (~optional ((sig:id (~optional lower:nat #:defaults ([lower #'0])) upper:nat) ...)))
      #`(begin
        (define run-name (~? (~@ (symbol->string 'name)) (~@ "no-name-provided")))
        (define run-state curr-state)
        (define run-preds (~? (~@ (list pred ...)) (~@ (list)))) 

        (define sig-bounds (~? 
          (~@
            (for/hash ([name (list 'sig ...)]
                       [lo (list lower ...)]
                       [hi (list upper ...)])
              (values (symbol->string name) (Range lo hi))))
          (~@ (hash))))
        (define bitwidth (if (hash-has-key? sig-bounds "int")
                             (begin0 (hash-ref sig-bounds "int")
                                     (hash-remove! sig-bounds "int"))
                             DEFAULT-BITWIDTH))
        (define default-bound DEFAULT-SIG-BOUND)
        (define run-bounds (Bound default-bound bitwidth sig-bounds))

        (define run-command #,command)

        (define run-info (Run-info run-state run-preds run-bounds))
        (define run-result (run-spec run-info))

        (define name (Run run-name run-command run-info run-result)))]))


(define-syntax (display-run stx)
  (syntax-parse stx
    [(display-run run:id)
      #'(let ()
        (define model-stream (Run-result run))
        (define (get-next-model)
          (define ret (stream-first model-stream))
          (set! model-stream (stream-rest model-stream))
          ret)
        (display-model get-next-model (Run-name run) (Run-command run) "/no-name.rkt" (get-bitwidth (Run-run-info run)) empty))]))

(define-syntax (test stx)
  (syntax-parse stx
    [(test name:id 
           (pred:id ...)
           (~optional ((sig:id (~optional lower:nat #:defaults ([lower #'0])) upper:nat) ...))
           (~and (~or 'sat 'unsat) sat-or-unsat))
     #'(begin
       (run temp-run (pred ...) (~? (~@ ([sig lower upper] ...)) (~@)))
       (define first-instance (stream-first (Run-result temp-run)))
       (display (equal? (car first-instance) sat-or-unsat)))]))

; run-spec :: Run-info -> void
; Given a Run-info structure, processes the data and communicates it to KodKod-CLI;
; then takes result from KodKod-CLI and connects to Sterling server.
(define (run-spec run-info)
  ; Get KodKod names, min sets, and max sets of Sigs and Relations
  (define-values (sig-to-name ; Map<String, int>
                  sig-to-min  ; Map<String, List<int>>
                  sig-to-max  ; Map<String, List<int>>
                  num-atoms)  ; int
                 (get-sig-info run-info))

  (define-values (rel-to-name ; Map<String, int>
                  rel-to-min  ; Map<String, nones??>
                  rel-to-max) ; Map<String, List<List<int>>>
                 (get-relation-info run-info sig-to-max (+ (hash-count sig-to-name) 2)))

  ;; Print to KodKod-CLI
  ; print configure
  ; declare univ size

  ; declare ints
  ; print Int sig (r0)
  ; print succ relation (r1)

  ; print sigs (r2 ... rm)
  ; print relations (r(m + 1) ... rn)

  ; print formula / assert formula (f0 ... fk)

  ; print solve

  ; Initializing our kodkod-cli process, and getting ports for communication with it
  (define kks (new server%
                   [initializer (thunk (kodkod-initializer #f))]
                   [stderr-handler (curry kodkod-stderr-handler "blank")]))
  (send kks initialize)
  (define stdin (send kks stdin))
  (define stdout (send kks stdout))

  (define-syntax-rule (kk-print lines ...)
    (cmd 
      [stdin]
      lines ...))

  ; Print configure and declare univ size
  (define bitwidth (get-bitwidth run-info))
  (kk-print
    (configure (format ":bitwidth ~a :solver ~a :max-solutions 1 :verbosity 7 :sb ~a :core-gran ~a :log-trans ~a"
                       bitwidth "SAT4J" 20 0 1))
    (declare-univ num-atoms))

  ; Declare ints
  (define num-ints (expt 2 bitwidth))
  (kk-print
    (declare-ints (range (- (/ num-ints 2)) (/ num-ints 2)) ; ints
                  (range num-ints)))                        ; indexes

  ; Print Int sig and succ relation
  (define int-rel (tupleset #:tuples (map list (range num-ints))))
  (define succ-rel (tupleset #:tuples (map list (range (sub1 num-ints))
                                                (range 1 num-ints))))
  (kk-print
    (declare-rel (r 0) int-rel int-rel)
    (declare-rel (r 1) succ-rel succ-rel))

  ; Declare sigs
  ; Sort the sigs by KodKod name to print in order
  (define sigs (map car (sort (hash->list sig-to-name) @< #:key cdr)))
  (for ([sigstr sigs])
    (define name (hash-ref sig-to-name sigstr))
    (define lo 
      (if (cons? (hash-ref sig-to-min sigstr))
          (tupleset #:tuples (map list (hash-ref sig-to-min sigstr)))
          "none"))
    (define hi (tupleset #:tuples (map list (hash-ref sig-to-max sigstr))))
    (kk-print (declare-rel (r name) lo hi)))

  ; Declare relations
  ; Sort the relations by KodKod name to print in order
  (define relations (map car (sort (hash->list rel-to-name) @< #:key cdr)))
  (for ([relstr relations])
    (define name (hash-ref rel-to-name relstr))
    (define lo (hash-ref rel-to-min relstr))
    (define hi (tupleset #:tuples (hash-ref rel-to-max relstr)))
    (kk-print (declare-rel (r name) lo hi)))
  

  ; Declare assertions

  ; Get a list of node/expr/relations for all Sigs and Relations so
  ; translate-to-kodkod-cli knows what names to use.
  ; The extra sorting process is to align list index position with
  ; KodKod CLI relation names.
  (define all-sigs
    (map Sig-rel (sort (get-sigs run-info)
                       @<
                       #:key (compose (curry hash-ref sig-to-name )
                                      Sig-name))))
  (define all-relations
    (map Relation-rel (sort (get-relations run-info)
                            @<
                            #:key (compose (curry hash-ref rel-to-name )
                                           Relation-name))))
  (define all-rels (append (list Int succ) all-sigs all-relations))

  ; Get and print predicates
  (define run-constraints 
    (append (Run-info-preds run-info)
            (get-sig-size-preds run-info)
            (get-relation-preds run-info)
            (get-extender-preds run-info)))
            ;(get-break-preds run-info)))

  (for ([p run-constraints]
        [assertion-number (in-naturals)])
    (kk-print
      (print-cmd-cont "(f~a " assertion-number)
      (translate-to-kodkod-cli p all-rels '())
      (print-cmd ")")
      (assert (f assertion-number))))

  ; Create list of atom names for translate-from-kodkod-cli
  (define inty-univ
    (for/fold ([inty-univ (reverse (range (- (/ num-ints 2)) (/ num-ints 2)))]
               [highest-name (hash)]; Map<String, int>
               #:result (reverse inty-univ)) 
              ([i (in-range num-ints num-atoms)])
      (define curr-sig (findf (compose (curry member i )
                                       (curry hash-ref sig-to-max )
                                       Sig-name) 
                              (get-sigs run-info)))
      (define root-sig
        (let get-root ([sig curr-sig])
          (if (@and (Sig-extends sig) (@not (Sig-one sig)))
              (get-root (get-sig run-info (Sig-extends sig)))
              sig)))

      (define sig-num (add1 (hash-ref highest-name (Sig-name root-sig) -1)))
      (define new-highest-name (hash-set highest-name (Sig-name root-sig) sig-num))

      (define name (string->symbol (format "~a~a" (Sig-name root-sig) sig-num)))
      ; Raise an error if false
      (values (cons name inty-univ) new-highest-name)))

  ; Print solve
  (define (get-next-model)
    (kk-print (solve))
    (match-define (cons restype inst) (translate-from-kodkod-cli 'run (read-solution stdout) all-rels inty-univ))
    (cons restype inst))

  (define (model-stream)
    (stream-cons (get-next-model) (model-stream)))

  (model-stream))


; get-sig-info :: Run-info -> Map<String, int>, 
;                        Map<String, List<int>>, 
;                        Map<String, List<int>>, 
;                        int
; Given a Run-info, assigns names to each sig, assigns minimum and maximum 
; sets of atoms for each, and find the total number of atoms needed (including ints).
(define (get-sig-info run-info)
  ;; Get true bounds (Map<String, Range>)
  ; This takes into account the demanded atoms from (recursive) child sigs
  ; and also contains the default bounds for unspecified sigs.
  (define true-bounds (make-hash))

  ; get-demand :: Sig -> int
  ; Updates lower and upper bounds and
  ; returns the (minimum) number of atoms that a particular sig demands from its parent
  (define (get-demand sig)
    ; Recur and get childrens demands
    (define children (get-children run-info sig))
    (define demands (apply + (map get-demand children)))

    ; Generate true bounds
    (define bounds (get-bound run-info sig))
    ;(assert (<= demands (Range-upper bounds)))
    (define true-demands (@max demands (Range-lower bounds)))
    (hash-set! true-bounds (Sig-name sig) (Range true-demands (Range-upper bounds)))

    ; Return own demands
    true-demands)

  (for-each get-demand (get-top-level-sigs run-info))
  ; ideally should freeze true-bounds here


  ;; To return
  (define sig-to-name (make-hash)) ; Map<String, int>
  (define sig-to-min (make-hash))  ; Map<String, List<int>>
  (define sig-to-max (make-hash))  ; Map<String, List<int>>

  ; Name updaters
  (define curr-relation 2) ; Start at 2 to account for Int and succ relations
  (define curr-max-atom (expt 2 (get-bitwidth run-info))) ; Keep track of what to name next atoms

  ; generate :: Sig, List<int>, List<int> -> void
  ; Takes in a Sig, and two integer lists and
  ; sets name, min list, and max list of atoms for sig.
  ; private-atoms will be the lower bound allocated atoms,
  ; and shared-atoms will be a pool of extra atoms which can
  ; be used to populate up to upper bound.
  (define (generate sig private-atoms shared-atoms)
    ; Set name of sig
    (hash-set! sig-to-name (Sig-name sig) curr-relation)
    (set! curr-relation (add1 curr-relation))

    ; Set min and max list of atoms
    (define min-atoms private-atoms)
    ; Optimized by not providing shared atoms if true lower bound = upper bound
    (define exact-bound
      (let ([bound (hash-ref true-bounds (Sig-name sig))])
        (@= (Range-lower bound) (Range-upper bound))))
    (define max-atoms 
      (if exact-bound
          private-atoms
          (append private-atoms shared-atoms)))
    (hash-set! sig-to-min (Sig-name sig) min-atoms)
    (hash-set! sig-to-max (Sig-name sig) max-atoms)

    ; Get children lower bounds
    (define children (get-children run-info sig))
    (define children-lower (map (compose (curry Range-lower )
                                         (curry hash-ref true-bounds )
                                         (curry Sig-name ))
                                children))

    ; Generate new private and shared atoms for children
    (define num-new-private (apply + children-lower))
    (define-values (new-private-atoms new-shared-atoms) (split-at max-atoms num-new-private))
    (define num-allocated 0) ; Keep track of what private atoms have been allocated

    ; allocate-atoms :: Sig, int -> void
    ; Recur into children atoms, allocating portion of private-atoms to each.
    (define (allocate-atoms child child-lower)
      (define child-private-atoms (drop (take new-private-atoms 
                                              (+ num-allocated child-lower))
                                        num-allocated))
      (generate child child-private-atoms new-shared-atoms)
      (set! num-allocated (+ num-allocated child-lower)))

    (for-each allocate-atoms children children-lower)
  )

  ; generate-top-level :: Sig -> void
  ; Assigns set of unused atoms to each top-level Sig based on
  ; upper bound, and recurs into children sigs with generate.
  (define (generate-top-level sig)
    (define sig-bound (hash-ref true-bounds (Sig-name sig)))
    (define lower (Range-lower sig-bound))
    (define upper (Range-upper sig-bound))

    (define private-atoms (range curr-max-atom (+ curr-max-atom lower)))
    (define shared-atoms (range (+ curr-max-atom lower) (+ curr-max-atom upper)))
    (set! curr-max-atom (+ curr-max-atom upper)) ; Update so next top-level Sig has new atoms

    (generate sig private-atoms shared-atoms))

  (for-each generate-top-level (get-top-level-sigs run-info))

  (values sig-to-name sig-to-min sig-to-max curr-max-atom)
)

; get-relation-info :: Run-info -> Map<String, int>, 
;                             Map<String, nones?>, 
;                             Map<String, List<List<int>>>
; Given a Run-info and the atoms assigned to each sig, assigns names to each relation
; and minimum and maximum sets of atoms for each relation.
(define (get-relation-info run-info sig-to-max start-relation-name)
  (define rel-to-name ; Map <String, int>
    (for/hash ([relation (get-relations run-info)]
               [name (in-naturals start-relation-name)])
    (values (Relation-name relation) name)))

  ; n-arity-none :: int -> nones?
  ; Creates an empty relation for lower bound for KodKod.
  (define (n-arity-none arity)
        (cond
          [(equal? arity 1) 'none]
          [(@> arity 0) (product 'none (n-arity-none (@- arity 1)))]
          [else (error "Error: Relation with negative or 0 arity specified.")]))
  (define rel-to-min ; Map<String, nones?>
    (for/hash ([relation (get-relations run-info)])
      (define nones (n-arity-none (length (Relation-sigs relation))))
      (values (Relation-name relation) nones)))

  (define rel-to-max ; Map<String, List<List<int>>>
    (for/hash ([relation (get-relations run-info)])
      (define atoms ; List<List<int>>
        (map (curry hash-ref sig-to-max ) (Relation-sigs relation)))
      (define tuples (apply cartesian-product atoms))
      (values (Relation-name relation) tuples)))

  (values rel-to-name rel-to-min rel-to-max))

; get-sig-size-preds :: Run-info -> List<node/formula>
; Creates assertions for each non-top-level Sig to restrict
; it to the correct upper bound.
(define (get-sig-size-preds run-info) 
  (define extender-sigs (filter Sig-extends (get-sigs run-info)))

  (for/list ([sig extender-sigs])
    (define upper-bound 
      (Range-upper (get-bound run-info sig)))
    (<= (card (Sig-rel sig)) (node/int/constant upper-bound))))

; get-extender-preds :: Run-info -> List<node/formula>
; Creates assertions for each Sig which has extending Sigs so that:
; - if it is abstract, then it must equal the sum of its extenders
; -                    else it must contain the sum of its extenders
; - all extenders are pair-wise disjoint.
(define (get-extender-preds run-info)
  (define sig-constraints (for/list ([sig (get-sigs run-info)])
    ; get children information
    (define children-rels (map Sig-rel (get-children run-info sig)))

    ; abstract and sig1, ... extend => (= sig (+ sig1 ...))
    ; not abstract and sig is parent of sig1 => (in sig1 sig)
    ; TODO: optimize by identifying abstract sigs as sum of children
    (define (abstract sig extenders)
      (= sig (apply + extenders)))
    (define (parent sig1 sig2)
      (in sig2 sig1))
    (define extends-constraints 
      (if (and (Sig-abstract sig) (cons? (Sig-extenders sig)))
          (list (abstract (Sig-rel sig) children-rels))
          (map (curry parent (Sig-rel sig)) children-rels)))

    ; sig1 and sig2 extend sig => (no (& sig1 sig2))
    (define (disjoin-pair sig1 sig2)
      (no (& sig1 sig2)))
    (define (disjoin-list a-sig a-list)
      (map (curry disjoin-pair a-sig) a-list))
    (define (disjoin a-list)
      (if (empty? a-list)
          empty
          (append (disjoin-list (first a-list) (rest a-list))
                  (disjoin (rest a-list)))))
    (define disjoint-constraints (disjoin children-rels))

    (append extends-constraints disjoint-constraints)))

  ; combine all constraints together
  (apply append sig-constraints))

; get-relation-preds :: Run-info -> List<node/formula>
; Creates assertions for each Relation to ensure that it does not
; contain any atoms which don't populate their Sig.
(define (get-relation-preds run-info)
  (for/list ([relation (get-relations run-info)])
    (define sig-rels (map Sig-rel (get-sigs run-info relation)))
    (in (Relation-rel relation) (apply -> sig-rels))))


