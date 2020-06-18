#lang racket

; (require "lang/ast.rkt" "lang/bounds.rkt" (prefix-in @ racket) "server/forgeserver.rkt"
;          "kodkod-cli/server/kks.rkt" "kodkod-cli/server/server.rkt"
;          "kodkod-cli/server/server-common.rkt" "translate-to-kodkod-cli.rkt" "translate-from-kodkod-cli.rkt" racket/stxparam br/datum
;          "breaks.rkt"
;          "demo/life.rkt")

(require (prefix-in @ racket) 
         "lang/ast.rkt" 
         "kodkod-cli/server/kks.rkt" 
         "translate-to-kodkod-cli.rkt")

; ; racket/string needed for replacing transpose operator (~) with escaped version in error messages
; (require (for-syntax racket/syntax)
;          (for-syntax racket/string))
; (require racket/trace)

; (provide break instance quote begin println filepath set-path! let void)

; ; For verbosity, etc. that must be shared without cyclic dependency
; (require "shared.rkt")

; (provide declare-sig set-top-level-bound sigs pred)
; (provide run check test fact)
; (provide Int iden univ none)
; (provide no some one lone all two)
; (provide + - ^ & ~ join !)
; (provide set in )
; (provide = -> * => not and or)
; (provide set-bitwidth)
; (provide < > int=)
; (provide add subtract multiply divide sign abs remainder)
; (provide card sum sing succ max min)
; (provide add-relation set-option)

(require (prefix-in @ racket/set))
(require (for-syntax syntax/parse))

(struct Sig (
  name       ; String
  rel
  one        ; Boolean
  abstract   ; Boolean
  extends    ; String | #f
  extenders  ; List<String>
  ) #:transparent)

(struct Relation (
  name  ; String
  rel
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

(struct Run (
  name ; String
  state ; State
  preds ; Set<String>
  bounds ; Bound
  ) #:transparent)

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

; Some macros for ast
(require syntax/parse/define)
(define-simple-macro (iff a b) (and (=> a b) (=> b a)))
(define-simple-macro (ifte a b c) (and (=> a b) (=> (not a) c)))
(define-simple-macro (>= a b) (or (> a b) (int= a b)))
(define-simple-macro (<= a b) (or (< a b) (int= a b)))
(define-simple-macro (ni a b) (in b a))


; sig-add-extender :: Sig, String -> Sig
; Adds a new extender to the given Sig.
(define (sig-add-extender sig extender)
  (match sig [(Sig name rel one abstract extends old-extenders)
    (Sig name rel one abstract extends (append old-extenders (list extender)))]))

; state-add-sig :: State, String, bool, bool, (String | #f) -> State
; Adds a new sig to the given State; if new sig extends some
; other sig, then updates that sig with extension.
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



; (define-for-syntax (state-add-bounds state ))


(define curr-state init-state)
(define (update-state! new-state) 
  (set! curr-state new-state))

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

(define-syntax (relation stx)
  (syntax-parse stx
    [(relation name:id (sig1:id sig2:id sigs ...))
     #'(begin
       (define true-name (symbol->string 'name))
       (define true-sigs (map symbol->string (list 'sig1 'sig2 'sigs ...)))
       (define name (declare-relation true-sigs (symbol->string 'sig1) true-name))
       (update-state! (state-add-relation curr-state true-name name true-sigs)))]))

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

(define-syntax (fun stx)
  (syntax-parse stx
    [(fun (name:id args:id ...+) result:expr) 
      #'(begin 
        (define (name args ...) result)
        (update-state! (state-add-function curr-state (symbol->string 'name))))]))

(define-syntax (const stx)
  (syntax-parse stx
    [(const name:id value:expr) 
      #'(begin 
        (define name value)
        (update-state! (state-add-constant curr-state (symbol->string 'name))))]))

(define-syntax (run stx)
  (syntax-parse stx
    [(run name:id
          (~optional (pred:id ...))
          (~optional ((sig:id (~optional lower:nat #:defaults ([lower #'0])) upper:nat) ...)))
    #'(begin
      (define run-name (symbol->string 'name))
      (define run-state curr-state)
      (define run-preds (~? (~@ (list pred ...)) (~@ (list)))) 

      (define sig-bounds
        (for/hash ([name (list 'sig ...)]
                   [lo (list lower ...)]
                   [hi (list upper ...)])
          (values (symbol->string name) (Range lo hi))))
      (define bitwidth (if (hash-has-key? sig-bounds "int")
                           (begin0 (hash-ref sig-bounds "int")
                                   (hash-remove! sig-bounds "int"))
                           DEFAULT-BITWIDTH))
      (define default-bound DEFAULT-SIG-BOUND)
      (define run-bounds (Bound default-bound bitwidth sig-bounds))

      (define run-info (Run run-name run-state run-preds run-bounds))

      (run-spec run-info))]))


(define (run-spec run-info)
  (define run-state (Run-state run-info))
  (define run-preds (Run-preds run-info))
  (define bitwidth (Bound-bitwidth (Run-bounds run-info)))

  ; get kk names, min sets, and max sets of sigs and relations
  (define-values (sig-to-name ; Map<String, int>
                  sig-to-min  ; Map<String, List<int>>
                  sig-to-max  ; Map<String, List<int>>
                  num-atoms)  ; int
                 (get-sig-info run-info))

  (define-values (rel-to-name ; Map<String, int>
                  rel-to-min  ; Map<String, nones??>
                  rel-to-max) ; Map<String, List<List<int>>>
                 (get-relation-info run-info sig-to-max (+ (hash-count sig-to-name) 2)))

  ;; PRINT TO KODKOD CLI
  ; print configure
  ; declare univ size

  ; declare ints
  ; print Int sig (r0)
  ; print succ relation (r1)

  ; print sigs (r2 ... rm)
  ; print relations (r(m + 1) ... rn)

  ; print formula / assert formula (f0 ... fk)

  ; print solve


  ; print configure and declare univ size
  (configure (format ":bitwidth ~a :solver ~a :max-solutions 1 :verbosity 7 :sb ~a :core-gran ~a :log-trans ~a"
                     bitwidth "SAT4J" 20 0 1))
  (declare-univ num-atoms)

  ; declare ints
  (define num-ints (expt 2 bitwidth))
  (declare-ints (range (- (/ num-ints 2)) (/ num-ints 2)) ; ints
                (range num-ints))                         ; indexes

  ; print Int sig and succ relation
  (define int-rel (map list (range num-ints)))
  (define succ-rel (map list (range num-ints) 
                             (range 1 (+ num-ints 1))))
  (declare-rel (r 0) int-rel int-rel)
  (declare-rel (r 1) succ-rel succ-rel)

  ; declare sigs
  (define sigs (map car (sort (hash->list sig-to-name) @< #:key cdr)))
  (for ([sigstr sigs])
    (define name (hash-ref sig-to-name sigstr))
    (define lo 
      (if (cons? (hash-ref sig-to-min sigstr))
          (tupleset #:tuples (map list (hash-ref sig-to-min sigstr)))
          "none"))
    (define hi (tupleset #:tuples (map list (hash-ref sig-to-max sigstr))))
    (declare-rel (r name) lo hi))

  ; declare relations
  (define relations (map car (sort (hash->list rel-to-name) @< #:key cdr)))
  (for ([relstr relations])
    (define name (hash-ref rel-to-name relstr))
    (define lo (hash-ref rel-to-min relstr))
    (define hi (hash-ref rel-to-max relstr))
    (declare-rel (r name) lo hi))
  

  ; declare assertions
  (define run-constraints 
    (append run-preds
            (get-sig-size-preds run-info)
            (get-relation-preds run-info)
            (get-extender-preds run-info)))
            ;(get-break-preds run-info)))


  ; the extra sorting process is so that translate-to-kodkod knows
  ; which names to use in predicate translation.
  (define all-sigs
    (map Sig-rel (sort (hash-values (State-sigs run-state))
                       @<
                       #:key (compose (curry hash-ref sig-to-name )
                                      Sig-name))))
  (define all-relations
    (map Relation-rel (sort (hash-values (State-relations run-state))
                            @<
                            #:key (compose (curry hash-ref rel-to-name )
                                           Relation-name))))
  (define all-rels (append (list Int succ) all-sigs all-relations))

  ; run predicates
  (for ([p run-constraints]
        [assertion-number (in-naturals)])
    (print-cmd-cont "(f~a " assertion-number)
    (translate-to-kodkod-cli p all-rels '())
    (print-cmd ")")
    (assert (f assertion-number)))

  (solve))

; get-sig-info :: Run -> Map<String, int>, 
;                        Map<String, List<int>>, 
;                        Map<String, List<int>>, 
;                        int
; Given a run, assigns names to each sig, minimum and maximum 
; sets of atoms for each, and the total number of atoms needed (including ints).
(define (get-sig-info run-info)
  ;; extract state info
  (define state (Run-state run-info))
  (define sigs-map (State-sigs state))
  (define sigs (hash-values sigs-map))
  (define run-bounds (Run-bounds run-info))
  (define sig-bounds (Bound-sig-bounds run-bounds))
  (define bitwidth (Bound-bitwidth run-bounds))

  ; get top-level sigs
  (define top-level-sigs (filter (compose @not (curry Sig-extends)) sigs))


  ;; get true bounds (Map<String, Range>)
  ; this takes into account the demanded atoms from (recursive) child sigs
  ; and also contains the default bounds for unspecified sigs
  (define true-bounds (make-hash))

  ; get-demand :: Sig -> int
  ; updates lower and upper bounds and
  ; returns the (minimum) number of atoms that a particular sig demands from its parent
  (define (get-demand sig)
    ; recur and get childrens demands
    (define children (map (curry hash-ref sigs-map ) (Sig-extenders sig)))
    (define demands (apply + (map get-demand children)))

    ; generate true bounds
    (define bounds (hash-ref sig-bounds (Sig-name sig) (or (Bound-default-bound run-bounds)  
                                                           DEFAULT-SIG-BOUND)))
    ;(assert (<= demands (Range-upper bounds)))
    (define true-demands (@max demands (Range-lower bounds)))
    (hash-set! true-bounds (Sig-name sig) (Range true-demands (Range-upper bounds)))

    ; return own demands
    true-demands)

  (for-each get-demand top-level-sigs)
  ; ideally should freeze lower-bounds and upper-bounds here


  ;; to return
  (define sig-to-name (make-hash))
  (define sig-to-min (make-hash))
  (define sig-to-max (make-hash))

  ; name updaters
  (define curr-relation 2)
  (define curr-max-atom (expt 2 (or bitwidth DEFAULT-BITWIDTH)))

  ; generate :: Sig, List<int>, List<int> -> void
  ; Takes in a Sig, and two integer lists and
  ; sets name, min list, and max list of atoms for sig.
  ; private-atoms will be the lower bound allocated atoms,
  ; and shared-atoms will be a pool of extra atoms which can
  ; be used to populate up to upper bound.
  (define (generate sig private-atoms shared-atoms)
    ; set name of sig
    (hash-set! sig-to-name (Sig-name sig) curr-relation)
    (set! curr-relation (add1 curr-relation))

    ; find min and max list of atoms
    (define min-atoms private-atoms)
    (define max-atoms (append private-atoms shared-atoms))

    ; update hash map
    (hash-set! sig-to-min (Sig-name sig) min-atoms)
    (hash-set! sig-to-max (Sig-name sig) max-atoms)

    ; get children lower bounds
    (define children (map (curry hash-ref sigs-map ) (Sig-extenders sig)))
    (define children-lower (map (compose (curry Range-lower )
                                         (curry hash-ref true-bounds )
                                         (curry Sig-name ))
                                children))

    ; generate new private and shared atoms for children
    (define num-new-private (apply + children-lower))
    (define-values (new-private-atoms new-shared-atoms) (split-at max-atoms num-new-private))
    (define max-allocated 0)

    (define (allocate-atoms child child-lower)
      (define child-private-atoms (drop (take new-private-atoms 
                                              (+ max-allocated child-lower))
                                        max-allocated))
      (generate child child-private-atoms new-shared-atoms)
      (set! max-allocated (+ max-allocated child-lower)))

    (for-each allocate-atoms children children-lower)
  )

  (define (generate-top-level sig)
    (define sig-bound (hash-ref true-bounds (Sig-name sig)))
    (define lower (Range-lower sig-bound))
    (define upper (Range-upper sig-bound))

    (define private-atoms (range curr-max-atom (+ curr-max-atom lower)))
    (define shared-atoms (range (+ curr-max-atom lower) (+ curr-max-atom upper)))
    (set! curr-max-atom (+ curr-max-atom upper))

    (generate sig private-atoms shared-atoms))

  (for-each generate-top-level top-level-sigs)

  (values sig-to-name sig-to-min sig-to-max curr-max-atom)
)

; get-relation-info :: Run -> Map<String, int>, 
;                             Map<String, nones?>, 
;                             Map<String, List<List<int>>>
; Given a run and the atoms assigned to each sig, assigns names to each relation
; and minimum and maximum sets of atoms for each relation.
(define (get-relation-info run-info sig-to-max start-relation-name)
  (define state (Run-state run-info))
  (define relations (hash-values (State-relations state)))
  
  (define rel-to-name
    (for/hash ([relation relations]
               [name (in-naturals start-relation-name)])
    (values (Relation-name relation) name)))

  (define (n-arity-none arity)
        (cond
          [(equal? arity 1) 'none]
          [(@> arity 0) (product 'none (n-arity-none (@- arity 1)))]
          [else (error "Error: Relation with negative or 0 arity specified.")]))
  (define rel-to-min
    (for/hash ([relation relations])
      (define nones (n-arity-none (length (Relation-sigs relation))))
      (values (Relation-name relation) nones)))

  (define rel-to-max
    (for/hash ([relation relations])
      (define atoms ; List<List<int>>
        (map (curry hash-ref sig-to-max ) (Relation-sigs relation)))
      (define tuples (apply cartesian-product atoms))
      (values (Relation-name relation) tuples)))

  (values rel-to-name rel-to-min rel-to-max)
  )

(define (get-sig-size-preds run-info) 
  (define run-state (Run-state run-info))
  (define sig-map (State-sigs run-state))
  (define extender-sigs (filter Sig-extends (hash-values sig-map)))

  (define run-bounds (Run-bounds run-info))
  (define sig-bounds (Bound-sig-bounds run-bounds))
  (define default-bounds (Bound-default-bound run-bounds))

  (for/list ([sig extender-sigs])
    (define upper-bound 
      (Range-upper (hash-ref sig-bounds 
                             (Sig-name sig) 
                             (or default-bounds DEFAULT-SIG-BOUND))))
    (<= (card (Sig-rel sig)) (node/int/constant upper-bound))))

(define (get-extender-preds run-info)
  (define run-state (Run-state run-info))
  (define sig-map (State-sigs run-state))

  (define sig-constraints (for/list ([sig (hash-values sig-map)])
    ; get children information
    (define children-sigs (map (curry hash-ref sig-map ) 
                               (Sig-extenders sig)))
    (define children-rels (map Sig-rel children-sigs))

    ; abstract and sig1, ... extend => (= sig (+ sig1 ...))
    ; not abstract and sig is parent of sig1 => (in sig1 sig)
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

  (apply append sig-constraints))

(define (get-relation-preds run-info)
  (define run-state (Run-state run-info))
  (define sig-map (State-sigs run-state))
  (define relation-map (State-relations run-state))

  (for/list ([relation (hash-values relation-map)])
    (define children-sigs (map (curry hash-ref sig-map ) (Relation-sigs relation)))
    (define children-rels (map Sig-rel children-sigs))
    (in (Relation-rel relation) (apply -> children-rels))))



(sig A #:abstract)
(sig A1 #:one #:extends A)
(sig A2 #:extends A)
(sig A3 #:extends A)
(sig A31 #:extends A3)
(sig A32 #:extends A3)

(relation R1 (A1 A2 A3))

; (pred P1 (in A1 A))
; (pred (ni s1 s2) (in s2 s1))
; (pred P2 (ni A A2))
(fun (my-func x y) (+ x y))
(const A12 (my-func A1 A2))
(pred (P x y) (or (in x y) (in y x)))
(pred P1 (P A12 A))
(pred P2 true)

(run my-run () ())



