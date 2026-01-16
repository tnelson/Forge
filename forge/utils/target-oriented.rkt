#lang typed/racket/base/optional

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper module for gadgets related to target-oriented model finding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (prefix-in @ (only-in racket/base - +))
         racket/list)

;; Import types and typed AST functions from the adapter
(require forge/types/ast-adapter)

;; Import from sigs-structs (which is typed)
(require forge/sigs-structs)

;; Typed imports from sigs-functional (untyped module)
(require/typed forge/sigs-functional
  [make-sig (->* () (Symbol #:one Boolean #:lone Boolean #:abstract Boolean
                            #:is-var (U String Boolean) #:in (U Sig False)
                            #:extends (U Sig False) #:info (U nodeinfo False))
                 Sig)]
  [make-relation (->* ((U Symbol (Listof Sig)))
                      ((Listof (U Sig (-> Sig)))
                       #:is (U node/breaking/break False)
                       #:is-var (U String Boolean)
                       #:info (U nodeinfo False))
                      Relation)]
  [make-inst (-> (Listof Any) Inst)]
  [get-sig (-> State Symbol (U Sig False))]
  [get-relation (-> State Symbol (U Relation False))]
  [Int Sig]
  [DEFAULT-BITWIDTH Nonnegative-Integer])

(provide build-int-opt-gadget)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;               Integer minimization/optimization
;
; Encode an integer-expression minimization problem as a relation
; minimization problem for Pardinus' target-oriented solver. The strategy
; works with the help of 3 hidden relations:
;
; (1) A sig `__K_HELPER_ATOM`, which contains exactly 2^{bitwidth} distinct
;     new atoms.
; (2) A 2-ary relation `__OPT_K_HELPER` on (Int -> _K_HELPER_ATOM), mapping
;     integer values within the current bitwidth to sets of helper atoms.
;     Each integer maps to a different set that contains the prior int's set
;     (if any). The distinct min[Int] maps to the empty set.
; (3) A sig `__OPT_K_COUNT_SET`, extending `__K_HELPER_ATOM`.
;
; We then add two new components to the solver problem:
;
; (A) A partial instance `__OPT_K_INST` that exact bounds `__K_HELPER_ATOM`
;     and `__OPT_K_COUNT_SET` according to the current bitwidth. We could
;     use constraints here, but using a partial instance will be more efficient.
; (B) A constraint that the contents of `__OPT_K_COUNT_SET` are equal to the join:
;         (<given-int-expr> . __OPT_K_HELPER)
;     where <given-int-expr> is the minimization target.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: list-helper-atoms (-> Integer Integer (Listof node/expr)))
(define (list-helper-atoms kmin kmax)
  ; E.g., from -8 to +7: `__HELPER0 through `__HELPER15
  (build-list (@+ 1 (@- kmax kmin))
              (lambda ([v : Integer])
                (atom/func (string->symbol (format "__HELPER~a" v))))))

(: list-helper-tuples (-> Integer Integer (Listof node/expr)))
(define (list-helper-tuples kmin kmax)
  ; E.g., from -8 to +7: (-7 -> `__HELPER0) + (-6 -> (`__HELPER0 + `__HELPER1)) + ...
  ; min[Int] is represented by the empty set: "no (min[Int]).__OPT_K_HELPER)" holds.
  (build-list (@- kmax kmin)
              (lambda ([offset : Integer])
                (->/func (sing/func (int/func (@+ kmin offset 1)))
                      (if (eq? offset 0)
                          (atom/func (string->symbol "__HELPER0"))
                          (fold-ast +/func (build-list (@+ offset 1)
                                       (lambda ([atomnum : Integer])
                                         (atom/func (string->symbol (format "__HELPER~a" atomnum)))))))))))


; Given an integer expression, generate a "gadget" in the form
; of additional partial bounds and additional constraints that
; will aid in translating the integer expression target to a
; standard relational target.
(: build-int-opt-gadget
   (-> Any                              ; given-int-expr (node/int)
       (U Scope (Listof Any))           ; run-scope
       Any                              ; run-bounds
       (Listof node/formula)            ; run-preds
       (-> State)                       ; get-curr-state
       (-> State Void)                  ; update-state!
       (-> State Symbol Sig Any State)  ; state-add-sig
       (-> State Symbol Relation State) ; state-add-relation
       (Values (HashTable Symbol (Listof Any))  ; target hash
               Inst                             ; augmented bounds
               (Listof node/formula))))         ; augmented preds
(define (build-int-opt-gadget given-int-expr run-scope run-bounds run-preds
                              get-curr-state update-state! state-add-sig state-add-relation)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Step 1: set up the fixed helper relations and prepare to build bounds.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Because the notion of the "state" is so entangled with the run pipeline, we'll
  ; enable these helper relations permanently as soon as Forge processes _one_ of
  ; these integer-optimization commands. This isn't ideal.

  ; The full set of possible helper atoms
  (define __K_HELPER_ATOM : Sig
    (cond [(get-sig (get-curr-state) '__K_HELPER_ATOM)
           => (lambda ([s : Sig]) s)]
          [else
           (let ([new-sig (make-sig '__K_HELPER_ATOM)])
             (update-state! (state-add-sig (get-curr-state)
                                           '__K_HELPER_ATOM
                                           new-sig #f))
             new-sig)]))

  ; The exact-bounded helper relation
  (define __OPT_K_HELPER : Relation
    (cond [(get-relation (get-curr-state) '__OPT_K_HELPER)
           => (lambda ([r : Relation]) r)]
          [else
           (let ([new-rel (make-relation '__OPT_K_HELPER (list Int __K_HELPER_ATOM))])
             (update-state! (state-add-relation (get-curr-state) '__OPT_K_HELPER new-rel))
             new-rel)]))

  ; The set of counting-helper atoms that are used in a given instance
  (define __OPT_K_COUNT_SET : Sig
    (cond [(get-sig (get-curr-state) '__OPT_K_COUNT_SET)
           => (lambda ([s : Sig]) s)]
          [else
           (let ([new-sig (make-sig '__OPT_K_COUNT_SET #:extends __K_HELPER_ATOM)])
             (update-state! (state-add-sig (get-curr-state)
                                           '__OPT_K_COUNT_SET
                                           new-sig #f))
             new-sig)]))

  (: bind-k-helper-atom (-> Integer Integer node/formula))
  (define (bind-k-helper-atom kmin kmax)
    (=/func __K_HELPER_ATOM (fold-ast +/func (list-helper-atoms kmin kmax))))

  (: bind-opt-k-helper (-> Integer Integer node/formula))
  (define (bind-opt-k-helper kmin kmax)
    (=/func __OPT_K_HELPER (fold-ast +/func (list-helper-tuples kmin kmax))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Step 2: Use the bitwidth given by the actual Forge problem to
  ; generate bounds on the helper relations. We get this from the scope,
  ; but this might be either a Scope struct or a list of size lists.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-values (bitwidth found?)
    (cond [(Scope? run-scope)
           (values (or (Scope-bitwidth run-scope) DEFAULT-BITWIDTH) #t)]
          [(list? run-scope)
           (for/fold : (Values Nonnegative-Integer Boolean)
                     ([bw : Nonnegative-Integer DEFAULT-BITWIDTH]
                      [found? : Boolean #f])
                     ([sc : Any run-scope])
             (cond [(and (list? sc)
                         (equal? Int (first sc))
                         (equal? (length sc) 2))
                    (values (assert (second sc) exact-nonnegative-integer?) #t)]
                   [(and (list? sc)
                         (equal? Int (first sc))
                         (equal? (length sc) 3))
                    (values (assert (third sc) exact-nonnegative-integer?) #t)]
                   [(and (list? sc) (equal? Int (first sc)))
                    (raise-forge-error #:msg (format "Unexpected scope-list format: ~a~n" sc)
                                       #:context #f)]
                   [else (if found?
                             (values bw #t)
                             (values DEFAULT-BITWIDTH #f))]))]
          [else
           (raise-forge-error #:msg (format "Unexpected scope format: ~a~n" run-scope)
                              #:context #f)]))

  (define num-ints : Integer (assert (expt 2 bitwidth) exact-integer?))
  (define min-int : Integer (@- (quotient num-ints 2)))
  (define max-int : Integer (@- (quotient num-ints 2) 1))

  (define __OPT_K_INST : Inst
    (make-inst
     (list (bind-k-helper-atom min-int max-int)
           (bind-opt-k-helper min-int max-int))))

  ; ...will be the result of looking up the given-int-expr's value in the __OPT_K_HELPER table.
  ; Convert given-int-expr (node/int) to a relational expression via sing/func
  (define given-int-expr* : node/int (assert given-int-expr node/int?))
  (define __OPT_K_WELLFORMED : node/formula
    (=/func __OPT_K_COUNT_SET (join/func (sing/func given-int-expr*) __OPT_K_HELPER)))

  (values
   ; the target, in hash-instance format
   (hash '__OPT_K_COUNT_SET '())
   ; the augmented relational bounds
   (if (Inst? run-bounds)
       (make-inst (list __OPT_K_INST run-bounds))
       __OPT_K_INST)
   ; the augmented constraint set
   (cons __OPT_K_WELLFORMED run-preds)))
