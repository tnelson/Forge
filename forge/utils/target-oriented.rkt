#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper module for gadgets related to target-oriented model finding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (prefix-in @ (only-in racket/base - +))
         forge/sigs-functional
         (only-in forge/lang/ast atom -> + int))

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

(define (list-helper-atoms kmin kmax)
  ; E.g., from -8 to +7: `__HELPER0 through `__HELPER15
  (build-list (@+ 1 (@- kmax kmin))
              (lambda (v)
                (atom (string->symbol (format "__HELPER~a" v))))))

(define (list-helper-tuples kmin kmax)
  ; E.g., from -8 to +7: (-7 -> `__HELPER0) + (-6 -> (`__HELPER0 + `__HELPER1)) + ...
  ; min[Int] is represented by the empty set: "no (min[Int]).__OPT_K_HELPER)" holds.
  (build-list (@- kmax kmin)
              (lambda (offset)
                (-> (int (@+ kmin offset 1))
                      (if (eq? offset 0)
                          (atom (string->symbol "__HELPER0"))
                          (+ (build-list (@+ offset 1)
                                         (lambda (atomnum)
                                           (atom (string->symbol (format "__HELPER~a" atomnum)))))))))))


; Given an integer expression, generate a "gadget" in the form
; of additional partial bounds and additional constraints that
; will aid in translating the integer expression target to a
; standard relational target.
(define (build-int-opt-gadget given-int-expr run-scope run-bounds run-preds
                              get-curr-state update-state! state-add-sig state-add-relation)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Step 1: set up the fixed helper relations and prepare to build bounds.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Because the notion of the "state" is so entangled with the run pipeline, we'll
  ; enable these helper relations permanently as soon as Forge processes _one_ of
  ; these integer-optimization commands. This isn't ideal.
  
  ; The full set of possible helper atoms
  (define __K_HELPER_ATOM
    (cond [(get-sig (get-curr-state) '__K_HELPER_ATOM)
           (get-sig (get-curr-state) '__K_HELPER_ATOM)]
          [else
           (let ([new-sig (make-sig '__K_HELPER_ATOM)])
             (update-state! (state-add-sig (get-curr-state)
                                           '__K_HELPER_ATOM
                                           new-sig #f))
             new-sig)]))
  
  ; The exact-bounded helper relation
  (define __OPT_K_HELPER
    (cond [(get-relation (get-curr-state) '__OPT_K_HELPER)
           (get-relation (get-curr-state) '__OPT_K_HELPER)]
          [else
           (let ([new-rel (make-relation '__OPT_K_HELPER (list Int __K_HELPER_ATOM))])
             (update-state! (state-add-relation (get-curr-state) '__OPT_K_HELPER new-rel))
             new-rel)]))
  
  ; The set of counting-helper atoms that are used in a given instance
  (define __OPT_K_COUNT_SET
    (cond [(get-sig (get-curr-state) '__OPT_K_COUNT_SET)
           (get-sig (get-curr-state) '__OPT_K_COUNT_SET)]
          [else
           (let ([new-sig (make-sig '__OPT_K_COUNT_SET #:extends __K_HELPER_ATOM)])
             (update-state! (state-add-sig (get-curr-state)
                                           '__OPT_K_COUNT_SET
                                           new-sig #f))
             new-sig)]))
  
  (define (bind-k-helper-atom kmin kmax)
    (= __K_HELPER_ATOM (+ (list-helper-atoms kmin kmax))))
  
  (define (bind-opt-k-helper kmin kmax)
    (= __OPT_K_HELPER (+ (list-helper-tuples kmin kmax))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Step 2: Use the bitwidth given by the actual Forge problem to
  ; generate bounds on the helper relations.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define bitwidth (or (and (Scope? run-scope) (Scope-bitwidth run-scope))
                       DEFAULT-BITWIDTH))
  (define num-ints (expt 2 bitwidth))
  (define min-int (@- (/ num-ints 2)))
  (define max-int (@- (/ num-ints 2) 1))
  
  (define __OPT_K_INST
    (make-inst
     (list (bind-k-helper-atom min-int max-int)
           (bind-opt-k-helper min-int max-int))))
  
  ; ...will be the result of looking up the given-int-expr's value in the __OPT_K_HELPER table.
  (define __OPT_K_WELLFORMED (= __OPT_K_COUNT_SET (join given-int-expr __OPT_K_HELPER)))

  (values
   ; the target, in hash-instance format
   (hash '__OPT_K_COUNT_SET '())
   ; the augmented relational bounds
   (or (and (Inst? run-bounds) (make-inst (list __OPT_K_INST run-bounds)))
       __OPT_K_INST)
   ; the augmented constraint set
   (cons __OPT_K_WELLFORMED run-preds)
   ; the two sigs
   ;(list __OPT_K_COUNT_SET __K_HELPER_ATOM)
   ; the one relation
   ;(list __OPT_K_HELPER)
   ))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Step 3: Create the run
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Note well: this excludes temporal mode
;(set-option! 'problem_type 'target)
; Note well: this isn't supported on all operating systems yet
;(set-option! 'solver 'PMaxSAT4J)

; Overflows can make this look strange. E.g., at bitwidth 3, 8 edges, becomes "minimal" for #edges.
;(set-option! 'no_overflow 'true)
; We also need to make sure that the given bitwidth above actually matches the bitwidth of the run. 


