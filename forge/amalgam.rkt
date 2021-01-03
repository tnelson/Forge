#lang forge/core

(set-verbosity 10)

(require forge/amalgam/desugar/desugar)

(require "amalgam/tests/forge_ex2.rkt")
(require racket/hash)
(require (prefix-in @ racket/set))
(require (prefix-in @ (only-in racket ->)))
(require debug/repl)

(require pretty-format)

;(require (only-in "breaks.rkt" sbound))
; ^ This will be a *different* struct defn; instead get via sigs

(run udt
     ; #:preds [isUndirectedTree]
     #:preds [irreflexive]
     #:scope [(Node 4)]) 

; Entry point for Amalgam from forge/core
;  This is a sketch for the moment

(provide build-provenances)

(struct provenanceNode (annotations) #:transparent)
(struct contraLeaf provenanceNode () #:transparent)
(struct alphaLeaf provenanceNode () #:transparent)
(struct desugarStep provenanceNode () #:transparent)
(struct ANDProof provenanceNode (options provTrees) #:transparent)
(struct ORProof provenanceNode (alphas obligations) #:transparent)

 
; takes a hash-table representation of an instance and a tuple and flips the truth of
;  that tuple in the instance. The tuple is <t> in <raw-r>.
(define (flip-tuple a-hash t raw-r)
  (define r (cond [(string? raw-r) (string->symbol raw-r)]
                  [(symbol? raw-r) raw-r]
                  [else (error (format "amalgam: unexpected type for relation name: ~a" raw-r))]))
  (unless (hash-has-key? a-hash r)
    (error (format "amalgam: failed to find ~a in ~a" r (hash-keys a-hash))))
  (if (member t (hash-ref a-hash r))
      ; update functionally, return immutable map
      (hash-set a-hash r (remove t (hash-ref a-hash r)))
      (hash-set a-hash r (cons t (hash-ref a-hash r)))))

; input:
;        A - a set of sets
;        B - a set of sets
;
; output: the union product of A and B which is defined as:
;         {a_i ∪ b_j |1 ≤ i ≤ n, 1 ≤ j ≤ m}
;            where n and m are the sizes of A and B respectively
; build set cartesian product of A and B
; map over the cartesian product of A and B a lambda that takes the union
; of these two things.
; 
(define/contract (union-product A B)
  (@-> set? set? set?)  
  (foldl set-union (@set) (set->list (for/set ([i A]) (for/set ([j B]) (list i j))))))

(define/contract (new-union-product A B)
  (@-> set? set? set?)
  (foldl set-union (@set)
         (set->list 
          (for/set ([i A])
            (foldl set-union (@set)
                   (set->list 
                    (for/set ([k B])
                      (foldl set-union (@set)
                             (set->list 
                              (for/set ([j i])
                                (for/set ([h k])
                                  (list j h))))))))))))

(define/contract (isLiteralIn fmla)
  (@-> node/formula? boolean?)

  (define args (node/formula/op-children fmla))
  (define LHS (first args))
  (define RHS (second args))

  ; Check if LHS is just a simple tuple 
  (define evalLHS
    (cond
      [(node/expr/op/->? LHS)
       (define children (node/expr/op-children LHS))
       ((listof node/expr/atom?) children)]
      [(node/expr/atom? LHS) #t]))

  ; Check if RHS is either a relation name, atom, the int constant, or other
  ; expression constants
  (define evalRHS
    (or
     (node/expr/relation? RHS)
     (node/expr/atom? RHS)
     (node/expr/constant? RHS)
     (node/expr/constant? RHS)))
  (and evalLHS evalRHS))

; L is the target of the provenance query
; fmla is the current target of blame
(define/contract (amalgam-descent fmla orig-run alt-run L currSign)
  (@-> node/formula? forge:Run? forge:Run? pair? boolean? (or/c set? exn:fail?))
  (printf "amalgam-descent (currSign is ~a): ~a~n" currSign fmla)
  ; Invariant: instance from orig-run satisfies fmla
  ;            instance from alt-run does not satisfy fmla
  ; For debugging purposes, fail noisily if this invariant is violated
  (unless (or (and currSign (evaluate orig-run 'unused fmla))
              (and (not currSign) (not (evaluate orig-run 'unused fmla))))
    (error (pretty-format "amalgam-descent (sign=~a): original instance failed to satisfy ~a~n" currSign fmla)))
  (when (or (and currSign (evaluate alt-run 'unused fmla))
            (and (not currSign) (not (evaluate alt-run 'unused fmla))))
    (error (pretty-format "amalgam-descent (sign=~a): L-alternate (L=~a) instance satisfied ~a~n" currSign L fmla)))  

  (define (handleAND info args)
    ; *every* arg must have satisfied the orig instance
    ; but not every arg necessarily fails in L-alt instance
    ; each failed arg is its own new provenance-set, which we union together
    (define failed-args (filter
                         (lambda (arg)
                           (if currSign
                               (not (evaluate alt-run 'unused arg))     ; real "and"
                               (evaluate alt-run 'unused arg))) args))  ; negated "or"
    (define prov-sets (map
                       (lambda (arg)
                         (amalgam-descent arg orig-run alt-run L currSign)) failed-args))
    ; TODO: Is this what we want to be returning from this case? 
    (ANDProof 'andProof failed-args prov-sets)
    ; TODO: Do we need to remove this?
    (apply set-union prov-sets))
  
  (define (handleOR info args)
    ; orig instance satisfies at least one arg (not necessarily all, but not zero)
    ; L-alt instance satisfies no args
    (define-values (orig-true-args orig-false-args) ; filter but return #f args as 2nd value
      (partition
       (lambda (arg)
         (if currSign 
             (evaluate orig-run 'unused arg)
             (not (evaluate orig-run 'unused arg)))) args))
    ; This is a big OR, so we can think of it as an implication.
    ;  furthermore, we're free to shuffle args to the left or right of the => as we see fit
    ; So specialize to the original instance: NOT OR[orig-false-args] ==> OR[orig-true-args]
    (define prov-sets (map (lambda (arg) (amalgam-descent arg orig-run alt-run L currSign)) orig-true-args))
    (define new-alpha-set (list->set (map (lambda (arg) (!/info info (list arg))) orig-false-args)))
    ; We need *ALL* of orig-true-args to fail, and may have multiple justifications for each (union product)

    ; TODO: Remove the call to union-product since that isn't what we're doing anymore 
    ; (define failure-reasons (list (foldl (lambda (x acc) (new-union-product x acc)) (first prov-sets) (rest prov-sets))))

    ; TODO: Is this what we want to return in this case? 
    (ORProof 'orProof new-alpha-set prov-sets)
    ; add prov-sets to each failure-reasons + return 
    (list->set (map (lambda (reason) (set-union new-alpha-set reason)) failure-reasons)))
  
  (match fmla
    ; base case: AND 
    [(node/formula/op/&& info args)
     (if currSign (handleAND info args) (handleOR info args))]

    ; base case: OR 
    [(node/formula/op/|| info args)
     (if currSign (handleOR info args) (handleAND info args))]

    ; base case: positive literal
    [(node/formula/op/in info args)
     ; Check that the formula is the simplest case of IN, or take it to desugar
     #:when (isLiteralIn fmla)
     ; TODO : This is wrong, might need a re-write but the check might just be enough  
     ;(if (equal? fmla L)
         (cond
           [currSign (list->set (list (list->set (list fmla))))]
           [else (list->set (list (list->set (list (not fmla)))))])]

    ; base case: negation
    [(node/formula/op/! info args)     
     (amalgam-descent (first args) orig-run alt-run L (not currSign))]
    [else
     (define desugaredFormula (desugarFormula fmla '() orig-run currSign))
     ; TODO: What can we do with this new desugar node? 
     (define newDesugarNode (desugarStep (second desugaredFormula)))
     (amalgam-descent (first desugaredFormula) orig-run alt-run L currSign)]))

; pair<list<atom>, string>, boolean, Run -> provenance-set
; Due to the way the evaluator works at the moment, this is always
; with respect to the current solver state for <a-run>.
(define (build-provenances tup orig-run)
  (printf "build-provenances ~a~n" tup)
  ; get conjunction of predicates F from the run command
  (define spec (forge:Run-run-spec orig-run))
  (define Fs (forge:Run-spec-preds spec))
  (define F (foldl (lambda (f acc) (and f acc)) (first Fs) (rest Fs)))
  
  (define state (forge:Run-spec-state spec))
  (define orig-scope (forge:Run-spec-scope spec))
  (define orig-bounds (forge:Run-spec-bounds spec))
  (define sigs (forge:State-sigs state))
  (define relations (forge:State-relations state))
  
  ; TODO This may only work if the solver state is the *first* instance in the stream
  ;   Confirm + discuss: what's the method by which the generator moves forward?
  (define orig-inst (stream-first (forge:Run-result orig-run)))
  (unless (symbol=? 'sat (car orig-inst))
    (error "amalgam called on unsat run"))
  (printf "~n  first orig instance: ~a~n" orig-inst)
  ;(printf "~n  orig-inst: ~a~n" orig-inst)
  ;(printf "~n  orig-bounds: ~a~n" (forge:Run-spec-bounds spec))
  ;(printf "~n  orig-scope: ~a~n" orig-scope) 
  (define new-totals (flip-tuple (cdr orig-inst) (car tup) (cdr tup)))
  (printf "new-totals is ~a~n" new-totals)
  ; no. "total bindings" is a misnomer. instead need to provide sbounds in pbindings
  ; e.g. (for fixed edge relation)
  ;Original PBindings: 
  ; #hash(((relation 2 "edges" (Node Node) Node) .
  ;   #(struct:sbound (relation 2 "edges" (Node Node) Node)
  ;   #<set: (Node0 Node1)> #<set: (Node0 Node1)>))
  ;    ...
  ;(define bounds (forge:Bound (hash) new-totals))
  ;(define all-rels (hash-union sigs relations))
  
  (define (make-exact-sbound rel tups)
    (forge:sbound rel (list->set tups) (list->set tups)))
  
  ; For reasons passing understanding, get-relation returns a symbol...
  (define (get-actual-relation relname)
    (cond [(hash-has-key? sigs relname) (forge:Sig-rel (hash-ref sigs relname))]
          [(hash-has-key? relations relname) (forge:Relation-rel (hash-ref relations relname))]
          [else (error "unknown relation or sig" relname)]))
 
  (define new-pbindings
    (for/hash ([k (hash-keys new-totals)]);(hash-keys orig-bounds)])
      (values (get-actual-relation k)
              (make-exact-sbound (get-actual-relation k)
                                 (hash-ref new-totals k)))))
  
  ;(printf "~n~n  new-pbindings: ~a~n" new-pbindings)
 
  (define bounds (forge:Bound new-pbindings (forge:Bound-tbindings orig-bounds)))
  (define scope (forge:Scope #f #f (hash))) ; empty
  ; can't use inst syntax here, so construct manually
  (define alt-inst
    (lambda (s b) (values scope bounds)))
  ; Get the solver to produce the L-alternate for us
  (run alt-run
       #:preds []
       #:bounds alt-inst)
  (printf "~n  first alt instance: ~a~n" (stream-first (forge:Run-result alt-run)))
  ;(printf "~n  ALT BOUNDS: ~a~n" (forge:Run-spec-bounds (forge:Run-run-spec alt-run)))
  ; evaluate to make sure tup is locally necessary  
  (define check-alt (evaluate alt-run 'unused F))
  (printf "~n  check-alt: ~a~n" check-alt)

  ; desugar F
  ; Pass in the run, not the bounds, since we may need more of the run (like atom-rels)
  ; Don't desugar first; desugar as needed in the amalgam descent
  ;(define desugared (desugarFormula F '() orig-run #t))
  
  ; do amalgam descent on desugared F
  (amalgam-descent F orig-run alt-run tup #t))


; E.g., at REPL (or in separate module that requires the forge module)
; > (require racket/stream racket/base forge/amalgam)
; > (build-provenances (cons '(Node3 Node1) "edges") #f udt)

; This will produce an error if Node1 isn't in all-atoms
;   TODO
;(build-provenances (cons '(Node1 Node1) "edges") udt)

; these are OK assuming 3, 4, 5, 6 are used
(build-provenances (cons '(Node1 Node1) "edges") udt) ; add
;(build-provenances (cons '(Node4 Node5) "edges") udt) ; remove

;(desugarFormula (in (-> (atom 'Node0) (atom 'Node1)) (& iden edges)) '() udt #f)
