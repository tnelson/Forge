#lang typed/racket/base/optional

(require forge/types/structs-adapter)
(require forge/types/ast-adapter)

(provide (struct-out bounds-hints) preprocess-sigs)
(require (only-in racket/hash hash-union))
(require (only-in racket empty?)
         (prefix-in @ (only-in racket +)))

; A Sig struct has: name one lone abstract extends 
(struct bounds-hints
  (
   ; The declared max scope for this sig
   [upper-declared : Integer]
   
   ; The "surplus" capacity within that declared scope to accommodate all child sigs
   ; Contains the difference between the sig's numeric bound and the total of its 
   ; childrens'. A positive value means that there is excess in the parent
   ; (and additional atoms are needed). Otherwise, the children need more than the
   ; parent was declared to have available.
   [upper-surplus  : Integer]
   
   ;[upper-inferred : Integer]

  ;; TODO: more as needed, just an experiment so far
  )
  #:transparent)

(define-type HintMap (Immutable-HashTable Sig bounds-hints))

; Walk the tree of sigs, starting from the top-level sigs, calculating surplus scope. 
(: preprocess-sigs (-> (U Run-spec Run) HintMap))
(define (preprocess-sigs run)
  (define top-levels (get-top-level-sigs run))
  (printf "Preprocessing sigs. Top levels: ~a~n" top-levels)
  (define subtrees (for/list : (Listof HintMap) ([tl-sig top-levels])
                     (preprocess-sig run tl-sig)))
  ; hash-union requires at least one hash table, and subtrees may be empty.
  (apply hash-union (ann (hash) HintMap) subtrees))

(: preprocess-sig (-> (U Run-spec Run) Sig HintMap))
(define (preprocess-sig run the-sig)
  (define the-children (get-children run the-sig))
  (define my-declared-upper (Range-upper (get-scope run the-sig)))
  ; get-scope might return a range with #f in it, which does not mean a lack of children
  (unless my-declared-upper
    (raise-forge-error #:msg (format "Internal error: unexpected empty upper scope for ~a" the-sig)
                       #:context run))

  ; How many atoms does each child sig need in its upper bounds?
  (: children-hints HintMap)
  (define children-hints (apply hash-union (ann (hash) HintMap) (for/list : (Listof HintMap) ([child the-children]) (preprocess-sig run child))))

  ;; TODO: not inferred!!! may need to up size? or no?
  (define total-children-upper (apply @+ (hash-map children-hints (lambda ([k : Sig] [v : bounds-hints]) (bounds-hints-upper-declared v)))))
  (define my-surplus-upper (- my-declared-upper total-children-upper))
  
  ;(define my-inferred-upper 

  (define my-hints (hash the-sig (bounds-hints my-declared-upper my-surplus-upper)))
  (hash-union my-hints children-hints))
