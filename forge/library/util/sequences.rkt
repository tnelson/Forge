#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Seq Library  ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (for-syntax racket/base racket/syntax syntax/srcloc syntax/parse)
         (only-in forge/lang/ast raise-forge-error pretty-name-predicate
                                 &&/info in/info ->/info univ join/info all/info
                                 sum/info int lone/info some/info =>/info sing/info
                                 subtract/info card/info -/info no/func nodeinfo)
         (only-in forge/sigs-structs Int int>= != succ min max)
         forge/lang/deparse
         syntax/parse)

; reference:
; https://github.com/AlloyTools/org.alloytools.alloy/blob/master/org.alloytools.alloy.core/src/main/resources/models/util/seqrel.als

(provide isSeqOf seqFirst seqLast indsOf idxOf lastIdxOf elems inds isEmpty hasDups seqRest)

(define-syntax (define-builtin stx)
  (syntax-parse stx
   [(define-builtin:id (opName:id locArg:id args:id ...) body:expr)
    (with-syntax ([opName/func (format-id #'opName "~a/func" #'opName)]
                  [ellip '...])
      (syntax/loc stx (begin
        (define-syntax (opName stxx)
          (syntax-parse stxx
            ; For use in forge/core; full s-expression expands to 0-ary procedure
            ; Note use of "ellip" to denote "..." for the inner macro.
            [(opName inner-args:id ellip)
             (quasisyntax/loc stxx
               (opName/func (nodeinfo #,(build-source-location stxx) 'checklangNoCheck #f) inner-args ellip))]
            ; For use with #lang forge; identifier by itself expands to 3+-ary procedure
            [opName
             (quasisyntax/loc stxx
               (lambda (args ...)
                 (opName/func (nodeinfo #,(build-source-location stxx) 'checklangNoCheck #f) args ...)))]))
        
        (define (opName/func locArg args ...)
          body)
        )))
    ]))

(define-builtin (isSeqOf info r1 d)
  (&&/info info
      (in/info info r1 (->/info info Int univ))
      (in/info info (join/info info Int r1) d)
      (all/info info ([i1 (join/info info r1 univ)])
           (&&/info info (int>= (sum/info info i1) (int 0))
               (lone/info info (join/info info i1 r1))))
      (all/info info ([e (join/info info Int r1)])
           (some/info info (join/info info r1 e)))
      (all/info info ([i1 (join/info info r1 univ)])
           (=>/info info (!= i1 (sing/info info (int 0)))
                    (some/info info (join/info info
                     (sing/info info
                      (subtract/info info
                       (sum/info info i1) (int 1))) r1))))))

(define-builtin (seqFirst info r)
  (join/info info
    (sing/info info (int 0))
    r))

(define-builtin (seqLast info r)
  (join/info info
    (sing/info info
      (subtract/info info
        (card/info info r) (int 1)))
    r))

; precondition: r isSeqOf something
(define-builtin (seqRest info r)
  (-/info info 
    (join/info info succ r)
    (->/info info (int -1) univ)))

(define-builtin (indsOf info r e)
  (join/info info r e))

(define-builtin (idxOf info r e)
  (min (join/info info r e)))

(define-builtin (lastIdxOf info r e)
  (max (join/info info r e)))

(define-builtin (elems info r)
  (join/info info Int r))

(define-builtin (inds info r)
  (join/info info r univ))

(define-builtin (isEmpty info r)
  (no/func r #:info info))

(define-builtin (hasDups info r)
  (some/info info ([e (elems/func info r)])
    (some/info info ([num1 (indsOf/func info r e)] [num2 (indsOf/func info r e)])
      (!= num1 num2))))

