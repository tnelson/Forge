#lang forge/core
;racket
;; TODO: bug report discuss with Thomas -- needed forge/core, not require sigs?

(require racket/list) ; do we need this?
;(require "../sigs.rkt") ; import Forge stuff
(require (only-in "../lang/ast.rkt" node/expr/op/->))

(provide tup2Expr)

#|
Function tup2Expr that has the purpose of translating a given tuple (i.e. '(Node0 Node1), where Node0 and Node1 are atoms)
   to an expression (i.e. Node0->Node1, where Node0 and Node1 are the corresponding constant relations for each atom).
In essence, this function goes from semantics (a row in a database/relation) to syntax (an *expression* that can
  later be used in a formula that can constrain the world relative to those atoms.
|#

; tup2Expr: (list? (or/c string? symbol?)), Run
(define (tup2Expr tuple context)
  ;(printf "tup2Expr: testing is context a Run? ~a~n" (forge:Run? context))
  (define tupRelationList
   ; replace every element of the tuple (atoms) with the corresponding atom relation
   (map
    (lambda (tupElem)
      ; keep only the atom relations whose name matches tupElem
      (define filterResult
        (filter (lambda (atomRel)
                  ;(if (positive? -5) (error "doesn't get here") 2)
                  ;tupleElem is a string and atomRel is a symbol
                  ;(if (and (string? stupElem) (not (symbol? atomRel)))
                  (cond
                    [(and (string? tupElem) (not (symbol? atomRel))) (equal? tupElem (string atomRel))]
                    [(and (string? tupElem) (symbol? atomRel)) (equal? tupElem (symbol->string atomRel))]
                    [(and (not (string? tupElem)) (not (symbol? atomRel))) (equal? (symbol->string tupElem) (string atomRel))]
                    [(and (not (string? tupElem)) (symbol? atomRel)) (equal? (symbol->string tupElem) (symbol->string atomRel))]))
                  ;(or (and (symbol? atomRel) (equal? tupElem (symbol->string atomRel)))
                  ;    (and (not (symbol? atomRel)) (equal? (string atomRel) tupElem))
                  ;    (and (symbol? tupElem)
                  ;        (equal? (symbol->string tupElem) (symbol->string atomRel)))))
                ; Is something messed up with the types?
                (forge:Run-atoms context)))
      (cond [(equal? 1 (length filterResult)) (first filterResult)]
            [else (error (format "tup2Expr: ~a had <>1 result in atom rels: ~a" tupElem filterResult))]))
    tuple))
  ; TODO: once Tim revises the AST, will need to provide a source location
  (node/expr/op/-> (length tupRelationList) tupRelationList))


; There are, unfortunately, multiple ways that we use the word "relation". There are even more in the context
; of Forge. Here are some:
;   (1) relation name (string)
;   (2) relation struct e.g.
;          (relation 1 "Node" (Node) univ) for the Node relation overall
;       or (relation 1 "Node0" (Node0) univ) for the constant relation that represents atom "Node0"
;   (3) relation as in a DB table or a "relational constant", i.e., a set of tuples of the appropriate type



