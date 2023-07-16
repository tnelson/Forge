#lang forge/core

;(require (only-in "../../lang/ast.rkt" node/expr/op/->))

(provide tup2Expr)

#|
Function tup2Expr that has the purpose of translating a given tuple (i.e. '(Node0 Node1), where Node0 and Node1 are atoms)
   to an expression (i.e. Node0->Node1, where Node0 and Node1 are the corresponding constant relations for each atom).
In essence, this function goes from semantics (a row in a database/relation) to syntax (an *expression* that can
  later be used in a formula that can constrain the world relative to those atoms.
|#

; Function tuple2Expression which turns a Tuple into an Expression
; Backup
#|(define (tup2Expr tuple context)
  ;(printf "tup2Expr: testing is context a Run? ~a~n" (forge:Run? context))
  (define tupRelationList
   ; replace every element of the tuple (atoms) with the corresponding atom relation
   (map
    (lambda (tupElem)
      ; keep only the atom relations whose name matches tupElem
      (define filterResult
        (filter (lambda (atomRel)
                  (or (equal? tupElem (forge:relation-name atomRel))
                      (and (symbol? tupElem)
                           (equal? (symbol->string tupElem) (forge:relation-name atomRel)))))
                (forge:Run-atom-rels context)))
      (cond [(equal? 1 (length filterResult)) (first filterResult)]
            [else (error (format "tup2Expr: ~a had <>1 result in atom rels: ~a" tupElem filterResult))]))
    tuple))
  ; TODO: once Tim revises the AST, will need to provide a source location
  (node/expr/op/-> (length tupRelationList) tupRelationList))|#

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
                  ;tupleElem is a string and atomRel is a symbol
                  (print (symbol? atomRel))
                  (print (not (symbol? atomRel)))
                  (cond
                    [(and (string? tupElem) (not (symbol? atomRel))) (equal? tupElem (string atomRel))]
                    [(and (string? tupElem) (symbol? atomRel)) (equal? tupElem (symbol->string atomRel))]
                    [(and (not (string? tupElem)) (not (symbol? atomRel))) (equal? (symbol->string tupElem) (number->string atomRel))]
                    [(and (not (string? tupElem)) (symbol? atomRel)) (equal? (symbol->string tupElem) (symbol->string atomRel))]))
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



