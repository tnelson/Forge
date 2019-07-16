#lang rosette

(require forged-ocelot)
;(require "../ocelot/goatswolves.rkt")
(require json)

; This is all that should be exported to the Json.
; Javascript does ALLLL the work, once given the json. So this is all the modelToDot does.
; And i will need to talk to Charlie! All works out.

; Convert a list of tuples of datums to a list of tuples of strings
(define (ocelot-relation->JSON-relation rel)
  (map (lambda (x)
         (map (lambda (y) (symbol->string y)) x))
       rel))

(define (unfold-list lst)
  (foldl (lambda (x acc) (append x acc)) null lst))

(define (nodes-and-edges tuples)
  (define-values (nodes-temp edges) (partition (lambda (x) (= (length x) 1)) tuples))
  (define nodes (unfold-list nodes-temp))
  (values nodes edges))

(define (model-to-JSON modelhash)
  ; map each name to
  (define pairs (hash-map modelhash
            (lambda (key value)
              (cons (string->symbol (relation-name key)) (ocelot-relation->JSON-relation value)))))
  (define rels (make-hasheq pairs))
  (define all-tuples (unfold-list (hash-values rels)))
  (define-values (all-nodes all-edges) (nodes-and-edges all-tuples))
  (hasheq 'relations rels 'nodes all-nodes 'edges all-edges))

(provide model-to-JSON)

; It may be more convenient for now for this JSON expor to be in the format cytoscape expects
; Well, no, just append an object in the format cytoscape expects. So, a list of all nodes, and a list of all edges. Easy!
; How do I get all nodes here?

    
#|
(define (model-to-JSON modelhash)
  (define relname-to-arity
    (make-hasheq (map (lambda (x)
                        (println (relation-arity x))
                        (list (string->symbol (relation-name x)) (relation-arity x)))
                      (hash-keys modelhash))))
  (println relname-to-arity)
  (define relname-to-rel
    (make-hasheq (map (lambda (x) (list (string->symbol (relation-name x)) (ocelot-relation->JSON-relation (hash-ref modelhash x)))) (hash-keys modelhash))))

  (hasheq (string->symbol "arities") relname-to-arity (string->symbol "relations") relname-to-rel))|#

;(define jsontype (model-to-JSON model))
;(define text (jsexpr->string jsontype))