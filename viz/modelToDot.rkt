#lang rosette

(require ocelot)
(require "../ocelot/goatswolves.rkt")
(require graph)
; What does this file have to do? Has to take a model, convert to graph, print graph to dot, use dot to make svg.
; Website always imports the same svg, so don't have to worry about that.
; How do I do this?
; OK for all non-singleton tuples, make graphs out of them!

; fuck its cuz these are relations of higher arity. Hwow do I do this?
; How do I represent this? Alloy does it with labels on the edges. Do I want to do that?
; It would sorta be nice if I could also do it with single lines. Cuz it's the same idea, it just links many nodes together instead of just one.

; OK yea alloy just has intermediate things in the label. Do that.

; OK plan: how do I do this?
; Take each tuple. Add to graph first and last, with label composed of middle.
; ahhhhhh, fukkit. I need labeling, which means extending the graphviz system.
; basically adding an alternative weight, called label. and I can totally do that, but I don't want to do it right now.
; What can I get working for tomorrow? Pretty sure the only thing I could get working for tomorrow is doubleton relations.

; So may be better to focus on ocelot for now, work on cardinality.
; eh but if i can get a mini visualizer working, that's mroe useful So I'll do that.
; Will convert everything to arity 2, and then do that.

; models will be trimmed! don't worry about that. Everything in the model is a relation that needs to be shown.
; The tricky bit for me is converting it to json
; OK: how to convert to json?
; Each relation name indexes to a relation.
; The relations are represented by an array of arrays.
; each relation name should also index to an arity. that's in a separate object.

; step one is to ake a single json object, from a single relation.

(

(define mod '((s1 g1 far)
              (s1 g2 near)
              (s1 g3 near)
              (s1 w1 near)
              (s1 w2 far)
              (s1 w3 far)
              (s1 boat near)
              (s2 g1 far)
              (s2 g2 near)
              (s2 g3 near)
              (s2 w1 near)
              (s2 w2 far)
              (s2 w3 near)
              (s2 boat far)
              (s3 g1 near)
              (s3 g2 near)
              (s3 g3 near)
              (s3 w1 far)
              (s3 w2 far)
              (s3 w3 near)
              (s3 boat near)
              (s4 g1 near)
              (s4 g1 far)
              (s4 g2 near)
              (s4 g2 far)
              (s4 g3 near)
              (s4 g3 far)
              (s4 w1 near)
              (s4 w1 far)
              (s4 w2 near)
              (s4 w2 far)
              (s4 w3 near)
              (s4 w3 far)
              (s4 boat near)
              (s4 boat far)
              (s5 g1 near)
              (s5 g2 near)
              (s5 g3 near)
              (s5 w1 near)
              (s5 w2 near)
              (s5 w3 near)
              (s6 g1 far)
              (s6 g2 far)
              (s6 g3 far)
              (s6 w1 far)
              (s6 w2 far)
              (s6 w3 far)
              (s6 boat far)
              (s7 g1 near)
              (s7 g1 far)
              (s7 g2 near)
              (s7 g2 far)
              (s7 g3 near)
              (s7 g3 far)
              (s7 w1 near)
              (s7 w1 far)
              (s7 w2 near)
              (s7 w2 far)
              (s7 w3 near)
              (s7 w3 far)
              (s8 g1 near)
              (s8 g1 far)
              (s8 g2 near)
              (s8 g2 far)
              (s8 g3 near)
              (s8 g3 far)
              (s8 w1 near)
              (s8 w1 far)
              (s8 w2 near)
              (s8 w2 far)
              (s8 w3 near)
              (s8 w3 far)
              (s8 boat far)
              (s9 g1 near)
              (s9 g1 far)
              (s9 g2 near)
              (s9 g2 far)
              (s9 g3 near)
              (s9 g3 far)
              (s9 w1 near)
              (s9 w1 far)
              (s9 w2 near)
              (s9 w2 far)
              (s9 w3 near)
              (s9 w3 far)
              (s10 g1 far)
              (s10 g2 far)
              (s10 g3 far)
              (s10 w1 near)
              (s10 w2 far)
              (s10 w3 near)
              (s10 boat far)
              (s11 g1 far)
              (s11 g2 near)
              (s11 g3 far)
              (s11 w1 near)
              (s11 w2 near)
              (s11 w3 far)
              (s11 boat near)))

; Given just one 
; Returns a list of all non-singleton relations


(define (strip-middle rel)
  (map (lambda (x) (list (first x) (last x))) rel))

(define (make-doubletons model keys)
  (map (lambda (x) (if (= (relation-arity x) 2)
                       (hash-ref model x)
                       (strip-middle (hash-ref model x))))
       keys))

(define (non-singleton-keys model)
  (filter (lambda (x) (> (relation-arity x) 1))
               (hash-keys model)))

(define k (first (non-singleton-keys model)))
(define rel (hash-ref model k))
(define strip (strip-middle rel))

;(define graphvizs (map graphviz (map directed-graph (non-singletons model))));
;(display graphvizs)


  

;(define g (unweighted-graph/undirected '((a b) (c d))))
;(displayln (graphviz g))