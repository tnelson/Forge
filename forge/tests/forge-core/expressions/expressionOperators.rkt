#lang forge/core

(set-option! 'verbose 0)

(sig Node)
(relation edges (Node Node))

; Unary operators

(pred Tilde
    (= (~ edges) 
       (set ([n1 Node] [n2 Node]) 
           (in (-> n2 n1) edges))))

(pred Caret
    (implies (int< (card Node) (int 4))
             (= (^ edges)
                (+ (+ edges
                      (join edges edges))
                   (+ (join edges (join edges edges))
                      (join edges (join edges (join edges edges))))))))

(pred Star
    (= (* edges)
       (+ (^ edges) iden)))


; Binary operators

(pred Plus
    (all ([n1 Node]
          [n2 Node])
        (= (+ (join n1 edges)
              (join n2 edges))
           (set ([n Node])
               (|| (in n (join n1 edges))
                   (in n (join n2 edges)))))))

(pred Minus
    (all ([n1 Node]
          [n2 Node])
        (= (- (join n1 edges)
              (join n2 edges))
           (set ([n Node])
               (&& (in n (join n1 edges))
                    (!in n (join n2 edges)))))))

(pred Ampersand
    (all ([n1 Node]
          [n2 Node])
        (= (& (join n1 edges)
              (join n2 edges))
           (set ([n Node])
               (&& (in n (join n1 edges))
                    (in n (join n2 edges)))))))

(pred Arrow
    (all ([n1 Node]
          [n2 Node])
        (= (-> (join n1 edges)
               (join n2 edges))
           (set ([n3 Node]
                 [n4 Node])
               (&& (in n3 (join n1 edges))
                    (in n4 (join n2 edges)))))))

(pred Dot
    (= (join edges edges)
       (set ([n1 Node]
             [n2 Node])
           (some ([n3 Node])
               (&& (in n3 (join n1 edges))
                    (in n3 (join edges n2)))))))

(pred IfThenElse1
      (no (ite (in edges edges)
                 univ
                 none)))
(pred IfThenElse2
      (some (ite (some (& Node Int))
                 univ
                 none)))



#|
CURRENTLY BUGGED?
pred LessColon {
    all n: Node |
        n.edges <: edges = {n1: Node, n2: Node | n1->n2 in edges and n1 in n.edges}
}

pred ColonGreater {
    all n: Node |
        edges :> n.edges = {n1: Node, n2: Node | n1->n2 in edges and n2 in n.edges}
}
|#

; Helper functions `fun`
; forge/core's `fun` macro doesn't support no-args functions, because `define` works just as well.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TEST VARIATIONS OF MACRO USE (core)
; Single argument (arg domain?, arg mult?, result domain? result mult?]
(fun (helper1a x) (& x Int))                                    ; NNNN
(fun (helper1b (x univ)) (& x Int))                             ; YNNN
(fun (helper1c (x univ 'lone)) (& x Int))                       ; YYNN
(fun (helper1d (x univ)) (& x Int) #:codomain Int)              ; YNYN
(fun (helper1e (x univ)) (& x Int) #:codomain (Int 'set))       ; YNYY
(fun (helper1f (x univ 'lone)) (& x Int) #:codomain Int)        ; YYYN
(fun (helper1g (x univ 'lone)) (& x Int) #:codomain (Int 'set)) ; YYYY

; Multiple arguments (1 domain, 2 domain, 1 mult, 2 mult)
(fun (helper2a x y) (& x y))                                      ; NNNN
(fun (helper2b (x univ) y) (& x y))                               ; YNNN
(fun (helper2c x (y univ)) (& x y))                               ; NYNN
(fun (helper2d (x univ) (y univ)) (& x y))                        ; YYNN
(fun (helper2e (x univ 'lone) (y univ)) (& x y))                  ; YYYN
(fun (helper2f (x univ 'lone) (y univ 'set)) (& x y))             ; YYYY


;; TODO: import from forge and check contents of spacer fields

(pred HelperFun    
    (all ([value1 univ] [value2 univ])
         (and (= (helper1a value1) (& Int value1))
              (= (helper1b value1) (& Int value1))
              (= (helper1c value1) (& Int value1))
              (= (helper1d value1) (& Int value1))
              (= (helper1e value1) (& Int value1))
              (= (helper1f value1) (& Int value1))
              (= (helper1g value1) (& Int value1))
              (= (helper2a value1 value2) (& value2 value1))
              (= (helper2b value1 value2) (& value2 value1))
              (= (helper2c value1 value2) (& value2 value1))
              (= (helper2d value1 value2) (& value2 value1))
              (= (helper2e value1 value2) (& value2 value1))
              (= (helper2f value1 value2) (& value2 value1)))))


(test tilde #:preds [Tilde] #:expect theorem)
(test caret #:preds [Caret] #:expect theorem)
(test star #:preds [Star] #:expect theorem)
(test plus #:preds [Plus] #:expect theorem)
(test minus #:preds [Minus] #:expect theorem)
(test ampersandd #:preds [Ampersand] #:expect theorem)
(test arrow #:preds [Arrow] #:expect theorem)
(test dot #:preds [Dot] #:expect theorem)

(test ite1 #:preds [IfThenElse1] #:expect unsat)
(test ite2 #:preds [IfThenElse2] #:expect unsat)

(test helperfuns #:preds [HelperFun] #:expect theorem)

; (test lessColon #:preds [LessColon] #:expect theorem)
; (test colonGreater #:preds [ColonGreater] #:expect theorem)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test metadata

;(fun (helper1a x) (& x Int))                                    ; NNNN
;(fun (helper1b (x univ)) (& x Int))                             ; YNNN
;(fun (helper1c (x univ 'lone)) (& x Int))                       ; YYNN
;(fun (helper1d (x univ)) (& x Int) #:codomain Int)              ; YNYN
;(fun (helper1e (x univ)) (& x Int) #:codomain (Int 'set))       ; YNYY
;(fun (helper1f (x univ 'lone)) (& x Int) #:codomain Int)        ; YYYN
;(fun (helper1g (x univ 'lone)) (& x Int) #:codomain (Int 'set)) ; YYYY
;
;; Multiple arguments (1 domain, 2 domain, 1 mult, 2 mult)
;(fun (helper2a x y) (& x y))                                      ; NNNN
;(fun (helper2b (x univ) y) (& x y))                               ; YNNN
;(fun (helper2c x (y univ)) (& x y))                               ; NYNN
;(fun (helper2d (x univ) (y univ)) (& x y))                        ; YYNN
;(fun (helper2e (x univ 'lone) (y univ)) (& x y))                  ; YYYN
;(fun (helper2f (x univ 'lone) (y univ 'set)) (& x y))             ; YYYY

(fun (helper3 (x iden)) (& x iden)) ; arity > 1

(require (prefix-in @ rackunit))

; Check implicit application of default multiplicity
(@check-equal? (node/expr/fun-spacer-codomain (helper1a univ))
               (mexpr univ 'one))
(@check-equal? (node/expr/fun-spacer-args (helper1a univ))
               (list (apply-record 'x (mexpr univ 'one) univ)))
; No codomain provided, inferring univ -- should be inferring univ-product of appropriate arity
(@check-equal? (node/expr/fun-spacer-codomain (helper3 (-> Int Int)))
               (mexpr (-> univ univ) 'set))
(@check-equal? (node/expr/fun-spacer-args (helper3 (-> Int Int)))
               (list (apply-record 'x (mexpr iden 'set) (-> Int Int))))

