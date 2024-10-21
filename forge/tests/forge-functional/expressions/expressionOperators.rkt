#lang forge/core

(set-option! 'verbose 0)

(define Node (make-sig 'Node))
(define edges (make-relation 'edges (list Node Node)))

; Unary operators

(define Tilde
  (&&
   (= (~ edges) 
      (set ([n1 Node] [n2 Node])
           (in (-> n2 n1) edges)))))

(define Caret
  (&&
   (implies (int< (card Node) (int 4))
            (= (^ edges)
               (+ (+ edges
                     (join edges edges))
                  (+ (join edges (join edges edges))
                     (join edges (join edges (join edges edges)))))))))

(define Star
  (&&
   (= (* edges)
      (+ (^ edges) iden))))


; Binary operators

(define Plus
  (&&
   (all ([n1 Node]
         [n2 Node])
        (= (+ (join n1 edges)
              (join n2 edges))
           (set ([n Node])
                (|| (in n (join n1 edges))
                    (in n (join n2 edges))))))))

(define Minus
  (&&
   (all ([n1 Node]
         [n2 Node])
        (= (- (join n1 edges)
              (join n2 edges))
           (set ([n Node])
                (&& (in n (join n1 edges))
                     (!in n (join n2 edges))))))))

(define Ampersand
  (&&
   (all ([n1 Node]
         [n2 Node])
        (= (& (join n1 edges)
              (join n2 edges))
           (set ([n Node])
                (&& (in n (join n1 edges))
                     (in n (join n2 edges))))))))

(define Arrow
  (&&
   (all ([n1 Node]
         [n2 Node])
        (= (-> (join n1 edges)
               (join n2 edges))
           (set ([n3 Node]
                 [n4 Node])
                (&& (in n3 (join n1 edges))
                     (in n4 (join n2 edges))))))))

(define Dot
  (&&
   (= (join edges edges)
      (set ([n1 Node]
            [n2 Node])
           (some ([n3 Node])
                 (&& (in n3 (join n1 edges))
                      (in n3 (join edges n2))))))))

(define IfThenElse1
  (&&
   (no (ite (in edges edges)
            univ
            none))))

(define IfThenElse2
  (&&
   (some (ite (some (& Node Int))
              univ
              none))))

(make-test #:name 'tilde
           #:preds (list Tilde)
           #:sigs (list Node)
           #:relations (list edges)
           #:expect 'checked)
(make-test #:name 'caret
           #:preds (list Caret)
           #:sigs (list Node)
           #:relations (list edges)
           #:expect 'checked)
(make-test #:name 'star
           #:preds (list Star)
           #:sigs (list Node)
           #:relations (list edges)
           #:expect 'checked)
(make-test #:name 'plus
           #:preds (list Plus)
           #:sigs (list Node)
           #:relations (list edges)
           #:expect 'checked)
(make-test #:name 'minus
           #:preds (list Minus)
           #:sigs (list Node)
           #:relations (list edges)
           #:expect 'checked)
(make-test #:name 'ampersand
           #:preds (list Ampersand)
           #:sigs (list Node)
           #:relations (list edges)
           #:expect 'checked)
(make-test #:name 'arrow
           #:preds (list Arrow)
           #:sigs (list Node)
           #:relations (list edges)
           #:expect 'checked)
(make-test #:name 'dot
           #:preds (list Dot)
           #:sigs (list Node)
           #:relations (list edges)
           #:expect 'checked)

(make-test #:name 'ite1
           #:preds (list IfThenElse1)
           #:sigs (list Node)
           #:relations (list edges)
           #:expect 'unsat)
(make-test #:name 'ite2
           #:preds (list IfThenElse2)
           #:sigs (list Node)
           #:relations (list edges)
           #:expect 'unsat)

; All elements of RHS that start with an element of LHS
(pred DomainRestriction
      (all ([n Node])
           (= (<: (join n edges) edges)
              (set ([n1 Node] [n2 Node]) (&& (in (-> n1 n2) edges)
                                              (in n1 (join n edges)))))))
; All elements of LHS that end with an element of RHS
(pred RangeRestriction
      (all ([n Node])
           (= (:> edges (join n edges))
              (set ([n1 Node] [n2 Node]) (&& (in (-> n1 n2) edges)
                                              (in n2 (join n edges)))))))

(make-test #:name 'lessColon
           #:preds (list DomainRestriction)
           #:sigs (list Node)
           #:relations (list edges)
           #:expect 'checked)
(make-test #:name 'colonGreater
           #:preds (list RangeRestriction)
           #:sigs (list Node)
           #:relations (list edges)
           #:expect 'checked)
