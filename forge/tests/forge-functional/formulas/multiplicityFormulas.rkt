#lang forge/core

(set-option! 'verbose 0)

(define Color (make-sig 'Color #:abstract #t))
(define Red (make-sig 'Red #:one #t #:extends Color))
(define Green (make-sig 'Green #:one #t #:extends Color))
(define Blue (make-sig 'Blue #:one #t #:extends Color))

(define Node (make-sig 'Node #:abstract #t))
(define N1 (make-sig 'N1 #:one #t #:extends Node))
(define N2 (make-sig 'N2 #:one #t #:extends Node))
(define N3 (make-sig 'N3 #:one #t #:extends Node))

(define edges (make-relation 'edges (list Node Node Color)))

(define test-inst
  (make-inst (list
              (= edges (+ (-> (+ (-> (atom 'N10) (atom 'N20))
                                 (+ (-> (atom 'N10) (atom 'N30))
                                    (+ (-> (atom 'N20) (atom 'N30))
                                       (-> (atom 'N30) (atom 'N30))))) (atom 'Red0))
                          (-> (+ (-> (atom 'N10) (atom 'N10))
                                 (+ (-> (atom 'N10) (atom 'N20))
                                    (+ (-> (atom 'N10) (atom 'N30))
                                       (+ (-> (atom 'N20) (atom 'N30))
                                          (-> (atom 'N30) (atom 'N20)))))) (atom 'Green0)))))))

(define Some
  (&&/func
   (some      (join N1 (join edges Red)))
   (some      (join N2 (join edges Red)))
   (some      (join N3 (join edges Red)))
   (! (some (join (join edges Red) N1)))
   (some      (join (join edges Red) N2))
   (some      (join (join edges Red) N3))))

(define No
  (&&/func
   (! (no (join N1 (join edges Red))))
   (! (no (join N2 (join edges Red))))
   (! (no (join N3 (join edges Red))))
   (no      (join (join edges Red) N1))
   (! (no (join (join edges Red) N2)))
   (! (no (join (join edges Red) N3)))))

(define One1
  (&&/func
   (! (one (join N1 (join edges Red))))
   (one      (join N2 (join edges Red)))
   (one      (join N3 (join edges Red)))
   (! (one (join (join edges Red) N1)))
   (one      (join (join edges Red) N2))
   (! (one (join (join edges Red) N3)))))

(define Lone1
  (&&/func
   (! (lone (join N1 (join edges Red))))
   (lone      (join N2 (join edges Red)))
   (lone      (join N3 (join edges Red)))
   (lone      (join (join edges Red) N1))
   (lone      (join (join edges Red) N2))
   (! (lone (join (join edges Red) N3)))))

; These are treated as multiplicity formulas by ast.rkt,
; rather than quantifier formulas.

(define One2
  (&&/func
   (one ([n Node])
        (in (-> n Node)
            (join edges Green)))

   (one ([n1 Node]
         [n2 Node])
        (and (!= n1 n2)
             (in (+ (-> n1 n2) (-> n2 n1))
                 (join edges Green))))

   (one ([n Node]
         [c Color])
        (in (-> n (-> Node c))
            edges))))

(define Lone2
  (&&/func
   (lone ([n Node]) ; one
         (in (-> n Node)
             (join edges Green)))

   (lone ([n Node]) ; no
         (in (-> Node n)
             (join edges Green)))


   (lone ([n1 Node] ; one
          [n2 Node])
         (and (!= n1 n2)
              (in (+ (-> n1 n2) (-> n2 n1))
                  (join edges Green))))

   (lone ([n1 Node] ; no
          [n2 Node])
         (in (+ (-> Node (+ n1 n2))
                (-> (+ n1 n2) Node))
             (join edges Green)))


   (lone ([n Node] ;one
          [c Color])
         (in (-> (-> n Node) c)
             edges))

   (lone ([n Node] ; no
          [c Color])
         (in (-> (+ (-> n Node) (-> Node n)) c)
             edges))))

(define (SomePred n)
  (&&/func
   (in (-> (-> n Node) Red)
       edges)))

(define Equivalence
  (&&/func
   (iff (lone ([n Node]) (SomePred n))
        (or (no ([n Node]) (SomePred n))
            (one ([n Node]) (SomePred n))))))



(make-test #:name 'someAsMultiplicity
           #:preds (list Some)
           #:bounds (list test-inst)
           #:sigs (list Color Red Green Blue Node N1 N2 N3)
           #:relations (list edges)
           #:expect 'theorem)

(make-test #:name 'noAsMultiplicity
           #:preds (list No)
           #:bounds (list test-inst)
           #:sigs (list Color Red Green Blue Node N1 N2 N3)
           #:relations (list edges)
           #:expect 'theorem)

(make-test #:name 'oneAsMultiplicity
           #:preds (list One1)
           #:bounds (list test-inst)
           #:sigs (list Color Red Green Blue Node N1 N2 N3)
           #:relations (list edges)
           #:expect 'theorem)

(make-test #:name 'loneAsMultiplicity
           #:preds (list Lone1)
           #:bounds (list test-inst)
           #:sigs (list Color Red Green Blue Node N1 N2 N3)
           #:relations (list edges)
           #:expect 'theorem)

; (test oneAsQuantifer ; CUURRENTLY BUGGED
;       #:preds [One2]
;       #:bounds [test-inst]
;       #:expect theorem)

; (test loneAsQuantifer ; CUURRENTLY BUGGED
;       #:preds [Lone2]
;       #:bounds [test-inst]
;       #:expect theorem)

(make-test #:name 'loneEquivalentOneNo
           #:preds (list Equivalence)
           #:sigs (list Color Red Green Blue Node N1 N2 N3)
           #:relations (list edges)
           #:expect 'theorem)
