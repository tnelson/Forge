#lang forge/core

option run_sterling off
(set-option! 'verbose 0)

(define Node (make-sig 'Node))
(define edges (make-relation 'edges (list Node Node)))

(define In
  (&&/func
   (all ([n Node])
        (in (join n edges) Node))
   (all ([n Node])
        (ni Node (join n edges)))

   ; reflexive
   (all ([n Node])
        (in (join n edges) (join n edges)))

   ; anti-symmetric
   (all ([n1 Node]
         [n2 Node])
        (implies (&& (in (join n1 edges) (join n2 edges))
                      (in (join n2 edges) (join n1 edges)))
                 (= (join n1 edges) (join n2 edges))))

   ; transitive
   (all ([n1 Node]
         [n2 Node]
         [n3 Node])
        (implies (&& (in (join n1 edges) (join n2 edges))
                      (in (join n2 edges) (join n3 edges)))
                 (in (join n1 edges) (join n3 edges))))

   ; ni properly defined
   (all ([n1 Node]
         [n2 Node])
        (=> (in (join n1 edges) (join n2 edges))
            (ni (join n2 edges) (join n1 edges))))))

(define Equals
  (&&/func
   (= Node Node)

   ; reflexive
   (all ([n Node])
        (= (join n edges) (join n edges)))

   ; symmetric
   (all ([n1 Node]
         [n2 Node])
        (=> (= (join n1 edges) (join n2 edges))
            (= (join n2 edges) (join n1 edges))))

   ; transitive
   (all ([n1 Node]
         [n2 Node]
         [n3 Node])
        (=> (&& (= (join n1 edges) (join n2 edges))
                 (= (join n2 edges) (join n3 edges)))
            (= (join n1 edges) (join n3 edges))))))

(define ComboNot ; !=, !in, !ni
  (&&/func
   (all ([n1 Node]
         [n2 Node])
        (implies (!= (join n1 edges) (join n2 edges))
                 (! (= (join n1 edges) (join n2 edges)))))
   (all ([n1 Node]
         [n2 Node])
        (implies (!in (join n1 edges) (join n2 edges))
                 (! (in (join n1 edges) (join n2 edges)))))
   (all ([n1 Node]
         [n2 Node])
        (implies (!ni (join n1 edges) (join n2 edges))
                 (! (ni (join n1 edges) (join n2 edges)))))))

(make-test #:name 'InOps 
      #:preds (list In)
      #:sigs (list Node)
      #:relations (list edges)
      #:expect 'theorem)

(make-test #:name 'EqualsOp 
      #:preds (list Equals)
      #:sigs (list Node)
      #:relations (list edges)
      #:expect 'theorem)

(make-test #:name 'ComboNotOps 
      #:preds (list ComboNot)
      #:sigs (list Node)
      #:relations (list edges)
      #:expect 'theorem)
