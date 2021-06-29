#lang forge/core

(set-option! 'verbose 0)

(define A (make-sig 'A))
(define otherA (make-relation 'otherA (list A A)))

(define B (make-sig 'B))
(define otherB (make-relation 'otherB (list B A)))

(define relations
  (make-inst (list
              (is otherA func)
              (is otherB func))))

(define ComprehensionsOnSigs
  (&&
   (= (set ([a A])
           (in (-> a a) otherA))
      (join A (& otherA iden)))))

(define ComprehensionsOnSets
  (&&
   (= (set ([x (+ A B)])
           (some ([y A])
                 (or (= (join x otherA)
                        y)
                     (= (join x otherB)
                        y))))
      (join (+ otherA otherB)
            A))))

(define MultiComprehension
  (&&
   (= (set ([b B] [a A])
           (some ([c A])
                 (and (in (-> b c)
                          otherB)
                      (in (-> c a)
                          otherA))))
      (join otherB otherA))))

(make-test #:name 'comprehensionsOnSigs 
           #:preds (list ComprehensionsOnSigs)
           #:bounds (list relations)
           #:sigs (list A B)
           #:relations (list otherA otherB)
           #:expect 'theorem)
(make-test #:name 'comprehensionsOnSets
           #:preds (list ComprehensionsOnSets)
           #:bounds (list relations)
           #:sigs (list A B)
           #:relations (list otherA otherB)
           #:expect 'theorem)
(make-test #:name 'multiComprehension
           #:preds (list MultiComprehension)
           #:bounds (list relations)
           #:sigs (list A B)
           #:relations (list otherA otherB)
           #:expect 'theorem)
