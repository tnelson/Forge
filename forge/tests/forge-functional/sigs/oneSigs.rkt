#lang forge/core

(set-option! 'verbose 0)

(define UniqueObject (make-sig 'UniqueObject #:one #t))

(define Stuff (make-sig 'Stuff))

(test oneSigEnforced
      #:preds [(= (card UniqueObject) (int 1))]
      #:expect theorem)
(test oneSigIsntPersistent
      #:preds [(= (card Stuff) (int 2))]
      #:expect sat)


(define Thing (make-sig 'Thing))
(define SpecialThing (make-sig 'SpecialThing #:one #t #:extends Thing))
(define UnspecialThing (make-sig 'UnspecialThing #:extends Thing))

(test oneExtendActuallyExtends
      #:preds [(in SpecialThing Thing)]
      #:expect theorem)
(test oneExtendEnforced
      #:preds [(= (card SpecialThing) (int 1))]
      #:expect theorem)
(test oneExtendDoesntSpread
      #:preds [(= (card UnspecialThing) (int 2))] 
      #:expect sat)
