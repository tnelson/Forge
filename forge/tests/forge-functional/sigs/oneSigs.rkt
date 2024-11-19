#lang forge/core

(set-option! 'verbose 0)

(define UniqueObject (make-sig 'UniqueObject #:one #t))

(define Stuff (make-sig 'Stuff))

(make-test #:name 'oneSigEnforced
           #:preds (list (= (card UniqueObject) (int 1)))
           #:sigs (list UniqueObject Stuff)
           #:expect 'checked)
(make-test #:name 'oneSigIsntPersistent
           #:preds (list (= (card Stuff) (int 2)))
           #:sigs (list UniqueObject Stuff)
           #:expect 'sat)

(define Thing (make-sig 'Thing))
(define SpecialThing (make-sig 'SpecialThing #:one #t #:extends Thing))
(define UnspecialThing (make-sig 'UnspecialThing #:extends Thing))

(make-test #:name 'oneExtendActuallyExtends
           #:preds (list (in SpecialThing Thing))
           #:sigs (list UniqueObject Stuff Thing SpecialThing UnspecialThing)
           #:expect 'checked)
(make-test #:name 'oneExtendEnforced
           #:preds (list (= (card SpecialThing) (int 1)))
           #:sigs (list UniqueObject Stuff Thing SpecialThing UnspecialThing)
           #:expect 'checked)
(make-test #:name 'oneExtendDoesntSpread
           #:preds (list (= (card UnspecialThing) (int 2)))
           #:sigs (list UniqueObject Stuff Thing SpecialThing UnspecialThing)
           #:expect 'sat)
