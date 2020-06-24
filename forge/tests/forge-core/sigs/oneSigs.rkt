#lang racket

(require "../../../sigs.rkt")

(sig UniqueObject #:one)

(sig Stuff)

(check oneSigEnforced [(= (card UniqueObject) (node/int 1))])
(test oneSigIsntPersistent [(= (card Stuff) (node/int 2))] 'sat)


(sig Thing)
(sig SpecialThing #:one #:extends Thing)
(sig UnspecialThing #:extends Thing)

(check oneExtendActuallyExtends [(in SpecialThing Thing)])
(check oneExtendEnforced [(= (card SpecialThing) (node/int 1))])
(test oneExtendDoesntSpread [(= (card UnspecialThing) (node/int 2))] 'sat)