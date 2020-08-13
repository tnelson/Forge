#lang forge/core

(sig UniqueObject #:one)

(sig Stuff)

(test oneSigEnforced
      #:preds [(= (card UniqueObject) (node/int/constant 1))]
      #:expect theorem)
(test oneSigIsntPersistent
      #:preds [(= (card Stuff) (node/int/constant 2))]
      #:expect sat)


(sig Thing)
(sig SpecialThing #:one #:extends Thing)
(sig UnspecialThing #:extends Thing)

(test oneExtendActuallyExtends
      #:preds [(in SpecialThing Thing)]
      #:expect theorem)
(test oneExtendEnforced
      #:preds [(= (card SpecialThing) (node/int/constant 1))]
      #:expect theorem)
(test oneExtendDoesntSpread
      #:preds [(= (card UnspecialThing) (node/int/constant 2))] 
      #:expect sat)