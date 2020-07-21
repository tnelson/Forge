#lang forge/core

(sig UniqueObject #:one)

(sig Stuff)

(check oneSigEnforced
       #:preds [(= (card UniqueObject) (node/int/constant 1))])
(test oneSigIsntPersistent
      #:preds [(= (card Stuff) (node/int/constant 2))]
      sat)


(sig Thing)
(sig SpecialThing #:one #:extends Thing)
(sig UnspecialThing #:extends Thing)

(check oneExtendActuallyExtends
       #:preds [(in SpecialThing Thing)])
(check oneExtendEnforced
       #:preds [(= (card SpecialThing) (node/int/constant 1))])
(test oneExtendDoesntSpread
      #:preds [(= (card UnspecialThing) (node/int/constant 2))] 
      sat)