#lang forge/core

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; WITH THE WAY FUNCTIONAL FORGE SETS OPTIONS
; THESE TESTS ARE NOW MEANINGLESS
; TODO: DECIDE WHEN TO THROW THESE ERRORS / IF THEY ARE STILL NEEDED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(require (only-in rackunit check-exn))

(set-option! 'verbose 0)

;; NOT TEMPORAL YET

;cannot define var sig
(check-exn exn:fail:user?
           (lambda ()
             (sig Node #:is-var "var")))

;can define not-var sig
(sig Apple)
(sig Pear)

;cannot define var relation
(check-exn exn:fail:user?
           (lambda ()
             (relation appleToPear (Apple Pear) #:is-var "var")))

;can define not-var relation
(relation appleToApple (Apple Apple))
(relation pearToPear (Pear Pear))
(relation pearToApple (Pear Apple))
(relation appleToPear (Apple Pear))

(pred applesAndPears
      (= appleToPear
         (~ pearToApple)))

(test useNonVarStuff
      #:preds [applesAndPears]
      #:expect sat)

#|
TODO: see if this can be tested
it throws unbound-id error
WHICH IS GOOD but not really testable afaik cuz it's raised before run
(check-exn exn:fail:user?
           (lambda ()
             (test useVarStuff
                   #:preds [(in Node Node)]
                   #:expect sat)))
|#

;; FROM NOW ON, TEMPORAL
(set-option! 'problem_type 'temporal)

;can still define non-var sig
(sig Doggo)
(sig Cat)

;can now define var sig
(sig Fluffy #:is-var "var")
(sig Slithery #:is-var "var")

;can still define non-var relation
(relation meow (Cat Cat))
(relation friend (Doggo Cat))
(relation swoop (Fluffy Cat))
(relation bowow (Doggo Slithery))

;can now define non-var relation
(relation woof (Doggo Doggo) #:is-var "var")
(relation think (Cat Doggo) #:is-var "var")
(relation leap (Cat Slithery) #:is-var "var")
(relation dig (Fluffy Doggo) #:is-var "var")

(pred fluffyCat
      (= (join dig friend)
         (~ swoop)))

(test usingVarAndNonVarStuff
      #:preds [fluffyCat]
      #:expect sat)
|#
