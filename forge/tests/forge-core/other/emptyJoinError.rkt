#lang forge/core

option run_sterling off


(require (only-in rackunit check-exn))

(set-option! 'verbose 2) ; needed in order to do last-checking now
(sig Person)
(sig Marker)
(sig Node)
(sig Color)
(sig CoastalTown #:extends Node)

(relation markers (Person Marker))
(relation city (Marker Node))
(relation edges (Marker Node))
(relation colorOf (Node Color))

(test testOK
      #:preds [(some (join Marker (+ markers city colorOf)))]
      #:expect sat)

(check-exn exn:fail?
           (lambda () 
             (test testEmptyJoin
                   #:preds [(no (join city markers))]
                   #:expect sat)))
           
(check-exn exn:fail?
           (lambda () 
             (test testEmptyJoin
                   #:preds [(no (join Color markers))]
                   #:expect sat)))
