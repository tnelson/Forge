#lang forge/core

(require (only-in rackunit check-exn))

(set-option! 'verbose 2) ; needed in order to do last-checking now

(define Person (make-sig 'Person))
(define Marker (make-sig 'Marker))
(define Node (make-sig 'Node))
(define Color (make-sig 'Color))
(define CoastalTown (make-sig 'CoastalTown #:extends Node))

(define markers (make-relation 'markers (list Person Marker)))
(define city (make-relation 'city (list Marker Node)))
(define edges (make-relation 'edges (list Marker Node)))
(define colorOf (make-relation 'colorOf (list Node Color)))

(make-test #:name 'testOK
           #:preds (list (some (join Marker (+ markers city colorOf))))
           #:sigs (list Person Marker Node Color CoastalTown)
           #:relations (list markers city edges colorOf)
           #:expect 'sat)

(check-exn exn:fail?
           (lambda () 
             (make-test #:name 'testEmptyJoin
                        #:preds (list (no (join city markers)))
                        #:sigs (list Person Marker Node Color CoastalTown)
                        #:relations (list markers city edges colorOf)
                        #:expect 'sat)))
           
(check-exn exn:fail?
           (lambda () 
             (make-test #:name 'testEmptyJoin
                        #:preds (list (no (join Color markers)))
                        #:sigs (list Person Marker Node Color CoastalTown)
                        #:relations (list markers city edges colorOf)
                        #:expect 'sat)))
