#lang setup/infotab
(define collection 'multi)
(define deps '(
	       ;; these are what the package system tells me I need:
	       "base"
	       "net-lib"
	       "rackunit-lib"
	       "web-server-lib"
	       ))
(define build-deps '(
		     ;; these are what the package system tells me I need:
		     "scribble-lib"
		     "net-doc"
		     "racket-doc"
		     "web-server-doc"
		     ))
