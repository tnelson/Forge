#lang forge/core

(require (only-in rackunit check-exn))

(sig Parent #:abstract)
(sig P1 #:extends Parent #:one)
(sig P2 #:extends Parent #:one)
(sig P3 #:extends Parent #:one)

(sig Family #:abstract)
(sig F1 #:extends Family #:one)
(sig F2 #:extends Family #:one)
(sig F3 #:extends Family #:one)

(relation fp (Family Parent))
(relation ffp (Family Family Parent))
(relation ffpfp (Family Family Parent Family Parent))

(pred overArity2
	  (in (-> F1 P1) (++ fp (-> F1 P1)))
	  (! (in (-> F1 P2) (++ fp (-> F1 P1))))
	  (! (in (-> F1 P3) (++ fp (-> F1 P1))))
	  (one (join F1 (++ fp (-> F1 P1)))))

(test arity2
	  #:preds [overArity2]
	  #:expect checked)

(pred overArity3
	  (in (-> F2 (-> F2 P2)) (++ ffp (-> F2 (-> F2 P2))))
	  (one (join F2 (++ ffp (-> F2 (-> F2 P2))))))

(test arity3
	  #:preds [overArity3]
	  #:expect checked)

; Hopefully this test would fail if there were ever any issues
; with big arities larger than 3
(pred overArity5
	  (in (-> F3 (-> F1 (-> P3 (-> F2 P1))))
	  	  (++ ffpfp (-> F3 (-> F1 (-> P3 (-> F2 P1))))))
	  (one (join F3 (++ ffpfp (-> F3 (-> F1 (-> P3 (-> F2 P1))))))))

(test arity5
	  #:preds [overArity5]
	  #:expect checked)

(pred overEntireRelation
	  (in (-> F2 fp) (++ ffp (-> F2 fp)))
	  (implies (some fp) (= fp (join F2 (++ ffp (-> F2 fp))))))

(test overrideWithEntireRelation
	  #:preds [overEntireRelation]
	  #:expect checked)

; left arity bigger than right arity
(check-exn exn:fail?
	       (lambda ()
	       	 (++ ffpfp (-> F2 (-> F1 P3)))))

; left arity smaller than right arity
(check-exn exn:fail?
	       (lambda ()
	       	 (++ fp (-> F3 (-> F2 P1)))))

; Can't use ++ on sigs
(check-exn exn:fail?
	       (lambda ()
	       	 (run overSigs #:preds [(some (++ F1 F2))])))

; Can't use ++ on anything of arity 1
(check-exn exn:fail?
	       (lambda ()
	       	 (run overArity1 #:preds [(some (++ (join F3 fp) P3))])))

; Can't use ++ unless both arguments have the same types
(check-exn exn:fail?
	       (lambda ()
	       	 (run differentArgTypes #:preds [(some (++ ffp (-> F2 (-> P2 F2))))])))
