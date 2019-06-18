#lang rosette
(require ocelot)

(define U (universe '(a b c d)))
(universe-atoms U)


(define File (declare-relation 1 "File"))
(define Dir (declare-relation 1 "Dir"))
(define contents (declare-relation 2 "contents"))

(define DirsAndFiles (+ File Dir))
(define nonEmptyDir (some (join Dir contents)))
(define acyclic (no ([d Dir]) (in d (join d (^ contents)))))

(define bDir (make-exact-bound Dir '((a))))
(define bFile (make-bound File '((b)) '((b) (c) (d))))
(define bContents (make-product-bound contents '(a b c d) '(a b c d)))
(define allBounds (bounds U (list bDir bFile bContents)))

(define formula1 (interpret nonEmptyDir allBounds))
(define result1 (solve (assert formula1)))
(sat? result1)

; There is a counterexample to acyclicity
(define formula2 (interpret acyclic allBounds))
(define result2 (verify (assert formula2)))
(sat? result2)


; Failpoints: integers and the big graph.