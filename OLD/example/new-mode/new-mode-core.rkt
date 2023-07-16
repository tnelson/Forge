#lang forge/new-mode/core

(set-verbosity 10)

(sig Box)
(sig Contents)
(sig Init-Contents #:one #:extends Contents)
(sig Term-Contents #:one #:extends Contents)

(relation contains (Box Contents) #:is func)

(display-transition-run Box
                        (lambda (pre post)
                          (!= (join pre contains) (join post contains)))
                        (lambda (box)
                          (= (join box contains) Init-Contents))
                        (lambda (box)
                          (= (join box contains) Term-Contents)))

(run my-run)
(display my-run)
