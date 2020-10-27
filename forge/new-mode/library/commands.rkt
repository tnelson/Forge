#lang racket

(require forge/sigs)
(require syntax/parse/define)

#|
(provide display-transition-run)
(define-simple-macro (display-transition-run state:id transition-pred init-pred term-pred)
  (begin
    (define save-state curr-state)
    (relation transition (state state))
    (run transition-run #:preds [(init-pred (- state (join state transition)))
                                 (all ([pre state] [post (join pre transition)])
                                   (transition-pred pre post))
                                 (term-pred (- state (join transition state)))]
                        #:bounds [(is transition linear)])
    (display transition-run)
    (update-state! save-state)))
|#
