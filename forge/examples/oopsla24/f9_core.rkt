#lang forge/core

(set-option! 'problem_type 'temporal)

(sig Counter)
(relation value (Counter Int) #:is-var "var")
(pred someTrace (and
(= (join Counter value) (int 0)))
(always (= (join Counter (prime value))
(add (join Counter value) (int 1)))))
(run someTrace_run #:preds [someTrace])
(display someTrace_run)