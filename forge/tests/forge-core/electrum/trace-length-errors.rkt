#lang forge/core

(require (only-in rackunit check-exn check-true))

(set-option! 'verbose 0)

(check-exn exn:fail:user?
           (lambda ()
             (set-option! 'min_tracelength 0)))

(check-exn exn:fail:user?
           (lambda ()
             (set-option! 'max_tracelength 0)))

(check-exn exn:fail:user?
           (lambda ()
             (set-option! 'min_tracelength -9)))
(check-exn exn:fail:user?
           (lambda ()
             (set-option! 'max_tracelength -8)))

(check-exn exn:fail:user?
           (lambda ()
             (set-option! 'max_tracelength 15/2)))
(check-exn exn:fail:user?
           (lambda ()
             (set-option! 'min_tracelength 8/3)))

; min<=max is now checked when the solver runs and not at set-option! time.
(set-option! 'problem_type 'temporal)
(sig A #:is-var #t)

(set-option! 'min_tracelength 8)
(set-option! 'max_tracelength 5)
(check-exn #rx"min_tracelength \\(8\\) cannot be greater than max_tracelength \\(5\\)"
           (lambda ()
             (let ()
               (run min-above-max-run #:preds [(always (some A))])
               (void))))

(set-option! 'min_tracelength 9)
(set-option! 'max_tracelength 8)
(check-exn #rx"min_tracelength \\(9\\) cannot be greater than max_tracelength \\(8\\)"
           (lambda ()
             (let ()
               (run min-one-above-max-run #:preds [(always (some A))])
               (void))))

(set-option! 'min_tracelength 35)
(set-option! 'max_tracelength 30)
(check-exn #rx"min_tracelength \\(35\\) cannot be greater than max_tracelength \\(30\\)"
           (lambda ()
             (let ()
               (run min-far-above-max-run #:preds [(always (some A))])
               (void))))

(set-option! 'min_tracelength 8)
(set-option! 'max_tracelength 8)
(run equal-lengths-run #:preds [(always (some A))])
(check-true (is-sat? equal-lengths-run))
