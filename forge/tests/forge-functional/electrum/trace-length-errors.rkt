#lang forge/core

option run_sterling off
; THANKS TO FUNCTIONAL FORGE, THERE ARE SEVERELY CLEANER WAYS TO TEST THIS
; TODO: WRITE THOSE

#|
(require (only-in rackunit check-exn))

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

(check-exn exn:fail:user?
           (lambda ()
             (set-option! 'min_tracelength 8)))

(set-option! 'max_tracelength 8)
(set-option! 'min_tracelength 8)

(check-exn exn:fail:user?
           (lambda ()
             (set-option! 'min_tracelength 9)))

(check-exn exn:fail:user?
           (lambda ()
             (set-option! 'max_tracelength 4)))

(set-option! 'max_tracelength 20)
(set-option! 'max_tracelength 30)
(check-exn exn:fail:user?
           (lambda ()
             (set-option! 'min_tracelength 35)))
(set-option! 'min_tracelength 25)
(set-option! 'max_tracelength 25)
(set-option! 'min_tracelength 14)
(set-option! 'max_tracelength 40)
|#
