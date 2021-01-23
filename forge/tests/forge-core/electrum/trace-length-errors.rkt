#lang forge/core

(require (only-in rackunit check-exn))

(set-option! 'verbose 0)

;TODO - tests for errors where tracelength is set to negative or fraction

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