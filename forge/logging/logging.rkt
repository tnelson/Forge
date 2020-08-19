#lang racket

(require forge/data-structures)
(require json)

(provide set-logging-mode)


(define logging-mode 'none)

(define modes '(cs1950y
                check-ex-spec
                forge/core))

(define (set-logging-mode value)
  (unless (member value modes)
    (raise (format "Invalid mode ~a; expected one of ~a." value modes))))


(define ())


(define (log-run run)
  )