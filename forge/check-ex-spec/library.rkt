#lang racket

(require "library/commands.rkt")
(provide (all-from-out "library/commands.rkt"))

(require "library/student.rkt"
         "library/ta.rkt")

(provide (prefix-out check-ex-spec: (all-from-out "library/student.rkt"))
         (prefix-out check-ex-spec: (all-from-out "library/ta.rkt")))