#lang racket/base

; At verbose 0, the "Test does not reference" warning is silent.

(require "warning-helpers.rkt")

(module+ test
  (check-silent "unreferenced-pred-verbose0-helper.frg" #rx"Test does not reference"))
