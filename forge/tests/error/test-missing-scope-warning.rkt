#lang racket/base

; In some situations, Forge will issue a missing-scope warning. 

(require "warning-helpers.rkt")

(module+ test
  (check-silent "missing-scope-warning-none-helper.frg" #rx"No scope was given"))
