#lang forge/core

(require "make-b-sig.rkt")
(sig A)

(run my-run #:scope ([A 1 2] [B 2 2]))
(display my-run)