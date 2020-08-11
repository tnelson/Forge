#lang forge/core

(set-verbosity 10)

(sig A)
(sig A1 #:extends A)

(run my-run)
(display my-run)
