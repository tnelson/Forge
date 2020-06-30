#lang br/quicklang


(module reader racket
  (provide read-syntax)
  (require "lang/reader.rkt"))

(require "forge-core.rkt")

(provide (all-from-out "forge-core.rkt"))