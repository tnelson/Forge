#lang racket/base

(module reader racket
  (provide read-syntax)
  (require "lang/reader.rkt")
  (provide get-info)
  (require (only-in (submod forge/main reader)
                    get-info)))
