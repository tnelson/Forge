#lang racket/base

(module reader racket
  (require "lang/reader.rkt")
  (provide read-syntax)
  
  (require (only-in (submod forge/main reader)
                    get-info))
  (provide get-info))
