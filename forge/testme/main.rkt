#lang racket/base

(module reader racket
  (require forge/lang/reader)
  (provide read-syntax)
  
  (require (only-in (submod forge/main reader)
                    get-info))
  (provide get-info))
