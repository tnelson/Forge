#lang br/quicklang

(module reader racket
  (require "reader.rkt")
  (provide read-syntax get-info)
  (define (get-info port src-mod src-line src-col src-pos)
    (define (handle-query key default)
      (case key
        [(drracket:indentation)
         (dynamic-require 'forge2/indenter 'indent-forge)]
        [else default]))
    handle-query))
