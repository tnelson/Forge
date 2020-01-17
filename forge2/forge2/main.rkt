#lang br/quicklang

(require br/indent)

(module reader racket
  (require "reader.rkt")
  (provide read-syntax get-info)
  (define (get-info port src-mod src-line src-col src-pos)
    (define (handle-query key default)
      (case key
        [(drracket:indentation)
         (dynamic-require 'forge2/indenter 'indent-forge)]
        [(drracket:keystrokes)
         (list (list "}"
                     (lambda (text event)
                       (send text insert #\} (send text get-start-position))
                       (send text tabify))))]
        [else default]))
    handle-query))
