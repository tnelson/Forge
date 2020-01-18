#lang br/quicklang

(require br/indent)

(module reader racket
  (provide read-syntax)
  (require "lang/reader.rkt")
  (provide get-info)
  (define (get-info port src-mod src-line src-col src-pos)
    (define (handle-query key default)
      (case key
        [(color-lexer)
         (dynamic-require 'forge/lang/alloy-syntax/colorer 'color-forge)]

        [(drracket:indentation)
         (dynamic-require 'forge/lang/alloy-syntax/indenter 'indent-forge)]
        [(drracket:keystrokes)
         (list 
               (list "}"
                     (lambda (text event)
                       (send text insert #\} (send text get-start-position))
                       (send text tabify)))
               (list "c:/"
                     (lambda (text event)
                       (define start-pos (send text get-start-position))
                       (define end-pos (send text get-end-position))
                       (define first-line (send text position-line start-pos))
                       (define first-line-start-pos (send text line-start-position first-line))
                       (define first-line-prefix
                         (string-join (map (λ (pos) (string (send text get-character pos)))
                                           (range first-line-start-pos (+ first-line-start-pos 2)))
                                      ""))
                       (cond
                         ; uncomment
                         [(or (equal? (substring first-line-prefix 0 2) "--")
                              (equal? (substring first-line-prefix 0 2) "//"))
                          (send text delete first-line-start-pos (+ first-line-start-pos 2))]
                         ; comment
                         [else (send text insert "--" (send text line-start-position first-line))]))))]
        [else default]))
    handle-query))

(require "forge.rkt")

(provide (all-from-out "forge.rkt"))