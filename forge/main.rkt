#lang br/quicklang

(require br/indent)

(module reader racket
  (provide read-syntax)
  (require "lang/reader.rkt")
  (provide get-info)
  (define (get-info port src-mod src-line src-col src-pos)
    (define (handle-query key default)
      (case key
        [(drracket:opt-out-toolbar-buttons)
            '(debug-tool macro-stepper drracket:syncheck)]
        
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
                       (define (commented? line)
                         (and (>= (send text line-length line) 2)
                              (let* ([line-start-pos
                                      (send text line-start-position line)]
                                     [line-prefix
                                      (string-join (map (λ (pos) (string (send text get-character pos)))
                                           (range line-start-pos (+ line-start-pos 2)))
                                      "")])
                                (or (equal? line-prefix "--") (equal? line-prefix "//")))))
                       (define (toggle on?)
                         (for/fold ([prev-line #f]) ([pos (in-range start-pos (add1 end-pos))])
                           (define current-line (send text position-line pos))
                           (define current-line-start-pos (send text line-start-position current-line))
                           (if (equal? current-line prev-line)
                               prev-line
                               (begin (cond
                                        [(and on? (not (commented? current-line)))
                                         (send text insert "--" (send text line-start-position current-line))]
                                        [(and (not on?) (commented? current-line))
                                         (send text delete current-line-start-pos (+ current-line-start-pos 2))])
                                      current-line))))
                       (toggle (not (commented? first-line))))))]
        [else default]))
    handle-query))

(require "forge.rkt")

(provide (all-from-out "forge.rkt"))