#lang br
(require brag/support syntax-color/racket-lexer "lexer.rkt")

(define (color-forge port)
  ; tbh, this handler might not work
  (define (handle-lexer-error excn)
    (match-define (srcloc val line col posn span) (car excn))
    (token 'ERROR val line col posn span #f))
  (define tok
    (with-handlers ([exn:fail:read? handle-lexer-error])
      (forge-lexer port)))
  (match tok
    [(? eof-object?) (values tok 'eof #f #f #f)]
    [else
     (match-define (token-struct type val posn _ _ span _) tok)
     (define start posn)
     (define end (+ start span))
     (match-define (list cat paren)
       (match type
         ['COMMENT '(comment #f)]
         ['ERROR '(error #f)]
         ['NUM-CONST-TOK '(constant #f)]
         [else
          (match val
            [(? keyword?) '(hash-colon-keyword #f)]
            [(? paren?) '(parenthesis (string->symbol val))]
            [else '(symbol #f)])]))
     (values val cat paren start end)]))

(provide color-forge)