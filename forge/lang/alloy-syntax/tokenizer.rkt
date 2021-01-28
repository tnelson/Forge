#lang racket/base

(require racket/string)
(require brag/support)
(require (rename-in br-parser-tools/lex-sre [- :-] [+ :+]) "lexer.rkt")

;; reference: http://alloytools.org/download/alloy-language-reference.pdf

(define (make-tokenizer port)
  (port-count-lines! port) ; <- turn on line & column counting
  (define (next-token)
    (forge-lexer port)) 
  next-token)
(provide make-tokenizer)

(define str-lexer
  (lexer
   [(:- any-char "\\" "\n" "\"") lexeme]
   [(: "\\" (:- any-char "n" "t")) (trim-ends "\\" lexeme "")]
   ["\\t" "\t"]
   ["\\n" "\n"]
   [any-char (error 'absurd)]  ;; unreachable when passed proper string contents
   )
  )
(define (clean str)
  (string-join (apply-lexer str-lexer str) "")
  )