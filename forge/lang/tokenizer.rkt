#lang br/quicklang
(require brag/support)
(require (rename-in br-parser-tools/lex-sre [- :-] [+ :+]))

;; reference: http://alloytools.org/download/alloy-language-reference.pdf

(define (make-tokenizer port)
  (port-count-lines! port) ; <- turn on line & column counting
  (define (next-token)
    (define forge-lexer
      (lexer

       ;; comments
       [(or (from/to "--" "\n") (from/to "//" "\n") (from/to "/*" "*/"))
        (next-token)]

       ;; reserved
       [(or "$" "%" "?")
        (token+ 'RESERVED-TOK "" lexeme "" lexeme-start lexeme-end)]
       ;; numeric
       [(: (char-range "1" "9") (* (char-range "0" "9")))
        (token+ 'NUM-CONST-TOK "" lexeme "" lexeme-start lexeme-end)]
       ;; single tokens
       [(or "=>" ">=" "<=" "=<"  "->" "<:" ":>" "++" "&&" "||")
        (token+ 'OP-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["=>" (token+ 'IMP-OP-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  [">=" (token+ 'GEQ-OP-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  [(or "<=" "=<") (token+ 'LEQ-OP-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["->" (token+ 'PROD-OP-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["<:" (token+ 'SUBT-OP-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  [":>" (token+ 'SUPT-OP-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["++" (token+ 'OVER-OP-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["&&" (token+ 'CONJ-OP-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["||" (token+ 'DISJ-OP-TOK "" lexeme "" lexeme-start lexeme-end)]
       ;; negation
       ["!" (token+ 'NEG-TOK "" lexeme "" lexeme-start lexeme-end)]
       ;; keywords
       [(or "abstract" "all" "and" "as" "assert"
            "but" "check" "disj" "else" "exactly"
            "extends" "fact" "for" "fun" "iden"
            "iff" "implies" "in" "Int" "let"
            "lone" "module" "no" "none" "not"
            "one" "open" "or" "pred" "run"
            "set" "sig" "some" "sum" "univ")
        (token+ 'KEYWORD-TOK "" lexeme "" lexeme-start lexeme-end)]

       ;; identifiers
       [(: alphabetic (* (or alphabetic numeric "_" "\'" "\"")))
        (token+ 'IDENTIFIER-TOK "" lexeme "" lexeme-start lexeme-end)]

       ;; otherwise
       [whitespace (next-token)]
       [any-char (error (format "unexpected char: ~a" lexeme))]

       ))
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

(define (token+ type left lex right lex-start lex-end)
  (let ([l0 (string-length left)] [l1 (string-length right)])
    (token type (trim-ends left lex right)
           #:position (+ (pos lex-start) l0)
           #:line (line lex-start)
           #:column (+ (col lex-start) l0)
           #:span (- (pos lex-end)
                     (pos lex-start) l0 l1))
    )
  )