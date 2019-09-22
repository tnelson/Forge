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
       
       [(or "!" "not") 
        (token+ 'NEG-TOK "" lexeme "" lexeme-start lexeme-end)]
       [(or "none" "univ" "iden")
        (token+ 'CONST-TOK "" lexeme "" lexeme-start lexeme-end)]
       [(or "!" "not" "no" "mult" "set" "#" "~" "*" "^")
        (token+ 'UN-OP-TOK "" lexeme "" lexeme-start lexeme-end)]
       [(or "||" "or" "&&" "and" "<=>" "iff" "=>" "implies" "&" "+" "-" "++" "<:" ":>" ".")
        (token+ 'BIN-OP-TOK "" lexeme "" lexeme-start lexeme-end)]
       [(or "in" "=" "<" ">" "=<" ">=")
        (token+ 'COMP-OP-TOK "" lexeme "" lexeme-start lexeme-end)]
       [(or "all" "no" "sum")
        (token+ 'QUANT-TOK "" lexeme "" lexeme-start lexeme-end)]
       [(or "lone" "some" "one")
        (token+ 'MULT-TOK "" lexeme "" lexeme-start lexeme-end)]
       

       ;; single tokens
      ;  [(or "=>" ">=" "<=" "=<"  "->" "<:" ":>" "++" "&&" "||" "<=>" ".")
      ;   (token+ 'OP-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["=>" (token+ 'IMP-OP-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  [">=" (token+ 'GEQ-OP-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  [(or "<=" "=<") (token+ 'LEQ-OP-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["->" (token+ 'PROD-OP-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["<:" (token+ 'SUBT-OP-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  [":>" (token+ 'SUPT-OP-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["++" (token+ 'OVER-OP-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["&&" (token+ 'CONJ-OP-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["||" (token+ 'DISJ-OP-TOK "" lexeme "" lexeme-start lexeme-end)]
       
       ;; keywords
      ;  ["abstract"  (token+ `ABSTRACT-TOK "" lexeme "" lexeme-start lexeme-end)]      
      ;  ["all"       (token+ `ALL-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["and"       (token+ `AND-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["as"        (token+ `AS-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["assert"    (token+ `ASSERT-TOK "" lexeme "" lexeme-start lexeme-end)]    
      ;  ["but"       (token+ `BUT-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["check"     (token+ `CHECK-TOK "" lexeme "" lexeme-start lexeme-end)]  
      ;  ["disj"      (token+ `DISJ-TOK "" lexeme "" lexeme-start lexeme-end)]  
      ;  ["else"      (token+ `ELSE-TOK "" lexeme "" lexeme-start lexeme-end)]  
      ;  ["exactly"   (token+ `EXACTLY-TOK "" lexeme "" lexeme-start lexeme-end)]    
      ;  ["extends"   (token+ `EXTENDS-TOK "" lexeme "" lexeme-start lexeme-end)]    
      ;  ["fact"      (token+ `FACT-TOK "" lexeme "" lexeme-start lexeme-end)]  
      ;  ["for"       (token+ `FOR-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["fun"       (token+ `FUN-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["iden"      (token+ `IDEN-TOK "" lexeme "" lexeme-start lexeme-end)]  
      ;  ["iff"       (token+ `IFF-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["implies"   (token+ `IMPLIES-TOK "" lexeme "" lexeme-start lexeme-end)]    
      ;  ["in"        (token+ `IN-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["Int"       (token+ `INT-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["let"       (token+ `LET-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["lone"      (token+ `LONE-TOK "" lexeme "" lexeme-start lexeme-end)]  
      ;  ["module"    (token+ `MODULE-TOK "" lexeme "" lexeme-start lexeme-end)]    
      ;  ["no"        (token+ `NO-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["none"      (token+ `NONE-TOK "" lexeme "" lexeme-start lexeme-end)]  
      ;  ["not"       (token+ `NOT-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["one"       (token+ `ONE-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["open"      (token+ `OPEN-TOK "" lexeme "" lexeme-start lexeme-end)]  
      ;  ["or"        (token+ `OR-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["pred"      (token+ `PRED-TOK "" lexeme "" lexeme-start lexeme-end)]  
      ;  ["run"       (token+ `RUN-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["set"       (token+ `SET-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["sig"       (token+ `SIG-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["some"      (token+ `SOME-TOK "" lexeme "" lexeme-start lexeme-end)]  
      ;  ["sum"       (token+ `SUM-TOK "" lexeme "" lexeme-start lexeme-end)]
      ;  ["univ"      (token+ `UNIV-TOK "" lexeme "" lexeme-start lexeme-end)]  
       [(or "abstract" "all" "and" "as" "assert"
            "but" "check" "disj" "else" "exactly"
            "extends" "fact" "for" "fun" "iden"
            "iff" "implies" "in" "Int" "let"
            "lone" "module" "no" "none" "not"
            "one" "open" "or" "pred" "run"
            "set" "sig" "some" "sum" "univ")
        (token+ 'KEYWORD-TOK "" lexeme "" lexeme-start lexeme-end)]

       ;; punctuation
       [(or "(" ")" "{" "}" "[" "]" "," ";" "/" ":")
        (token+ 'PUNCTUATION-TOK "" lexeme "" lexeme-start lexeme-end)]

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