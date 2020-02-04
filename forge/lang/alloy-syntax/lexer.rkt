#lang br/quicklang
(require brag/support)
(require (rename-in br-parser-tools/lex-sre [- :-] [+ :+]))

(define forge-lexer
  (lexer
   ;; sexprs
   [(from/to "<$>" "</$>")
    (token+ 'SEXPR-TOK "<$>" lexeme "</$>" lexeme-start lexeme-end)]
   [(from/to "--$" "\n")
    (token+ 'SEXPR-TOK "--$" lexeme "\n" lexeme-start lexeme-end)]
   [(from/to "//$" "\n")
    (token+ 'SEXPR-TOK "//$" lexeme "\n" lexeme-start lexeme-end)]
   [(from/to "/*$" "*/")
    (token+ 'SEXPR-TOK "/*$" lexeme "*/" lexeme-start lexeme-end)]

   ;; instances
   [(from/to "<instance" "</instance>")
    (token+ 'INSTANCE-TOK "" lexeme "" lexeme-start lexeme-end)]
   [(from/to "<alloy" "</alloy>")
    (token+ 'INSTANCE-TOK "" lexeme "" lexeme-start lexeme-end)]
   [(from/to "<sig" "</sig>")
    (token+ 'INSTANCE-TOK "" lexeme "" lexeme-start lexeme-end)]
   [(from/to "<field" "</field>")
    (token+ 'INSTANCE-TOK "" lexeme "" lexeme-start lexeme-end)]
        
   ;; comments
   [(or (from/stop-before "--" "\n") (from/stop-before "//" "\n") (from/to "/*" "*/"))
    (token+ 'COMMENT "" lexeme "" lexeme-start lexeme-end #t)]

   ;; reserved
   [(or "$" "%" "?")
    (token+ 'RESERVED-TOK "" lexeme "" lexeme-start lexeme-end)]
   ;; numeric
   [(or "0" (: (char-range "1" "9") (* (char-range "0" "9"))))
    (token+ 'NUM-CONST-TOK "" lexeme "" lexeme-start lexeme-end)]

   ["->" (token+ 'ARROW-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["." (token+ 'DOT-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["=" (token+ 'EQ-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["==" (token+ 'EQUIV-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["|" (token+ 'BAR-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["-" (token+ 'MINUS-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["#" (token+ 'HASH-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["~" (token+ 'TILDE-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["*" (token+ 'STAR-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["^" (token+ 'EXP-TOK "" lexeme "" lexeme-start lexeme-end)]
   [">=" (token+ 'GEQ-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["<:" (token+ 'SUBT-TOK "" lexeme "" lexeme-start lexeme-end)]
   [":>" (token+ 'SUPT-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["++" (token+ 'PPLUS-TOK "" lexeme "" lexeme-start lexeme-end)]       
   ["<" (token+ 'LT-TOK "" lexeme "" lexeme-start lexeme-end)]
   [">" (token+ 'GT-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["&" (token+ 'AMP-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["+" (token+ 'PLUS-TOK "" lexeme "" lexeme-start lexeme-end)]
   [(or "<=" "=<") (token+ 'LEQ-TOK "" lexeme "" lexeme-start lexeme-end)]
   [(or "!" "not") (token+ 'NEG-TOK "" lexeme "" lexeme-start lexeme-end)]
   [(or "||" "or") (token+ 'OR-TOK "" lexeme "" lexeme-start lexeme-end)]
   [(or "&&" "and") (token+ 'AND-TOK "" lexeme "" lexeme-start lexeme-end)]
   [(or "<=>" "iff") (token+ 'IFF-TOK "" lexeme "" lexeme-start lexeme-end)]
   [(or "=>" "implies") (token+ 'IMP-TOK "" lexeme "" lexeme-start lexeme-end)]
              
   ;; keywords
   ["abstract"  (token+ `ABSTRACT-TOK "" lexeme "" lexeme-start lexeme-end)]      
   ["all"       (token+ `ALL-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["as"        (token+ `AS-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["assert"    (token+ `ASSERT-TOK "" lexeme "" lexeme-start lexeme-end)]    
   ["but"       (token+ `BUT-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["check"     (token+ `CHECK-TOK "" lexeme "" lexeme-start lexeme-end)]  
   ["disj"      (token+ `DISJ-TOK "" lexeme "" lexeme-start lexeme-end)]  
   ["else"      (token+ `ELSE-TOK "" lexeme "" lexeme-start lexeme-end)]  
   ["eval"      (token+ `EVAL-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["exactly"   (token+ `EXACTLY-TOK "" lexeme "" lexeme-start lexeme-end)]   
   ["expect"    (token+ `EXPECT-TOK "" lexeme "" lexeme-start lexeme-end)]    
   ["extends"   (token+ `EXTENDS-TOK "" lexeme "" lexeme-start lexeme-end)]    
   ["fact"      (token+ `FACT-TOK "" lexeme "" lexeme-start lexeme-end)]  
   ["for"       (token+ `FOR-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["fun"       (token+ `FUN-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["iden"      (token+ `IDEN-TOK "" lexeme "" lexeme-start lexeme-end)]      
   ["in"        (token+ `IN-TOK "" lexeme "" lexeme-start lexeme-end)]
   ;["Int"       (token+ `INT-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["let"       (token+ `LET-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["lone"      (token+ `LONE-TOK "" lexeme "" lexeme-start lexeme-end)]  
   ["module"    (token+ `MODULE-TOK "" lexeme "" lexeme-start lexeme-end)]    
   ["no"        (token+ `NO-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["none"      (token+ `NONE-TOK "" lexeme "" lexeme-start lexeme-end)]  
   ["one"       (token+ `ONE-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["open"      (token+ `OPEN-TOK "" lexeme "" lexeme-start lexeme-end)]  
   ["pred"      (token+ `PRED-TOK "" lexeme "" lexeme-start lexeme-end)]  
   ["run"       (token+ `RUN-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["sat"       (token+ `SAT-TOK "" lexeme "" lexeme-start lexeme-end)] 
   ["set"       (token+ `SET-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["sig"       (token+ `SIG-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["some"      (token+ `SOME-TOK "" lexeme "" lexeme-start lexeme-end)]  
   ["sum"       (token+ `SUM-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["test"      (token+ `TEST-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["univ"      (token+ `UNIV-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["unsat"     (token+ `UNSAT-TOK "" lexeme "" lexeme-start lexeme-end)]  
   ["break"     (token+ `BREAK-TOK "" lexeme "" lexeme-start lexeme-end)]  

   ["state"     (token+ `STATE-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["facts"     (token+ `STATE-TOK "" lexeme "" lexeme-start lexeme-end)]  
   ["transition"(token+ `TRANSITION-TOK "" lexeme "" lexeme-start lexeme-end)]  

   ;; punctuation
   ["(" (token+ 'LEFT-PAREN-TOK "" lexeme "" lexeme-start lexeme-end)]
   [")" (token+ 'RIGHT-PAREN-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["{" (token+ 'LEFT-CURLY-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["}" (token+ 'RIGHT-CURLY-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["[" (token+ 'LEFT-SQUARE-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["]" (token+ 'RIGHT-SQUARE-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["," (token+ 'COMMA-TOK "" lexeme "" lexeme-start lexeme-end)]
   [";" (token+ 'SEMICOLON-TOK "" lexeme "" lexeme-start lexeme-end)]
   ["/" (token+ 'SLASH-TOK "" lexeme "" lexeme-start lexeme-end)]
   [":" (token+ 'COLON-TOK "" lexeme "" lexeme-start lexeme-end)]
   ;["@" (token+ 'AT-TOK "" lexeme "" lexeme-start lexeme-end)]

   ;; identifiers
   [(: (or alphabetic "@") (* (or alphabetic numeric "_" "\'" "\"")))   ;; "’" "”"
    (token+ 'IDENTIFIER-TOK "" lexeme "" lexeme-start lexeme-end)]
   [(* (char-set "➡️"))   ;; "’" "”"
    (token+ 'IDENTIFIER-TOK "" lexeme "" lexeme-start lexeme-end)]

   ;; otherwise
   [whitespace (token+ lexeme "" lexeme "" lexeme-start lexeme-end #t)]
   [any-char (error (format "unexpected char: ~a" lexeme))]))

(define (keyword? str)
  (member str
          (list
           "abstract"
           "all"
           "and"
           "as"
           "assert"
           "but"
           "check"
           "disj"
           "else"
           "exactly"
           "extends"
           "fact"
           "for"
           "fun"
           "iden"
           "iff"
           "implies"
           "in"
           "Int"
           "let"
           "lone"
           "module"
           "no"
           "none"
           "not"
           "one"
           "open"
           "or"
           "pred"
           "run"
           "set"
           "sig"
           "some"
           "sum"
           "test"
           "expect"
           "sat"
           "unsat"
           "univ"
           "break")))

(define (paren? str)
  (member str
          (list "("
                ")"
                "{"
                "}"
                "["
                "]")))

(define (token+ type left lex right lex-start lex-end [skip? #f])
  (let ([l0 (string-length left)] [l1 (string-length right)])
    (token type (trim-ends left lex right)
           #:position (+ (pos lex-start) l0)
           #:line (line lex-start)
           #:column (+ (col lex-start) l0)
           #:span (- (pos lex-end)
                     (pos lex-start) l0 l1)
           #:skip? skip?)))

(provide forge-lexer keyword? paren?)