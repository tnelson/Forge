#lang brag

;; reference: http://alloytools.org/download/alloy-language-reference.pdf

AlloyModule : ModuleDecl? Import* Paragraph*
ModuleDecl : "module" QualName (LEFT-SQUARE-TOK NameList RIGHT-SQUARE-TOK)?
           | Sexpr
Import : "open" QualName (LEFT-SQUARE-TOK QualNameList RIGHT-SQUARE-TOK)? ("as" Name)?
       | Sexpr
Paragraph : SigDecl 
          | FactDecl 
          | PredDecl 
          | FunDecl 
          | AssertDecl 
          | CmdDecl
          | Sexpr
SigDecl : "abstract"? Mult? "sig" NameList SigExt? LEFT-CURLY-TOK DeclList? RIGHT-CURLY-TOK Block?
SigExt : "extends" QualName 
       | "in" QualName ("+" QualName)*
Mult : LONE-TOK | SOME-TOK | ONE-TOK
Decl : "disj"? NameList COLON-TOK "disj"? Expr
FactDecl : "fact" Name? Block
PredDecl : "pred" (QualName PERIOD-TOK)? Name ParaDecls? Block
FunDecl : "fun" (QualName PERIOD-TOK)? Name ParaDecls? COLON-TOK Expr LEFT-CURLY-TOK Expr RIGHT-CURLY-TOK
ParaDecls : LEFT-PAREN-TOK DeclList? RIGHT-PAREN-TOK 
          | LEFT-SQUARE-TOK DeclList? RIGHT-SQUARE-TOK
AssertDecl : "assert" Name? Block
CmdDecl : (Name COLON-TOK)? ("run" | "check")? (QualName | Block)? Scope?
Scope : "for" Number ("but" TypescopeList)? 
      | "for" TypescopeList
Typescope : "exactly"? Number QualName
Const : NONE-TOK | UNIV-TOK | IDEN-TOK
      | MINUS-TOK? Number 
UnOp : Mult
     | NEG-TOK | NO-TOK | SET-TOK | HASH-TOK | TILDE-TOK | STAR-TOK | EXP-TOK
BinOp : DISJ-TOK | CONJ-TOK | IFF-TOK | IMPLIES-TOK | AMP-TOK 
      | PLUS-TOK | MINUS-TOK | OVER-TOK | SUBT-TOK | SUPT-TOK | DOT-TOK
ArrowOp : (Mult | "set")? ARROW-TOK (Mult | "set")?
CompareOp : COMP-OP-TOK
LetDecl : Name EQ-TOK Expr
Block : LEFT-CURLY-TOK Expr* RIGHT-CURLY-TOK
BlockOrBar : Block | BAR-TOK Expr 
Quant : ALL-TOK | NO-TOK | SUM-TOK 
      | Mult
QualName : ("this" SLASH-TOK)? (Name SLASH-TOK)* Name


;; this mess is the only way I know of to do operator precedence well in brag
;; imagine a coin rolling over a coin sorter and falling into the first slot that fits
Expr    : Expr1  
        | "let" LetDeclList BlockOrBar
        | Quant DeclList BlockOrBar
@Expr1  : Expr2  | Expr1 ("||" | "or") Expr2 
@Expr2  : Expr3  | Expr2 ("<=>" | "iff") Expr3
@Expr3  : Expr4  | Expr4 ("=>" | "implies") Expr3 ("else" Expr3)?          ;; right assoc
@Expr4  : Expr5  | Expr4 ("&&" | "and") Expr5
@Expr5  : Expr6  | ("!" | "not") Expr5
@Expr6  : Expr7  | Expr6 ("!" | "not")? CompareOp Expr7
@Expr7  : Expr8  | Expr7 ("+" | MINUS-TOK) Expr8
@Expr8  : Expr9  | "#" Expr8
@Expr9  : Expr10 | Expr9 "++" Expr10
@Expr10 : Expr11 | Expr10 "&" Expr11
@Expr11 : Expr12 | Expr11 ArrowOp Expr12
@Expr12 : Expr13 | Expr12 ("<:" | ":>") Expr13
@Expr13 : Expr14 | Expr13 LEFT-SQUARE-TOK ExprList RIGHT-SQUARE-TOK
ExprList : Expr13
         | Expr13 COMMA-TOK ExprList
@Expr14 : Expr15 | Expr14 PERIOD-TOK Expr15
@Expr15 : Expr16 | ("~" | "^" | "*") Expr15
@Expr16 : Const 
        | QualName 
        | "@" Name 
        | "this"
        | LEFT-CURLY-TOK DeclList BlockOrBar RIGHT-CURLY-TOK
        | LEFT-PAREN-TOK Expr RIGHT-PAREN-TOK 
        | Block

;;;;;;;;

Name : IDENTIFIER-TOK
Number : NUM-CONST-TOK
NameList : Name
         | Name COMMA-TOK NameList
QualNameList : QualName
             | QualName COMMA-TOK QualNameList
DeclList : Decl
         | Decl COMMA-TOK DeclList
LetDeclList : LetDecl
            | LetDecl COMMA-TOK LetDeclList
TypescopeList : Typescope
              | Typescope COMMA-TOK TypescopeList


Sexpr : LEFT-PAREN-TOK Sexpr* RIGHT-PAREN-TOK
      | Name
      | Const
      | UnOp
      | BinOp
      | CompareOp
      | Quant
      | KEYWORD-TOK

