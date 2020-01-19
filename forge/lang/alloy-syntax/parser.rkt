#lang brag

;; reference: http://alloytools.org/download/alloy-language-reference.pdf

AlloyModule : ModuleDecl? Import* Paragraph*
ModuleDecl : /MODULE-TOK QualName (LEFT-SQUARE-TOK NameList RIGHT-SQUARE-TOK)?
Import : OPEN-TOK QualName (LEFT-SQUARE-TOK QualNameList RIGHT-SQUARE-TOK)? (AS-TOK Name)?
@Paragraph : SigDecl 
          | FactDecl 
          | PredDecl 
          | FunDecl 
          | AssertDecl 
          | CmdDecl
          | SexprDecl
          | BreakDecl
          | InstanceDecl
          | QueryDecl
SigDecl : ABSTRACT-TOK? Mult? /SIG-TOK NameList SigExt? /LEFT-CURLY-TOK ArrowDeclList? /RIGHT-CURLY-TOK Block?
SigExt : EXTENDS-TOK QualName 
       | IN-TOK QualName (PLUS-TOK QualName)*
Mult : LONE-TOK | SOME-TOK | ONE-TOK
ArrowMult : LONE-TOK | SET-TOK | ONE-TOK
Decl : DISJ-TOK? NameList /COLON-TOK DISJ-TOK? Expr
ArrowDecl : DISJ-TOK? NameList /COLON-TOK DISJ-TOK? ArrowMult ArrowExpr
FactDecl : FACT-TOK Name? Block
PredDecl : /PRED-TOK (QualName DOT-TOK)? Name ParaDecls? Block
FunDecl : /FUN-TOK (QualName DOT-TOK)? Name ParaDecls? /COLON-TOK Expr Block
ParaDecls : /LEFT-PAREN-TOK @DeclList? /RIGHT-PAREN-TOK 
          | /LEFT-SQUARE-TOK @DeclList? /RIGHT-SQUARE-TOK
AssertDecl : /ASSERT-TOK Name? Block
CmdDecl : (Name /COLON-TOK)? (RUN-TOK | CHECK-TOK)? (QualName | Block)? Scope?
Scope : /FOR-TOK Number (/BUT-TOK @TypescopeList)? 
      | /FOR-TOK @TypescopeList
Typescope : EXACTLY-TOK? Number QualName
Const : NONE-TOK | UNIV-TOK | IDEN-TOK
      | MINUS-TOK? Number 
# UnOp : Mult
#      | NEG-TOK | NO-TOK | SET-TOK | HASH-TOK | TILDE-TOK | STAR-TOK | EXP-TOK
# BinOp : OR-TOK | AND-TOK | IFF-TOK | IMP-TOK | AMP-TOK
#       | PLUS-TOK | MINUS-TOK | PPLUS-TOK | SUBT-TOK | SUPT-TOK | DOT-TOK
ArrowOp : (@Mult | SET-TOK)? ARROW-TOK (@Mult | SET-TOK)?
        | STAR-TOK
CompareOp : IN-TOK | EQ-TOK | LT-TOK | GT-TOK | LEQ-TOK | GEQ-TOK | EQUIV-TOK
LetDecl : @Name /EQ-TOK Expr
Block : /LEFT-CURLY-TOK Expr* /RIGHT-CURLY-TOK
BlockOrBar : Block | BAR-TOK Expr 
Quant : ALL-TOK | NO-TOK | SUM-TOK 
      | @Mult
QualName : (THIS-TOK /SLASH-TOK)? (@Name /SLASH-TOK)* @Name
BreakDecl : /FACT-TOK /BREAK-TOK? Expr /COLON-TOK @NameList
          | /BREAK-TOK Expr /COLON-TOK @NameList


Name : IDENTIFIER-TOK
Number : NUM-CONST-TOK
NameList : @Name
         | @Name /COMMA-TOK @NameList
QualNameList : @QualName
             | @QualName /COMMA-TOK @QualNameList
DeclList : Decl
         | Decl /COMMA-TOK @DeclList
ArrowDeclList : ArrowDecl
              | ArrowDecl /COMMA-TOK @ArrowDeclList
LetDeclList : LetDecl
            | LetDecl /COMMA-TOK @LetDeclList
TypescopeList : Typescope
              | Typescope /COMMA-TOK @TypescopeList
ExprList : Expr
         | Expr /COMMA-TOK @ExprList

;;;;;;;;

;; this mess is the only way I know of to do operator precedence well in brag
;; goes from weakest to strongest binding
;; imagine a coin rolling over a coin sorter and falling into the first slot that fits
Expr    : @Expr1  
        | LET-TOK LetDeclList BlockOrBar
        | Quant DeclList BlockOrBar
Expr1  : @Expr2  | Expr1 OR-TOK Expr2
Expr2  : @Expr3  | Expr2 IFF-TOK Expr3
Expr3  : @Expr4  | Expr4 IMP-TOK Expr3 (ELSE-TOK Expr3)?          ;; right assoc
Expr4  : @Expr5  | Expr4 AND-TOK Expr5
Expr5  : @Expr6  | NEG-TOK Expr5
Expr6  : @Expr7  | Expr6 NEG-TOK? CompareOp Expr7
Expr7  : @Expr8  | (NO-TOK | SOME-TOK | LONE-TOK | ONE-TOK | SET-TOK) Expr8
Expr8  : @Expr9  | Expr8 (PLUS-TOK | MINUS-TOK) Expr9
Expr9  : @Expr10 | HASH-TOK Expr9
Expr10 : @Expr11 | Expr10 PPLUS-TOK Expr11
Expr11 : @Expr12 | Expr11 AMP-TOK Expr12
Expr12 : @Expr13 | Expr13 ArrowOp Expr12                          ;; right assoc
Expr13 : @Expr14 | Expr13 (SUBT-TOK | SUPT-TOK) Expr14
Expr14 : @Expr15 | Expr14 LEFT-SQUARE-TOK ExprList RIGHT-SQUARE-TOK
Expr15 : @Expr16 | Expr15 DOT-TOK Expr16
Expr16 : @Expr17 | (TILDE-TOK | EXP-TOK | STAR-TOK) Expr16
Expr17 : Const 
       | QualName 
       | AT-TOK Name 
       | THIS-TOK
       | /LEFT-CURLY-TOK DeclList BlockOrBar /RIGHT-CURLY-TOK
       | /LEFT-PAREN-TOK @Expr /RIGHT-PAREN-TOK 
       | Block
       | Sexpr

ArrowExpr : QualName
          | QualName /ARROW-TOK @ArrowExpr

;;;;;;;;

SexprDecl : Sexpr
Sexpr : SEXPR-TOK

;;;;;;;;

InstanceDecl : INSTANCE-TOK

QueryDecl : @Name /COLON-TOK ArrowExpr /EQ-TOK Expr
