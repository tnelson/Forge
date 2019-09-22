#lang brag

;; reference: http://alloytools.org/download/alloy-language-reference.pdf

AlloyModule : ModuleDecl? Import* Paragraph*
ModuleDecl : "module" QualName ("[" NameList "]")?
Import : "open" QualName ("[" QualNameList "]")? ("as" Name)?
Paragraph : SigDecl 
          | FactDecl 
          | PredDecl 
          | FunDecl 
          | AssertDecl 
          | CmdDecl
SigDecl : "abstract"? Mult? "sig" NameList SigExt? "{" DeclList? "}" Block?
SigExt : "extends" QualName 
       | "in" QualName ("+" QualName)*
Mult : MULT-TOK
Decl : "disj"? NameList ":" "disj"? Expr
FactDecl : "fact" Name? Block
PredDecl : "pred" (QualName ".")? Name ParaDecls? Block
FunDecl : "fun" (QualName ".")? Name ParaDecls? ":" Expr "{" Expr "}"
ParaDecls : "(" DeclList? ")" 
          | "[" DeclList? "]"
AssertDecl : "assert" Name? Block
CmdDecl : (Name ":")? ("run" | "check")? (QualName | Block)? Scope?
Scope : "for" Number ("but" TypescopeList)? 
      | "for" TypescopeList
Typescope : "exactly"? Number QualName
Const : "-"? Number 
      | CONST-TOK
UnOp : UN-OP-TOK 
     | Mult
BinOp : BIN-OP-TOK
ArrowOp : (Mult | "set")? "->" (Mult| "set")?
CompareOp : COMP-OP-TOK
LetDecl : Name "=" Expr
Block : "{" Expr* "}"
BlockOrBar : "|" Expr | Block
Quant : QUANT-TOK | Mult
QualName : ("this" "/")? (Name "/")* Name


Expr    : Expr1  
        | "let" LetDeclList BlockOrBar
        | Quant DeclList BlockOrBar
@Expr1  : Expr2  | Expr1 ("||" | "or") Expr2 
@Expr2  : Expr3  | Expr2 ("<=>" | "iff") Expr3
@Expr3  : Expr4  | Expr4 ("=>" | "implies") Expr3 ("else" Expr3)?          ;; right assoc
@Expr4  : Expr5  | Expr4 ("&&" | "and") Expr5
@Expr5  : Expr6  | ("!" | "not") Expr5
@Expr6  : Expr7  | Expr6 ("!" | "not")? COMP-OP-TOK Expr7
@Expr7  : Expr8  | Expr7 ("+" | "-") Expr8
@Expr8  : Expr9  | "#" Expr8
@Expr9  : Expr10 | Expr9 "++" Expr10
@Expr10 : Expr11 | Expr10 "&" Expr11
@Expr11 : Expr12 | Expr11 ArrowOp Expr12
@Expr12 : Expr13 | Expr12 ("<:" | ":>") Expr13
@Expr13 : Expr14 | Expr13 "[" ExprList "]"
ExprList : Expr13
         | Expr13 "," ExprList
@Expr14 : Expr15 | Expr14 "." Expr15
@Expr15 : Expr16 | ("~" | "^" | "*") Expr15
@Expr16 : Const 
        | QualName 
        | "@" Name 
        | "this"
        | "{" DeclList BlockOrBar "}"
        | "(" Expr ")" 
        | Block

;;;;;;;;

Name : IDENTIFIER-TOK
Number : NUM-CONST-TOK
NameList : Name
         | Name "," NameList
QualNameList : QualName
             | QualName "," QualNameList
DeclList : Decl
         | Decl "," DeclList
LetDeclList : LetDecl
            | LetDecl "," LetDeclList
TypescopeList : Typescope
              | Typescope "," TypescopeList

