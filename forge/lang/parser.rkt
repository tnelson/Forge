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
Expr : Const 
     | QualName 
     | "@" Name 
     | "this"
	   | UnOp Expr 
     | Expr BinOp Expr 
     | Expr ArrowOp Expr
     | Expr "[" ExprList? "]"
	   | Expr ("!" | "not")? CompareOp Expr
	   | Expr ("=>" | "implies") Expr "else" Expr
	   | "let" LetDeclList BlockOrBar
	   | Quant DeclList BlockOrBar
	   | "{" DeclList BlockOrBar "}"
	   | "(" Expr ")" 
     | Block
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
QualName : "this/"? (Name "/")* Name



Name : IDENTIFIER-TOK
Number : NUM-CONST-TOK
NameList : Name
         | Name "," NameList
QualNameList : QualName
             | QualName "," QualNameList
DeclList : Decl
         | Decl "," DeclList
TypescopeList : Typescope
              | Typescope "," TypescopeList
ExprList : Expr
         | Expr "," ExprList
LetDeclList : LetDecl
            | LetDecl "," LetDeclList
