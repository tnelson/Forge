#lang brag

; Alloy reference: http://alloytools.org/download/alloy-language-reference.pdf
; Forge's grammar is somewhat simpler, and introduces some new constructs,
; but is heavily inspired by Alloy.

; Grammar was cleaned up circa Feb 12 2022, so if you're looking for
; module names, old XML-style instances, state/transition sublanguage,
; etc. they would need to be re-added from prior history.

; Good error messages are tough to generate from the parser, especially when
; the language is ambiguous. E.g., x[y] might mean an invocation of a helper
; function, predicate, or a box-join. "a => b[x] else c[x]" makes this even worse.
; Thus: PARSE A MORE PERMISSIVE GRAMMAR than Forge really has. Then filter in the
; expander, after the input is parsed. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Remaining notes re: parser cleanup

; TODO: custom errors for common Alloy keywords
;   e.g., fact, after, ...
; TODO: odd associativity for => interplay with quantification
; TODO: Does brag allow heuristics for error location?


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Starting production for the parser
; Two options: a model, or an evaluation request
AlloyModule :  
            Import* Paragraph*
            | EvalDecl*

; Import other Forge files by name
Import : /OPEN-TOK QualName (LEFT-SQUARE-TOK QualNameList RIGHT-SQUARE-TOK)? (AS-TOK Name)?
       | /OPEN-TOK PATH-OR-STR-TOK (AS-TOK Name)?

; Basic top-level constructs of a model: sigs, preds, commands, etc.
@Paragraph : 
            SigDecl 
          | PredDecl 
          | FunDecl 
          | AssertDecl 
          | CmdDecl
          | TestExpectDecl
          | SexprDecl
          | QueryDecl
          | EvalRelDecl
          | OptionDecl
          | InstDecl
          | ExampleDecl 
          | PropertyDecl
          | QuantifiedPropertyDecl
          | SatisfiabilityDecl
          | ConsistencyDecl
          | TestSuiteDecl

; NOTE: When extending sigs with "in" (subset sigs) is implemented,
;  if "sig A in B extends C" is allowed, update this to allow multiple SigExt
SigDecl : VAR-TOK? ABSTRACT-TOK? Mult? /SIG-TOK NameList SigExt? /LEFT-CURLY-TOK ArrowDeclList? /RIGHT-CURLY-TOK Block?
SigExt : EXTENDS-TOK QualName 
       | IN-TOK QualName (PLUS-TOK QualName)*

Mult : LONE-TOK | SOME-TOK | ONE-TOK | TWO-TOK
; for field etc. declaration
ArrowMult : LONE-TOK | SET-TOK | ONE-TOK | TWO-TOK | FUNC-TOK | PFUNC-TOK
; for helper fun/pred declaration
HelperMult : LONE-TOK | SET-TOK | ONE-TOK | FUNC-TOK | PFUNC-TOK
ParaDecl  : DISJ-TOK? NameList /COLON-TOK HelperMult? Expr
QuantDecl : DISJ-TOK? NameList /COLON-TOK SET-TOK? Expr

; ArrowDecl should only be used by sig field declaration right now; 
; note the optional VAR for Electrum. Remember that a preceding / means
;  to cut the token; it won't get included in the AST.
ArrowDecl : VAR-TOK? NameList /COLON-TOK ArrowMult ArrowExpr

PredType : WHEAT-TOK

; A predicate declaration can contain any number of formulas in its body
PredDecl : /PRED-TOK PredType? (QualName DOT-TOK)? Name ParaDecls? Block
; A function declaration should only ever contain a single expression in its body
FunDecl : /FUN-TOK (QualName DOT-TOK)? Name ParaDecls? /COLON-TOK HelperMult? Expr /LEFT-CURLY-TOK Expr /RIGHT-CURLY-TOK
; A ParaDecls is a special declaration form for pred/fun definition, where every identifier
; is paired with an expr and (optional) multiplicity
ParaDecls : /LEFT-PAREN-TOK @ParaDeclList? /RIGHT-PAREN-TOK 
          | /LEFT-SQUARE-TOK @ParaDeclList? /RIGHT-SQUARE-TOK

AssertDecl : /ASSERT-TOK Name? Block
CmdDecl :  (Name /COLON-TOK)? (RUN-TOK | CHECK-TOK) (QualName | Block)? Scope? (/FOR-TOK Bounds)?

TestDecl : (Name /COLON-TOK)? (QualName | Block) Scope? (/FOR-TOK Bounds)? /IS-TOK
           (SAT-TOK | UNSAT-TOK | UNKNOWN-TOK | THEOREM-TOK | FORGE_ERROR-TOK (PATH-OR-STR-TOK)? | CHECKED-TOK )
TestExpectDecl : TEST-TOK? EXPECT-TOK Name? TestBlock
TestBlock : /LEFT-CURLY-TOK TestDecl* /RIGHT-CURLY-TOK
Scope : /FOR-TOK Number (/BUT-TOK @TypescopeList)?
      | /FOR-TOK @TypescopeList
Typescope : EXACTLY-TOK? Number QualName
Const : NONE-TOK | UNIV-TOK | IDEN-TOK
      | MINUS-TOK? Number 

SatisfiabilityDecl : /ASSERT-TOK Name /IS-TOK (SAT-TOK | UNSAT-TOK | FORGE_ERROR-TOK) Scope? (/FOR-TOK Bounds)? 
PropertyDecl : /ASSERT-TOK Name /IS-TOK (SUFFICIENT-TOK | NECESSARY-TOK) /FOR-TOK Name Scope? (/FOR-TOK Bounds)? 
QuantifiedPropertyDecl : /ASSERT-TOK /ALL-TOK DISJ-TOK? QuantDeclList /BAR-TOK Name (/LEFT-SQUARE-TOK ExprList /RIGHT-SQUARE-TOK)?  /IS-TOK (SUFFICIENT-TOK | NECESSARY-TOK) /FOR-TOK Name (/LEFT-SQUARE-TOK ExprList /RIGHT-SQUARE-TOK)? Scope? (/FOR-TOK Bounds)? 

; Should this have a unique name?
ConsistencyDecl: /ASSERT-TOK Expr /IS-TOK (CONSISTENT-TOK | INCONSISTENT-TOK) /WITH-TOK Name Scope? (/FOR-TOK Bounds)?


TestSuiteDecl : /TEST-TOK /SUITE-TOK /FOR-TOK Name /LEFT-CURLY-TOK TestConstruct* /RIGHT-CURLY-TOK

@TestConstruct : ExampleDecl | TestExpectDecl | PropertyDecl | QuantifiedPropertyDecl | SatisfiabilityDecl | ConsistencyDecl


# UnOp : Mult
#      | NEG-TOK | NO-TOK | SET-TOK | HASH-TOK | TILDE-TOK | STAR-TOK | EXP-TOK
# BinOp : OR-TOK | AND-TOK | IFF-TOK | IMP-TOK | AMP-TOK
#       | PLUS-TOK | MINUS-TOK | PPLUS-TOK | DOT-TOK | SUBT-TOK | SUPT-TOK
ArrowOp : (@Mult | SET-TOK)? ARROW-TOK (@Mult | SET-TOK)?
CompareOp : IN-TOK | EQ-TOK | LT-TOK | GT-TOK | LEQ-TOK | GEQ-TOK | IS-TOK | NI-TOK
LetDecl : @Name /EQ-TOK Expr
Block : /LEFT-CURLY-TOK Expr* /RIGHT-CURLY-TOK
BlockOrBar : Block | BAR-TOK Expr 
Quant : ALL-TOK | NO-TOK | SUM-TOK | @Mult
QualName : (THIS-TOK /SLASH-TOK)? (@Name /SLASH-TOK)* @Name | INT-TOK | SUM-TOK
OptionDecl : /OPTION-TOK QualName (QualName | PATH-OR-STR-TOK | MINUS-TOK? Number)

Name : IDENTIFIER-TOK
NameList : @Name
         | @Name /COMMA-TOK @NameList
QualNameList : @QualName
             | @QualName /COMMA-TOK @QualNameList

; Used only in fun/pred definition
ParaDeclList : ParaDecl
             | ParaDecl /COMMA-TOK @ParaDeclList
; Used only in quantifiers / set comprehension
QuantDeclList : QuantDecl
              | QuantDecl /COMMA-TOK @QuantDeclList
; Used in field declaration
ArrowDeclList : ArrowDecl
              | ArrowDecl /COMMA-TOK @ArrowDeclList

LetDeclList : LetDecl
            | LetDecl /COMMA-TOK @LetDeclList
TypescopeList : Typescope
              | Typescope /COMMA-TOK @TypescopeList
ExprList : Expr
         | Expr /COMMA-TOK @ExprList

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This chain of productions enforces operator precedence in brag.
; The list goes from weakest to strongest binding operators.

; This machinery is needed because, for better or worse, brag has no
; yacc-style "assoc" annotations, and so we need to enforce associativity
; and precedence ourselves. 

; Example: 
;   e := e /\ e | e => e | num
; as a BNF this is OK, but ambiguity must be resolved to build a parser. 
; Without "assoc" lists, we have to deny the parser undesireable derivations 
; ourselves, by controlling the flow of non-terminals. E.g.:
;   e  := e1 => e | @e1
;   e1 := e2 /\ e1 | @e2
;   e2 := num
;
; Note that, once we've progressed to e1, we no longer have the ability to 
; parse implication! Likewise, progressing into e2 removes the ability to 
; parse conjunction. "e1 => e" simply cannot have another implication on
; the LHS of the implication; others must group to the right. Neither side
; of "e2 /\ e1" can be an implication. Etc.

; Note on brag syntax:
; @: splice, merges elements of a node into the surrounding node
;   LHS splice: *always* merged into surrounding node
;   RHS splice: @'d pattern element is spliced if it appears

; Following Alloy's lead, we don't try to make the parser disambiguate 
; "formulas" and "expressions" --- just produce a parse tree that the 
; expander can handle. Alloy 6 spec:
;    https://alloytools.org/spec.html

; "All binary operators associate to the left, with the exception of 
;  implication and sequence, which associate to the right, and of 
;  binary temporal connectives which are not associative."

Expr    : @Expr1
        | LET-TOK LetDeclList BlockOrBar
        | BIND-TOK LetDeclList BlockOrBar
        | Quant DISJ-TOK? QuantDeclList BlockOrBar
Expr1   : @Expr1.5  | Expr1 OR-TOK Expr1.5
Expr1.5 : @Expr2    | Expr1.5 XOR-TOK Expr2
Expr2   : @Expr3  | Expr2 IFF-TOK Expr3
;; right assoc
Expr3   : @Expr4  | Expr4 IMP-TOK Expr3 (ELSE-TOK Expr3)?        
Expr4   : @Expr4.5  | Expr4 AND-TOK Expr4.5
; Electrum binary operators (not associative)
Expr4.5 : @Expr5  | Expr5 UNTIL-TOK Expr5
                  | Expr5 RELEASE-TOK Expr5
                  | Expr5 SINCE-TOK Expr5
                  | Expr5 TRIGGERED-TOK Expr5
Expr5   : @Expr6  | NEG-TOK Expr5
                  | ALWAYS-TOK Expr5
                  | EVENTUALLY-TOK Expr5
                  | AFTER-TOK Expr5
                  | BEFORE-TOK Expr5
                  | ONCE-TOK Expr5
                  | HISTORICALLY-TOK Expr5
Expr6   : @Expr7  | Expr6 NEG-TOK? CompareOp Expr7
Expr7   : @Expr8 | (NO-TOK | SOME-TOK | LONE-TOK | ONE-TOK | TWO-TOK | SET-TOK) Expr8
Expr8   : @Expr9  | Expr8 (PLUS-TOK | MINUS-TOK) Expr10
Expr9   : @Expr10 | CARD-TOK Expr9
Expr10  : @Expr11 | Expr10 PPLUS-TOK Expr11
Expr11  : @Expr12 | Expr11 AMP-TOK Expr12
Expr12  : @Expr13 | Expr12 ArrowOp Expr13
Expr13  : @Expr14 | Expr13 (SUBT-TOK | SUPT-TOK) Expr14
Expr14  : @Expr15 | Expr14 LEFT-SQUARE-TOK ExprList RIGHT-SQUARE-TOK
Expr15  : @Expr16 | Expr15 DOT-TOK Expr16
                  | Name LEFT-SQUARE-TOK ExprList RIGHT-SQUARE-TOK
Expr16  : @Expr17 | Expr16 PRIME-TOK
Expr17  : @Expr18 | (TILDE-TOK | EXP-TOK | STAR-TOK) Expr17
Expr18  : Const 
        | QualName 
        | AT-TOK Name
        | BACKQUOTE-TOK Name
        | THIS-TOK
        | LEFT-CURLY-TOK QuantDeclList BlockOrBar RIGHT-CURLY-TOK
        | /LEFT-PAREN-TOK @Expr /RIGHT-PAREN-TOK
        | Block
        | Sexpr

ArrowExpr : QualName
          | QualName /ARROW-TOK @ArrowExpr


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Escape to forge/core 

SexprDecl : Sexpr
Sexpr : SEXPR-TOK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Partial Instances

; In older versions, instances were pasted in as raw XML.
; This isn't used nor tested, so disabled
;InstanceDecl : INSTANCE-TOK

; Partial `inst` declaration
InstDecl : /INST-TOK Name Bounds Scope?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Productions for the evaluation-request case
;   i.e., this isn't a Forge model, but rather a query 
;   from the evaluator:
EvalRelDecl : ArrowDecl
EvalDecl : EVAL-TOK Expr

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Bounds : EXACTLY-TOK? @ExprList
;       | EXACTLY-TOK? @Block

ExampleDecl : /EXAMPLE-TOK Name /IS-TOK Expr /FOR-TOK Bounds

; ??? used where?
QueryDecl : @Name /COLON-TOK ArrowExpr /EQ-TOK Expr


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Ints
NumberList : Number
           | Number /COMMA-TOK @NumberList

Number : NUM-CONST-TOK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Inst/Example Bounds
; Note: don't use "EQUAL-TOK" here; it is in scope but not "="
; TODO: plus

; QualName if providing an identifier that refers to an inst declaration, possibly with "exactly"
Bounds : /LEFT-CURLY-TOK Bound* /RIGHT-CURLY-TOK
        | [EXACTLY-TOK] QualName

; Allow more permissive language here (technically only EQ-TOK, IN-TOK, NI-TOK allowed)
AtomNameOrNumber : BACKQUOTE-TOK Name | Number | MINUS-TOK Number
Bound : BoundLHS CompareOp BindRHSUnion
        | NO-TOK BoundLHS
        | QualName ; if providing an identifier that refers to an inst declaration (no exactly)

BoundLHS : CARD-TOK QualName
         | QualName
         | AtomNameOrNumber (/DOT-TOK QualName)+

; Union context
BindRHSUnion : BindRHSProduct
             | BindRHSUnion /PLUS-TOK BindRHSProduct
             | /LEFT-PAREN-TOK @BindRHSUnion /RIGHT-PAREN-TOK

; Tuple context
BindRHSProduct : /LEFT-PAREN-TOK @BindRHSProduct /RIGHT-PAREN-TOK
               | BindRHSProduct (/COMMA-TOK | /ARROW-TOK) @BindRHSProductBase
               | @BindRHSProductBase

; Base case for tuple context to help avoid ambiguity 
BindRHSProductBase : AtomNameOrNumber
                   | QualName   ; a Sig identifier (later, will check it has been bound)
                   ; Do not splice this non-terminal; it's needed to indicate switch in cases
                   | /LEFT-PAREN-TOK BindRHSUnion /RIGHT-PAREN-TOK ; e.g., "Node -> (0 + 1) -> Node"
