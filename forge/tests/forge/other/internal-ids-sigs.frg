#lang forge

open "empty.frg"

option run_sterling off
option verbose 0

/*
  Tests to ensure that internal expander-macro identifiers remain 
  inaccessible in the surface language. E.g., the "Block" macro has 
  been changed to the "NT-Block" (NT for non-terminal) macro, but
  we want to confirm there are no regressions.

  It was also unnecessary to rename all non-terminals, and so some 
  of these serve an active purpose in verifying that it continues
  to be unnecessary.
*/

sig AlloyModule, Block, ModuleDecl, Import, SigDecl, RelDecl
     ,PredDecl, FunDecl, CmdDecl, TestDecl, TestExpectDecl 
     ,PropertyDecl,QuantifiedPropertyDecl, SatisfiabilityDecl
     ,ConsistencyDecl, TestSuiteDecl, ExampleDecl, Const, OptionDecl
     ,InstDecl, Expr, Expr1, Expr2, Name, NameList, QualName, LetDecl
     ,Bound, Scope, QualNameList, ParaDeclList, QuantDeclList
     ,BlockOrBar, Quant, NumberList, Bounds, BoundLHS, BindRHSUnion
     ,BindRHSProduct, BindRHSProductBase, AtomNameOrNumber
     ,ArrowExpr, LetDeclList, TypescopeList, TestBlock, Paragraph, SigExt
     ,Mult, ArrowMult, HelperMult, ParaDecl, QuantDecl, ArrowDecl, PredType
     ,ParaDecls, TOMFParams, Typescope, TestConstruct
     ,ArrowOp, CompareOp, Sexpr, Number

     // Used in evaluator queries about a specific instance
     , EvalDecl

     // Not supported, but have a production anyway. Include to detect regressions.
     , AssertDecl ,FactDecl

     // Trying various identifiers introduced by Forge and Racket
     
     // helper macro defined by Forge, but not a keyword
     ,ifte, debug
     // top-level AST struct name
     ,node
     // Racket-defined macros
     ,lambda
     // Racket-defined procedures 
     ,gensym
     // Forge-defined helper procedures 
     ,primify
     // Forge-defined struct
     ,Sig, Run

{}

sig Node {edges: set Node, weightedEdges: set Node -> Int}

pred foo {all n: Node | n = n}
fun bar: Int { let five = 5 | {x : Int | x = five} }
inst baz { Node = `Node0 + `Foo 
           no edges}
pred foo2[n1: Node, n2: Node] {} 

test suite for foo {
    test expect { {foo} for baz is sat 
                  {foo} for 3 Node is sat} 
}
test expect {
    {some Node} is sat 
    {some n: Node | foo2[n, n]} is sat
    {some univ} is sat 
}
assert {} is sat for {} 
assert {} is sat for baz 
assert {} is necessary for foo
assert {} is consistent with foo

example anExample is {foo} for {
  Node = `Node0 + `Node1 + `Providence
  no `Node0.edges
  `Node1.edges = `Providence
}

exampleRun: run { 
  some Node => some Node else some Node
  some (some Node => Node else Node)
} for exactly 5 Node
