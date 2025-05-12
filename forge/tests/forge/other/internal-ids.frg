#lang forge

open "other.frg"

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
{}

sig Node {edges: set Node, weightedEdges: set Node -> Int}

pred foo {all n: Node | n = n}
fun bar: Int { let five = 5 | {x : Int | x = five} }
inst baz {}
pred foo2[n1: Node, n2: Node] {} 

test suite for foo {
    test expect { {foo} is sat } 
}
test expect {
    {some Node} is sat -- tests Exprs as well
    {some n: Node | foo2[n, n]} is sat -- tests NameList as well
    {some univ} is sat -- tests Const as well
}
assert {} is sat for {} -- tests local insts as well
assert {} is sat for baz -- tests declared insts as well
assert {} is necessary for foo
assert {} is consistent with foo

example anExample is {foo} for {
  Node = `Node0 + `Node1 + `Providence
}

exampleRun: run {} for exactly 5 Node
