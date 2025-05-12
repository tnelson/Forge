#lang forge

open "other.frg"

//option run_sterling off
option verbose 0

/*
  Tests to ensure that internal expander-macro identifiers remain 
  inaccessible in the surface language. E.g., the "Block" macro has 
  been changed to the "NT-Block" (NT for non-terminal) macro, but
  we want to confirm there are no regressions.

  It was also unnecessary to rename all non-terminals, and so some 
  of these serve an active purpose in verifying that it continues
  to be unnecessary.
 
  E.g., to confirm that "AlloyModule" will not clash, it needs to 
  be used as a name in the model.
*/

sig Block, AlloyModule, ModuleDecl, Import, SigDecl, RelDecl
     ,PredDecl, FunDecl, CmdDecl, TestDecl, TestExpectDecl 
     ,PropertyDecl,QuantifiedPropertyDecl, SatisfiabilityDecl
     ,ConsistencyDecl, TestSuiteDecl, ExampleDecl, Const, OptionDecl
     ,InstDecl, Expr, Expr1, Expr2, Name, NameList, QualName, LetDecl
     ,Bound, Scope, ArrowOp, QualNameList, ParaDeclList, QuantDeclList
     ,BlockOrBar, Quant, NumberList, Bounds, BoundLHS, BindRHSUnion
     ,BindRHSProduct, BindRHSProductBase, AtomNameOrNumber, EvalDecl
     ,ArrowExpr, LetDeclList, TypescopeList, TestBlock

     // Not supported, but have a production anyway. Include to detect regressions.
     , AssertDecl ,FactDecl
{}

// TODO: confirm this produces a usable evaluator

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

}

exampleRun: run {} for exactly 5 Node
