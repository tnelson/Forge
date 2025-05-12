#lang forge

open "other.frg"

option run_sterling off
option verbose 0

/*
  See internal-ids-sigs.frg for more information. This is a variant of that test file
  that declares predicates rather than sigs. 
*/

pred AlloyModule {}
pred Block {}
pred ModuleDecl {}
pred Import {}
pred SigDecl {}
pred RelDecl {}
pred PredDecl {}
pred FunDecl {}
pred CmdDecl {}
pred TestDecl {}
pred TestExpectDecl {}
pred PropertyDecl {}
pred QuantifiedPropertyDecl {}
pred SatisfiabilityDecl {}
pred ConsistencyDecl {}
pred TestSuiteDecl {}
pred ExampleDecl {}
pred Const {}
pred OptionDecl {}
pred InstDecl {}
pred Expr {}
pred Expr1 {}
pred Expr2 {}
pred Name {}
pred NameList {}
pred QualName {}
pred LetDecl {}
pred Bound {}
pred Scope {}
pred QualNameList {}
pred ParaDeclList {}
pred QuantDeclList {}
pred BlockOrBar {}
pred Quant {}
pred NumberList {}
pred Bounds {}
pred BoundLHS {}
pred BindRHSUnion {}
pred BindRHSProduct {}
pred BindRHSProductBase {}
pred AtomNameOrNumber {}
pred ArrowExpr {}
pred LetDeclList {}
pred TypescopeList {}
pred TestBlock {}
pred Paragraph {}
pred SigExt {}
pred Mult {}
pred ArrowMult {}
pred HelperMult {}
pred ParaDecl {}
pred QuantDecl {}
pred ArrowDecl {}
pred PredType {}
pred ParaDecls {}
pred TOMFParams {}
pred Typescope {}
pred TestConstruct {}
pred ArrowOp {}
pred CompareOp {}
pred Sexpr {}
pred Number {}

// Used in evaluator queries about a specific instance
pred EvalDecl {}
// Not supported, but have a production anyway. Include to detect regressions.
pred AssertDecl {}
pred FactDecl {}

// Trying various identifiers introduced by Forge and Racket     
// helper macro defined by Forge, but not a keyword
pred ifte {}
// top-level AST struct name
pred node {}
// Racket-defined macros
pred lambda {}
// Racket-defined procedures 
pred gensym {}
// Forge-defined helper procedures 
pred primify {}
// Forge-defined struct
pred Sig {}
pred Run {}

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
