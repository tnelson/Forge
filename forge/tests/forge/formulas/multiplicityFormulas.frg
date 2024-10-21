#lang forge

option run_sterling off


option verbose 0

abstract sig Color {}
one sig Red extends Color {}
one sig Green extends Color {}
one sig Blue extends Color {}

abstract sig Node {
    edges: set Node->Color
}

one sig N1 extends Node {}
one sig N2 extends Node {}
one sig N3 extends Node {}

inst TestInst {
    edges = (`N10->`N20 + `N10->`N30 + `N20->`N30 + `N30->`N30)->`Red0 +
            (`N10->`N10 + `N10->`N20 + `N10->`N30 + 
             `N20->`N30 + 
             `N30->`N20)->`Green0
}

pred Some {
    some      N1.(edges.Red)
    some      N2.(edges.Red)
    some      N3.(edges.Red)
    not (some (edges.Red).N1)
    some      (edges.Red).N2
    some      (edges.Red).N3
}

pred No {
    not (no N1.(edges.Red))
    not (no N2.(edges.Red))
    not (no N3.(edges.Red))
    no      (edges.Red).N1
    not (no (edges.Red).N2)
    not (no (edges.Red).N3)
}

pred One1 {
    not (one N1.(edges.Red))
    one      N2.(edges.Red)
    one      N3.(edges.Red)
    not (one (edges.Red).N1)
    one      (edges.Red).N2
    not (one (edges.Red).N3)
}

pred Lone1 {
    not (lone N1.(edges.Red))
    lone      N2.(edges.Red)
    lone      N3.(edges.Red)
    lone      (edges.Red).N1
    lone      (edges.Red).N2
    not (lone (edges.Red).N3)
}

-- one/lone quantifiers are treated as multiplicity formulas by ast.rkt,
-- rather than quantifier formulas. But we still must check that they are consistent.

pred One2 {
    not {one n: Node | n in N1.(edges.Red)}
    one n: Node | n in N2.(edges.Red)
    one n: Node | n in N3.(edges.Red)
    not {one n: Node | n in (edges.Red).N1}
    one n: Node | n in (edges.Red).N2
    not {one n: Node | n in (edges.Red).N3}
}

pred Lone2 {
    not {lone n: Node | n in  N1.(edges.Red)}
    lone n: Node | n in N2.(edges.Red)
    lone n: Node | n in  N3.(edges.Red)
    lone n: Node | n in (edges.Red).N1
    lone n: Node | n in (edges.Red).N2
    not {lone n: Node | n in (edges.Red).N3}
}

pred SomePred[n: Node] {
    n->Node->Red in edges
}

pred Equivalence {
    (lone n: Node | SomePred[n]) iff
    ((one n: Node | SomePred[n]) or 
     (no  n: Node | SomePred[n]))
}

test expect MultiplicityFormulas {
    someAsMultiplicity : Some for TestInst is checked
    noAsMultiplicity : No for TestInst is checked
    oneAsMultiplicity : One1 for TestInst is checked
    loneAsMultiplicity : Lone1 for TestInst is checked

    oneAsQuantifer : One2 for TestInst is checked 
    loneAsQuantifer : Lone2 for TestInst is checked 

    loneEquivalentOneNo : Equivalence is checked
}
