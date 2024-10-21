#lang forge

option run_sterling off
option verbose 0

sig Node {
    edges : set Node
}

-- Unary operators

pred Tilde {
    ~edges = {n1: Node, n2: Node | n2->n1 in edges}
}

pred Caret {
    #Node < 4 implies {
        ^edges = edges + edges.edges + edges.edges.edges + edges.edges.edges.edges
    }
}

pred Star {
    *edges = ^edges + iden
}


-- Binary operators

pred Plus {
    all n1, n2: Node |
        n1.edges + n2.edges = {n: Node | n in n1.edges or n in n2.edges}
}

pred Minus {
    all n1, n2: Node |
        n1.edges - n2.edges = {n: Node | n in n1.edges and n !in n2.edges}
}

pred Ampersand {
    all n1, n2: Node |
        n1.edges & n2.edges = {n: Node | n in n1.edges and n in n2.edges}
}

pred Arrow {
    all n1, n2: Node |
        n1.edges -> n2.edges = {n3: Node, n4: Node | n3 in n1.edges and n4 in n2.edges}
}

pred Dot {
    edges.edges = {n1: Node, n2: Node | some n3: Node | n3 in n1.edges and n3 in edges.n2}
}

pred IfThenElse1 {
    no (edges in edges => univ else none)
}

pred IfThenElse2 {
    some (some Node & Int => univ else none)
}


pred LessColon {
    all n: Node |
        n.edges <: edges = {n1: Node, n2: Node | n1->n2 in edges and n1 in n.edges}
}

pred ColonGreater {
    all n: Node |
        edges :> n.edges = {n1: Node, n2: Node | n1->n2 in edges and n2 in n.edges}
}

// These are tested more throughly in forge-core/expressedOperators.rkt
//   (Surface Forge can't express tests about metadata.)
fun helper1: one univ { univ }
fun helper1a: univ { univ }
fun helper2[x: univ]: one univ {x & Int}
fun helper2a[x: univ]: univ {x & Int}
fun helper3[x: univ, y: univ]: lone univ { x & y }
fun helper3a[x: one univ, y: univ]: set univ { x & y }

pred HelperFun {
    helper1 = univ
    helper1a = univ
    all value1, value2: univ | {
        helper2[value1] = Int & value1
        helper2a[value1] = Int & value1
        helper3[value1, value2] = value2 & value1
        helper3a[value1, value2] = value2 & value1
    }
}

fun canConvertIntExprNullary: one Int {
    #{n: Node | some n.edges}
}

fun canConvertIntExprUnary[nodes: set Node]: one Int {
    #{n: nodes | some n.edges}
}

fun forceSpacerAroundNode[n: Node]: one Node { n }

test expect ExpressionOperators {
    tilde : Tilde is checked
    caret : Caret is checked
    star : Star is checked
    plus : Plus is checked
    minus : Minus is checked
    ampersand : Ampersand is checked
    arrow : Arrow is checked
    dot : Dot is checked
    lessColon : LessColon is checked
    colonGreater : ColonGreater is checked

    ite1 : IfThenElse1 is unsat
    ite2 : IfThenElse2 is unsat

    helpers : HelperFun is checked
    autoConvertIntFun : {canConvertIntExprNullary <= #Node} is checked
    autoConvertIntFun_args : {canConvertIntExprUnary[Node] <= #Node} is checked

    -- Regression test for last-checker/join/spacer issue; Jan 05 2024
    regression_join_on_spacer : {
        some n: Node | { 
            some forceSpacerAroundNode[n].edges
        }
    } is sat
}
