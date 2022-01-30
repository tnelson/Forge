#lang forge

option verbose 0

sig Node {
    edges : set Node
}

// pred noVarSome {
//     some | true 
// }
// pred noVarSomeDisj {
//     some disj | true
// }
// test expect {
//     emptySome : {noVarSome iff noVarSomeDisj} is theorem
// }

pred oneVarSome {
    some n0: Node | n0 in Node.edges
}
pred oneVarSomeDisj {
    some disj n0: Node | n0 in Node.edges
}
test expect {
    oneSome : {oneVarSome iff oneVarSomeDisj} is theorem
}

pred twoVarSome {
    some n1, n2: Node {
        no n1 & n2
        n1 in n2.edges
    }
}
pred twoVarSomeDisj {
    some disj n1, n2: Node | n1 in n2.edges
}
test expect {
    twoSome : {twoVarSome iff twoVarSomeDisj} is theorem
}

pred manyVarSome {
    some n0, n1, n2, n3: Node | (no n0 & n1) && (no n0 & n2) && (no n0 & n3) && (no n1 & n2) && (no n1 & n3) && (no n2 & n3) && n1->n2 in edges && n3->n0 in edges
}
            
pred manyVarSomeDisj {
    some disj n0, n1, n2, n3: Node | n1->n2 in edges && n3->n0 in edges
}
            
test expect {
    manyVar : {manyVarSome iff manyVarSomeDisj} is theorem
}