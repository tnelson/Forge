#lang forge

option run_sterling off


option verbose 0

sig Node {
    edges : set Node
}

// pred noVarall {
//     all | true 
// }
// pred noVarallDisj {
//     all disj | true
// }
// test expect {
//     emptyall : {noVarall iff noVarallDisj} is theorem
// }

pred oneVarall {
    all n0: Node | n0 in Node.edges
}
pred oneVarallDisj {
    all disj n0: Node | n0 in Node.edges
}
test expect {
    oneall : {oneVarall iff oneVarallDisj} is theorem
}

pred twoVarall {
    all n1, n2: Node  | (no n1 & n2) implies (n1 in n2.edges)
}

pred twoVarallDisj {
    all disj n1, n2: Node | n1 in n2.edges
}

test expect {
    twoall : {twoVarall iff twoVarallDisj} is theorem
}

pred manyVarall {
    all n0, n1, n2, n3: Node | ((no n0 & n1) && (no n0 & n2) && (no n0 & n3) && (no n1 & n2) && (no n1 & n3) && (no n2 & n3)) implies (n1->n2 in edges && n3->n0 in edges)
}
            
pred manyVarallDisj {
    all disj n0, n1, n2, n3: Node | n1->n2 in edges && n3->n0 in edges
}
            
test expect {
    manyVar : {manyVarall iff manyVarallDisj} is theorem
}