#lang forge

option run_sterling off


option verbose 0

sig Node {
    edges : set Node
}

// pred noVarno {
//     no | true 
// }
// pred noVarnoDisj {
//     no disj | true
// }
// test expect {
//     emptyno : {noVarno iff noVarnoDisj} is checked
// }

pred oneVarno {
    no n0: Node | n0 in Node.edges
}
pred oneVarnoDisj {
    no disj n0: Node | n0 in Node.edges
}
test expect {
    oneVar : {oneVarno iff oneVarnoDisj} is checked
}

pred twoVarno {
    no n1, n2: Node  | (no n1 & n2) && (n1 in n2.edges)
}

pred twoVarnoDisj {
    no disj n1, n2: Node | n1 in n2.edges
}

test expect {
    twoVar : {twoVarno iff twoVarnoDisj} is checked
}

pred manyVarno {
    no n0, n1, n2, n3: Node | ((no n0 & n1) && (no n0 & n2) && (no n0 & n3) && (no n1 & n2) && (no n1 & n3) && (no n2 & n3)) && (n1->n2 in edges && n3->n0 in edges)
}
            
pred manyVarnoDisj {
    no disj n0, n1, n2, n3: Node | n1->n2 in edges && n3->n0 in edges
}
            
test expect {
    manyVar : {manyVarno iff manyVarnoDisj} is checked
}