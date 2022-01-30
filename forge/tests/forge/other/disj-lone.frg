#lang forge

option verbose 0

sig Node {
    edges : set Node
}

// pred noVarlone {
//     lone | true 
// }
// pred noVarloneDisj {
//     lone disj | true
// }
// test expect {
//     emptylone : {noVarlone iff noVarloneDisj} is theorem
// }

pred oneVarlone {
    lone n0: Node | n0 in Node.edges
}
pred oneVarloneDisj {
    lone disj n0: Node | n0 in Node.edges
}
test expect {
    onelone : {oneVarlone iff oneVarloneDisj} is theorem
}

pred twoVarlone {
    lone n1, n2: Node  | (no n1 & n2) && (n1 in n2.edges)
}

pred twoVarloneDisj {
    lone disj n1, n2: Node | n1 in n2.edges
}

test expect {
    twolone : {twoVarlone iff twoVarloneDisj} is theorem
}

pred manyVarlone {
    lone n0, n1, n2, n3: Node | ((no n0 & n1) && (no n0 & n2) && (no n0 & n3) && (no n1 & n2) && (no n1 & n3) && (no n2 & n3)) && (n1->n2 in edges && n3->n0 in edges)
}
            
pred manyVarloneDisj {
    lone disj n0, n1, n2, n3: Node | n1->n2 in edges && n3->n0 in edges
}
            
test expect {
    manyVar : {manyVarlone iff manyVarloneDisj} is theorem
}