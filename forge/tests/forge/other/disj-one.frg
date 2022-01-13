#lang forge

option verbose 0

sig Node {
    edges : set Node
}

pred oneVarOne {
    one n0: Node | n0 in Node.edges
}
pred oneVarOneDisj {
    one disj n0: Node | n0 in Node.edges
}
test expect {
    oneOne : {oneVarOne iff oneVarOneDisj} is theorem
}

pred twoVarOne {
    one n1, n2: Node {
        no n1 & n2
        n1 in n2.edges
    }
}
pred twoVarOneDisj {
    one disj n1, n2: Node | n1 in n2.edges
}
test expect {
    twoOne : {twoVarOne iff twoVarOneDisj} is theorem
}

pred manyVarOne {
    one n0, n1, n2, n3: Node | (no n0 & n1) && (no n0 & n2) && (no n0 & n3) && (no n1 & n2) && (no n1 & n3) && (no n2 & n3) && n1->n2 in edges && n3->n0 in edges
}
            
pred manyVarOneDisj {
    one disj n0, n1, n2, n3: Node | n1->n2 in edges && n3->n0 in edges
}
            
test expect {
    manyVar : {manyVarOne iff manyVarOneDisj} is theorem
}