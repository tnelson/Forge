/*
  Testing to make sure we understand what
  multiple multiplicites mean (TN Oct 2019)
*/

sig Node {}
one sig Test {
  huh: Node -> Node -> Node
}

pred c1 {
  -- Right-associative
  Test.huh in Node -> one Node -> Node
}

pred c2 {
  all n1: Node | 
    one Test.huh[n1]
}

pred c3 {
  Test.huh in Node -> one Node -> one Node
}

pred c4 {
  -- Test.huh in Node -> one (Node -> Node)
  all n1: Node | 
    one Test.huh[n1]

--  all n2: Node | 
  --  one Test.huh[Node][n2]

  -- Test.huh in (Node -> Node) -> one Node
  all n1, n2: Node |
    one Test.huh[n1][n2]

}

check c1c2 { c1 iff c2 } for 7
check c3c4 { c3 iff c4 } for 7
