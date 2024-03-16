#lang forge

sig A{ }
sig A_child1 extends A { } 
sig A_child2 extends A {}
sig B { }
sig B_child1 extends B { }
sig B_child2 extends B { }
sig B_child1_child extends B_child1 { }
pred p[a: B_child1 + A] {
}


test expect {
    practicing: {some x: B_child2 | p[x]} is sat
}