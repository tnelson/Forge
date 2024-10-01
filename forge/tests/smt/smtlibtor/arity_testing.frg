#lang forge

option backend smtlibtor

sig Mammal {}

sig Person extends Mammal {}

sig Animal extends Mammal {
    f1 : set Person -> Person -> Animal -> Int,
    f2: set Mammal -> Person -> Animal -> Int,
    f3 : set Mammal -> Int,
    f4 : set Int -> Int,
    f5 : set Mammal -> Person,
    f6 : set Person,
    f7 : set Mammal
}

sig NoExtend {
    field1: set NoExtend -> NoExtend
}

pred no_p1 {
    some Animal
    all a : Animal | no a.f1
}

pred no_p2 { 
    some Animal 
    all a : Animal | no a.f2
}

pred no_p3 {
    some Animal
    all a : Animal | no a.f3
}

pred no_p4 {
    some Animal
    all a : Animal | no a.f4
}

pred no_p5 {
    some Animal
    all a : Animal | no a.f5
}

pred no_p6 {
    some Animal
    all a : Animal | no a.f6
}

pred no_p7 {
    some Animal
    all a : Animal | no a.f7
}

pred no_p8 {
    some NoExtend
    all n : NoExtend | no n.field1
}

pred some_p1 {
    some Animal
    all a : Animal | some a.f1
}

pred some_p2 {
    some Animal
    all a : Animal | some a.f2
}

pred some_p3 {
    some Animal
    all a : Animal | some a.f3
}

pred some_p4 {
    some Animal
    all a : Animal | some a.f4
}

pred some_p5 {
    some Animal
    all a : Animal | some a.f5
}

pred some_p6 {
    some Animal
    all a : Animal | some a.f6
}

pred some_p7 {
    some Animal
    all a : Animal | some a.f7
}

pred some_p8 {
    some NoExtend
    all n : NoExtend | some n.field1
}

pred one_p1 {
    some Animal
    all a : Animal | one a.f1
}

pred one_p2 {
    some Animal
    all a : Animal | one a.f2
}

pred one_p3 {
    some Animal
    all a : Animal | one a.f3
}

pred one_p4 {
    some Animal
    all a : Animal | one a.f4
}

pred one_p5 {
    some Animal
    all a : Animal | one a.f5
}

pred one_p6 {
    some Animal
    all a : Animal | one a.f6
}

pred one_p7 {
    some Animal
    all a : Animal | one a.f7
}

pred one_p8 {
    some NoExtend
    all n : NoExtend | one n.field1
}

pred lone_p1 {
    some Animal
    all a : Animal | lone a.f1
}

pred lone_p2 {
    some Animal
    all a : Animal | lone a.f2
}

pred lone_p3 {
    some Animal
    all a : Animal | lone a.f3
}

pred lone_p4 {
    some Animal
    all a : Animal | lone a.f4
}

pred lone_p5 {
    some Animal
    all a : Animal | lone a.f5
}

pred lone_p6 {
    some Animal
    all a : Animal | lone a.f6
}

pred lone_p7 {
    some Animal
    all a : Animal | lone a.f7
}

pred lone_p8 {
    some NoExtend
    all n : NoExtend | lone n.field1
}

test expect {
    test1 : no_p1 is sat
    test2 : no_p2 is sat
    test3 : no_p3 is sat
    test4 : no_p4 is sat
    test5 : no_p5 is sat
    test6 : no_p6 is sat
    test7 : no_p7 is sat
    test8 : no_p8 is sat
    test9 : some_p1 is sat
    test10 : some_p2 is sat
    test11 : some_p3 is sat
    test12 : some_p4 is sat
    test13 : some_p5 is sat
    test14 : some_p6 is sat
    test15 : some_p7 is sat
    test16 : some_p8 is sat
    test17 : one_p1 is sat
    test18 : one_p2 is sat
    test19 : one_p3 is sat
    test20 : one_p4 is sat
    test21 : one_p5 is sat
    test22 : one_p6 is sat
    test23 : one_p7 is sat
    test24 : one_p8 is sat
    test25 : lone_p1 is sat
    test26 : lone_p2 is sat
    test27 : lone_p3 is sat
    test28 : lone_p4 is sat
    test29 : lone_p5 is sat
    test30 : lone_p6 is sat
    test31 : lone_p7 is sat
    test32 : lone_p8 is sat
}

