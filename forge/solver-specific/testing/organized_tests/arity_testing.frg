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

pred p1 {
    all a : Animal | no a.f1
}

pred p2 { 
    all a : Animal | no a.f2
}

pred p3 {
    all a : Animal | no a.f3
}

pred p4 {
    all a : Animal | no a.f4
}

pred p5 {
    all a : Animal | no a.f5
}

pred p6 {
    all a : Animal | no a.f6
}

pred p7 {
    all a : Animal | no a.f7
}

pred p8 {
    all n : NoExtend | no n.field1
}

test expect {
    test1 : p1 is sat
    test2 : p2 is sat
    test3 : p3 is sat
    test4 : p4 is sat
    test5 : p5 is sat
    test6 : p6 is sat
    test7 : p7 is sat
    test8 : p8 is sat
}

