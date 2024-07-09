#lang forge

option backend smtlibtor

sig Person {
    parent : lone Person,
    spouse : lone Person
}

pred single_quant {
    all p: Person | (p.parent != p)
}

pred nested_quant {
    all p1: Person | {
        all p2: Person | { 
            p1.spouse = p2 => p2.spouse = p1
        }
    }
}

pred double_quant {
    all p1, p2: Person | {
        p1.spouse = p2 => p2.spouse = p1
    }
}

run {double_quant}