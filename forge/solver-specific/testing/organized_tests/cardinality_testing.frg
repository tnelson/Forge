#lang forge

option backend smtlibtor
option run_sterling off

sig Person {
    age : one Int,
    parent : set Person
}

sig Animal {}

pred simple_card_greater {
    #{Person} > 4
}

pred simple_card_equal {
    #{Person} = 4
}

pred simple_card_less {
    #{Person} < 4
}

pred simple_card_leq {
    #{Person} <= 4
}

pred simple_card_geq {
    #{Person} >= 4
}

pred int_on_left_greater {
    4 > #{Person}
}

pred int_on_left_less {
    4 < #{Person}
}

pred union_card {
    #{Person + Animal} > 6 
}

pred comprehension_card {
    #{p : Person | p.age > 10} = 2
}

pred testing_quick {
    one parent
}

pred quantified_card {
    all p : Person | #{q : Person | q.age = p.age} = 1
}

pred no_excess_quantified_card {
    all p : Person | #{p.parent} = 2
}

pred less_than_1 { 
    all p : Person | #{p.parent} < 1
}

pred equal_zero {
    all p : Person | #{p.parent} = 0
}

pred less_than_zero {
    all p : Person | #{p.parent} < 0
}

pred equal_negative {
    all p : Person | #{p.parent} = -1
}

pred greater_than_negative {
    all p : Person | #{p.parent} > -1
}

pred greater_than_smaller_negative {
    all p : Person | #{p.parent} > -5
}

pred less_than_negative {
    all p : Person | #{p.parent} < -3
}

test expect {
    simple_greater : {simple_card_greater} is sat
    simple_equal : {simple_card_equal} is sat
    simple_less : {simple_card_less} is sat
    simple_leq : {simple_card_leq} is sat
    simple_geq : {simple_card_geq} is sat
    left_greater : {int_on_left_greater} is sat
    left_less : {int_on_left_less} is sat
    union : {union_card} is sat
    comprehension : {comprehension_card} is sat
    quick : {testing_quick} is sat
    quantified : {quantified_card} is sat
    no_excess : {no_excess_quantified_card} is sat
    zero_1 : {less_than_1} is sat
    zero_2 : {equal_zero} is sat
    neg_1 : {less_than_zero} is forge_error
    neg_2 : {equal_negative} is forge_error
    neg_3 : {greater_than_negative} is sat
    neg_4 : {greater_than_smaller_negative} is sat
    neg_5 : {less_than_negative} is forge_error
}