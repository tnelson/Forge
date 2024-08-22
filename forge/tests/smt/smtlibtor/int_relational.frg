#lang forge

option backend smtlibtor
// option run_sterling false
option verbose 0

sig Person { 
    parent : one Person,
    age : one Int
}

// Simple relational expressions inside of integer comparators.
pred relations_in_int_context_single {
    some p : Person | {
        p.age > 5
    }
}

pred relations_in_int_context_double {
    some p1 : Person | {
        some p2 : Person | {
            p1.age > p2.age
        }
    }
}

// Integer expressions that utilize relational operators within an integer operator context
pred addition_single_relation { 
    some p : Person | {
        add[p.age, 3] > 5
    }
}

pred addition_double_relation { 
    some p1 : Person | {
        some p2 : Person | {
            add[p1.age, p2.age] > 5
        }
    }
}

// "Ambiguous" expressions (=, not int=) that utilize integer expressions with nested relational operators
pred equality_single_relation { 
    some p1 : Person | {
        some p2 : Person | {
            add[p1.age, 1] = p2.age
        }
    }
}

pred equality_double_relation { 
    some p1 : Person | {
        some p2 : Person | {
            some p3 : Person | {
                add[p1.age, p2.age] = p3.age
            }
        }
    }
}

pred manual_sing_1 {
    sing[5] = 5
}

pred sum_of_none {
    add[5, sum[none]] = 5
}

pred ite_no_nest {
    some p : Person | {
        add[1, p.age > 21 => {p.age} else {0}] = 1
    }
}

pred ite_some_nest {
    some p : Person | { 
        add[1, p.age > 21 => {add[1, p.age]} else {0}] = 1
    }
}


pred ite_relational_context {
    some p : Person | {
        age.(p.age > 21 => {p.age} else {0}) = p
    }
}

pred using_sing_1 {
    some p : Person | {
        age.(sing[5]) = p
    }
}

pred using_sing_2 {
    some p1, p2 : Person | {
        age.(sing[add[5, p2.age]]) = p1
    }
}

pred sing_sum_stack {
    some p : Person | {
        sum[sing[sum[sing[sum[p.age]]]]] = 10
    }
}

test expect {
    single_int_context : relations_in_int_context_single is sat
    double_int_context : relations_in_int_context_double is sat
    single_relation_addition : addition_single_relation is sat
    double_relation_addition : addition_double_relation is sat
    single_relation_equality : equality_single_relation is sat
    double_relation_equality : equality_double_relation is sat
    manual_sing : manual_sing_1 is sat
    // none_sum : sum_of_none is sat
    ite_1 : ite_no_nest is sat
    ite_2 : ite_some_nest is sat
    ite_3 : ite_relational_context is sat
    sing_1 : using_sing_1 is sat
    sing_2 : using_sing_2 is sat
    sing_sum : sing_sum_stack is sat
}

/** Regression tests for uniqueness of integer values */ 
test expect {
    regression_int_unique_eq: {
        some x: Int | {
            x = 1
            x = 2
        }
    } is unsat

    regression_int_unique_in: {
        some y: Int | {
            y in 3
            y in 4
        }
    } is unsat

    regression_int_unique_ni: {
        some z: Int | {
            5 in z
            6 in z
        }
    } is unsat

    regression_int_unique_in_union: {
        some y: Int | {
            y in 13+14+15
            y in 16+17+18
        }
    } is unsat

    int_unique_with_arithmetic: {
        some a: Int | {
            a in add[100,200]
            a in add[300,400]
        }
    } is unsat

    int_unique_dependent: {
        some a: Int | some disj p1, p2: Person {
           p1.age != p2.age
           a in add[p1.age, 1]
           a in add[p2.age, 1]
        } 
    } is unsat

}