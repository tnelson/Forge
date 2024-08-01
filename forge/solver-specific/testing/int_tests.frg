#lang forge

option backend smtlibtor 

option run_sterling off

option verbose 0

sig Person {
    age : one Int,
    friend_length : set Person -> Person -> Int
}

pred test_pred {
    some p1 : Person | {
        some p2: Person | {
            p1.age > p2.age
        }
    }
}

pred big_arity {
    some p : Person | {
        all other_p : Person | p.friend_length[p][p] < p.age
    }
}


pred adding_single_rel {
    some p : Person | {
        add[p.age, 3] > 5
    }
}

pred adding_double_rel {
    some p1 : Person | {
        some p2 : Person | {
            add[p1.age, p2.age] > 5
        }
    }
}

pred nested_check {
    some p : Person | {
        add[p.age, p.age] = p.age
    }
}

pred in_check {
    some p : Person | {
        add[p.age, p.age] in p.age
    }
}

run {nested_check}