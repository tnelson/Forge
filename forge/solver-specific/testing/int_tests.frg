#lang forge

option backend smtlibtor 

option verbose 2

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

run {big_arity}