#lang forge

option backend smtlibtor 

option verbose 2

sig Person {
    age : one Int,
    friend_length : set Person -> Int
}

pred test_pred {
    some p1 : Person | {
        some p2: Person | {
            p1.age > p2.age
        }
    }
}

run {test_pred}