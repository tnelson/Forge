#lang forge

option backend smtlibtor

sig Person {
    age : one Int
}

test expect {
    testing : {some p : Person | p.age > 0} is sat
}