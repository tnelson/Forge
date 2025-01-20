#lang forge

option backend smtlibtor
option verbose 0

sig Person {
    age : one Int,
    parent : set Person,
    friend_of : set Person -> Person
}

pred one_arity_1 {
    one Person
}

pred one_arity_2_atom {
    one parent
}

pred one_arity_2_int {
    one age
}

pred one_arity_3 {
    one friend_of
}

pred lone_arity_1 {
    lone Person
}

pred lone_arity_2_atom {
    lone parent
}

pred lone_arity_2_int {
    lone age
}

pred lone_arity_3 {
    lone friend_of
}

pred some_arity_1 {
    some Person
}

pred some_arity_2_atom {
    some parent
}

pred some_arity_2_int {
    some age
}

pred some_arity_3 {
    some friend_of
}

pred no_arity_1 {
    no Person
}

pred no_arity_2_atom {
    no parent
}

pred no_arity_2_int {
    no age
}

pred no_arity_3 {
    no friend_of
}

test expect {
    one_1 : {one_arity_1} is sat
    one_2 : {one_arity_2_atom} is sat
    one_3 : {one_arity_2_int} is sat
    one_4 : {one_arity_3} is sat
    lone_1 : {lone_arity_1} is sat
    lone_2 : {lone_arity_2_atom} is sat
    lone_3 : {lone_arity_2_int} is sat
    lone_4 : {lone_arity_3} is sat
    some_1 : {some_arity_1} is sat
    some_2 : {some_arity_2_atom} is sat
    some_3 : {some_arity_2_int} is sat
    some_4 : {some_arity_3} is sat
    no_1 : {no_arity_1} is sat
    no_2 : {no_arity_2_atom} is sat
    no_3 : {no_arity_2_int} is sat
    no_4 : {no_arity_3} is sat
}

