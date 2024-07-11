#lang forge

option backend smtlibtor 

option verbose 2

sig Person {
    parent : lone Person,
    age : one Int,
    friends : one Int
}

sig Animal {
    owner : lone Person
}


run {some p : Person | p.age > 10 and p.friends < 5} 


