#lang forge

option backend smtlibtor 

// option run_sterling off

// option verbose 10

sig Person {
    parent : lone Person,
    age : one Int
}

sig Animal {
    owner : lone Person
}


run {all x : Int | some p : Person | p.age <= x} 