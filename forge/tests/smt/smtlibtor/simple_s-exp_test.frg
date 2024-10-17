#lang forge

option backend smtlibtor 
option verbose 0

sig Person {
    parent : set Person 
}

test expect {
    vacuity : {} is sat
}