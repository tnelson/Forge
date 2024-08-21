#lang forge

option backend smtlibtor 

sig Person {
    parent : set Person 
}

test expect {
    vacuity : {} is sat
}