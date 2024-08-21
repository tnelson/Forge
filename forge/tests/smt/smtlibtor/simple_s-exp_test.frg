#lang forge

option backend smtlibtor 

sig Person {
    parent : set Person 
}

run {} 