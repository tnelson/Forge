#lang forge
option run_sterling off
option verbose 0

one sig A { field: one B}
abstract sig B{
   
}

test expect {
 
    thm1: {some A.field} is sat
}