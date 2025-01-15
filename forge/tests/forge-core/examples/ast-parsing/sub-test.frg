#lang forge/froglet

sig Person {
    age : one Int
 }

sig Pet {
    owner : lone Person
}

pred quantified_pre {
    all a : Person | some b1, b2 : Pet | b1.owner = a
}

pred quantified_post {
    all a : Person | some b1, b2 : Pet | b2.owner = a
}

pred int_pre {
    all a : Person | some b : Int | b > 0
}

pred int_post { 
    all a : Person | a.age > 0
}