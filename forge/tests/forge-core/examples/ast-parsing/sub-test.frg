#lang froglet

sig Person { }

sig Pet {
    owner : lone Person
}

pred quantified_pre {
    all a : Person | some b1, b2 : Pet | b1.owner = a
}

pred quantified_post {
    all a : Person | some b1, b2 : Pet | b2.owner = a
}