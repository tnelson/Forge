#lang froglet

sig Person { }

sig Pet {
    owner : lone Person
}

pred sample_pet_substitution {
    all a : Person | some b : Pet | b.owner = a
}