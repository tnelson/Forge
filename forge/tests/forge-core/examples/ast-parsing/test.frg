#lang froglet

sig Person {
    age : one Int
}

pred age_limits { 
    all p : Person | {p.age <= 5 and p.age > 2}
}

