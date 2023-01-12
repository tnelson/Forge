#lang forge
option run_sterling off

sig Person {
    age: one Int
}

example onlyBabies is {some p: Person | p.age < 3 and p.age >= 0} for {
    Person = `Person0 + `Person1
    no age
}