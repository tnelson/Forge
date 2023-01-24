#lang forge
option run_sterling off

sig Person {
    age: one Int
}

example onlyBabies is {some p: Person | p.age < 3} for {
    Person = `Person0
    no age
}
