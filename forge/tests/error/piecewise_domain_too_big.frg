#lang forge/bsl
option verbose 0
option run_sterling off
sig Person  { spouse: lone Person }

example should_error is { true } for {
    // Without this line, there aren't enough atoms in the default for the bindings below
    //Person = `Person0 + `Person1 + `Person2 + `Person3 + `Person4
    no `Person0.spouse
    no `Person1.spouse
    no `Person2.spouse
    `Person3.spouse = `Person4
    `Person4.spouse = `Person3
}
