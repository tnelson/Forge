#lang forge
one sig Person { age: one Int }
// REGRESSION TEST:
// This should produce an error, since "- 1" shouldn't be a _number_ in Forge.
// (Caused by the fact that the lexer removes spaces, and just passes a minus token and a 1 token.)
test expect {
    parsing_should_error: {
        (Person.age <- 1)
        iff
        (Person.age < -1)} is checked
}
