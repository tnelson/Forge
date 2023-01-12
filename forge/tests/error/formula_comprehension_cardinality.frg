#lang forge
option run_sterling off

sig A {
    i: one Int
}

pred problem {
    -- Attempt to provide a (perfectly valid) formula to a comprehension
    -- The parser thinks this looks like a constraint block! Expander needs
    -- to prevent this from making it to AST-creation.
    #{all a: A | a.i > 0} > 1
}