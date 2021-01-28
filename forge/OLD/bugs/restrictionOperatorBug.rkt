#lang forge

-- I'm not sure if it's meant to be implemented, but <: and :> cause a weird
-- error: "match: no matching clause for [...]"
-- Commented out tests in:
--   - forge-alloy/expressions/expressionOperators.rkt

option verbose 0

sig Node {
    edges : set Node
}

pred LessColon {
    all n: Node |
        n.edges <: edges = n.edges->Node & edges
}

test expect restrictionOperatorBug {
    arityError : {not LessColon} is unsat
}


-- Also, confusing error for the following:

/*
pred ColonGreater {
    all n: Node |
        n.edges :> edges = {n1: Node, n2: Node | n1->n2 in edges and n2 in n.edges}
}
*/

-- The problem is that n.edges and edges are switched. The error message says that
-- the arity of the first argument must be 1, but that is because "a :> b" desugars into
-- "b <: a", so really it's saying arity of second argument must be 1.
