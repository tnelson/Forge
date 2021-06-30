#lang forge

option problem_type temporal
option verbose 4
sig Node {
  var edges: set Node
}

run {
    all n1, n2: Node | lone n1.edges
    always edges != edges'
    -- test
    --edges'.edges' != (edges.edges)'   -- PARSE ERROR
    --(edges').(edges') != (edges.edges)' -- UNSAT
    -- [1] And yet, in the evaluator, edges'.edges' parses and produces something
    --    different from (edges').(edges'). What is it?
    -- different from ((edges').edges)'
    -- [2] Also, (edges'.edges)'  <--- "node-info: contract violation"
} for exactly 5 Node
