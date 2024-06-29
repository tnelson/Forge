#lang forge 

-- Test the SMT-LIB Theory-of-Relations translation
option backend smtlibtor

-- Sterling should now get a workable instance
option run_sterling off

-- More debugging output
-- option verbose 5 

sig Node {edges: set Node}

test expect {
    {
        -- all reachable from n
        some n: Node | all n2: Node | n2 in n.^edges
        -- some node can't reach at least one other node
        some n: Node | some n2: Node | n2 not in n.^edges
    } is sat
    {
        -- all can reach n
        some n: Node | all n2: Node | n in n2.^edges
    } is sat
}

run {}