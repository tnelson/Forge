#lang forge 

-- Test the SMT-LIB Theory-of-Relations translation
option backend smtlibtor

-- Sterling should now get a workable instance, but leaving it off here since we are testing
-- soundness and completeness of the translation, not end-to-end functionality. 
option run_sterling off

-- Enable for more debugging output
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
        -- for every node, there is some in-edge
        -- TODO: not currently supported because of skolem restrictions
        --all n: Node | some n2: Node | n in n2.edges
        -- for every node, there are no in-edges
        no n: Node  | some n2: Node | n in n2.edges
        -- Should introduce a contradiction:
        some n: Node  | some n2: Node | n in n2.edges
    } is unsat
}

-- Currently issue w/ mixing run + tests; Forge is not waiting.
--run {}