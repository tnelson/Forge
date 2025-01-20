#lang forge 

-- Test the SMT-LIB Theory-of-Relations translation
option backend smtlibtor

-- Sterling should now get a workable instance, but leaving it off here since we are testing
-- soundness and completeness of the translation, not end-to-end functionality. 
-- option run_sterling off

-- Enable for more debugging output
option verbose 5

sig Node {edges: set Node}

-- Test various quantifier patterns at skolem depth = 0
-- TODO: re-enable
 expect {
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


/******************************************************************************/

sig WeightedGraphNode {
    weightedEdges: set WeightedGraphNode -> Int
}
fun connectivity: set WeightedGraphNode -> WeightedGraphNode {
    weightedEdges.Int
}

-- Test Int in relation
test expect {
    {
        -- Non-immediate connection between 2 different nodes
        -- some disj w1, w2: WeightedGraphNode | w1 in w2.^connectivity and w1 not in w2.connectivity
        -- ^ For now, need single decl per quantifier
        some w1: WeightedGraphNode | some w2: WeightedGraphNode | w1 in w2.^connectivity and w1 not in w2.connectivity
    } is sat
    {
        -- force a contradiction, confirm unsat
        some w1: WeightedGraphNode | some w2: WeightedGraphNode | {
            w1 in w2.^connectivity and w1 not in w2.connectivity
            w1 not in w2.^connectivity 
        }
    } is unsat

    -- Check that "unboxing" integers from relations functions
    {
        some w1: WeightedGraphNode | some w2: WeightedGraphNode | 
          (w1.weightedEdges)[w2] != (w2.weightedEdges)[w1]
    } is sat 
    
}


-- test some patterns with Skolem-depth > 0

test expect {
    {
        -- for every node, there is some in-edge
        all n: Node | some n2: Node | n in n2.edges
    } is sat
    {
        -- contradiction
        all n: Node | some n2: Node | n in n2.edges
        not { all n: Node | some n2: Node | n in n2.edges }
    } is sat
}


-- Currently issue w/ mixing run + tests; Forge is not waiting.
// run {
//     some w1: WeightedGraphNode | some w2: WeightedGraphNode | {
//           (w1.weightedEdges)[w2] != (w2.weightedEdges)[w1]
//           some (w1.weightedEdges)[w2]
//           some (w2.weightedEdges)[w1]
//     }
// }