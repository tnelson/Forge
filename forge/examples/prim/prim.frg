#lang forge/temporal
option max_tracelength 10

/*
Prim's algorithm in Forge
  Tim, 2021; revised to use Temporal Forge in 2024

  For visualization, use Table View and open the "Time" side drawer to step
  through the trace; you should see a new Node being added to Prim.pnodes every step.
  
  Note that this model will take around a minute to run. Some of the 
  (passing) tests are somewhat inefficient.

*/

-------------------------------------------------------------
-- Always have a specific weighted directed graph in the background
-------------------------------------------------------------

sig Node {
    edges: set Node -> Int
}

pred wellformedgraph {
    -- no differently-valued edges between the same nodes
    all n, m: Node | lone edges[n][m] 
    
    -- no negative weights
    all n, m: Node | some edges[n][m] implies edges[n][m] >= 0 
    
    -- This isn't strong enough (it says: symmetric, **without** regard to weights):
    -- edges.Int = ~(edges.Int)
    -- This is strong enough (symmetric, with same weights required):
    all n, m: Node | edges[n][m] = edges[m][n]

    -- no self-loops
    no (iden & edges.Int)

    -- connected (or else it cannot be spanned)
    all disj n, m: Node | n in m.^(edges.Int)
}

-- Demo checking that the weight-aware symmetry constraint subsumes the other.
pred test_old { edges.Int = ~(edges.Int) }
pred test_new { all n, m: Node | edges[n][m] = edges[m][n] }
assert test_new is sufficient for test_old for 5 Node, 5 Int

-------------------------------------------------------------
-- Prim's algorithm
-------------------------------------------------------------

one sig Prim {
    var pnodes: set Node,
    var ptree: set Node->Node
}

pred prim_init {
    one Prim.pnodes -- one arbitrary node picked to start
    no Prim.ptree   -- no spanning tree edges yet
}

-- Intermediate steps represented as helper predicates

-- The set of possible nodes to expand the tree to next, along with costs
fun candidatesWithWeights: set Node -> Int { 
    ((Node-Prim.pnodes) -> Int) & Prim.pnodes.edges
}
-- The cheapest cost among all candidates
fun minCandidateWeight: set Int { 
    min[candidatesWithWeights[Node]]
}
-- The candidates who share that cheapest cost
-- (Note how you can use a helper function as a relation to join on!)
fun minWeightCandidates: set Node {
  candidatesWithWeights.minCandidateWeight 
}

-- A little code duplication here, but it enables a richer validation below
pred prim_step_enabled[m, n: Node] {
    m in Prim.pnodes             -- source node is in the set
    n in minWeightCandidates     -- destination node is a minimal hop away
    -- perhaps there's a more efficient way to do this line?
    m->n->minCandidateWeight in edges  -- there's an actual edge at this weight
}
pred prim_step[m, n: Node] {
    -- GUARD
    prim_step_enabled[m, n]
    -- ACTION/FRAME for pnodes
    Prim.pnodes' = Prim.pnodes + n 
    -- ACTION/FRAME for ptree (remember to add both symmetric edges!)
    Prim.ptree' = Prim.ptree + (m -> n) + (n -> m)
}

pred prim_doNothing {
    -- GUARD
    -- we cannot make progress using a Prim step -- DO NOT CONFUSE this with prim_step
    all m_no, n_no: Node | not prim_step_enabled[m_no, n_no]
    -- ACTION
    Prim.pnodes' = Prim.pnodes
    Prim.ptree' = Prim.ptree
}


-----------------------------------------------
-- Run!
-----------------------------------------------

pred prim_trace {
    wellformedgraph  -- require a well-formed graph
    prim_init        -- start in an initial state
    
    -- always step forward using these transitions
    always { 
        some m, n: Node | { prim_step[m, n] }
        or 
        prim_doNothing 
    }
}

-------------------------------------------------------------
-- View a run!
-------------------------------------------------------------

--run prim_trace for exactly 5 Node, 5 Int

-------------------------------------------------------------
-- Model Validation
-------------------------------------------------------------

-- "Optional" predicates focusing on wellformedgraph, since our criterion for 
-- finishing Prim's is that all the nodes are in the spanning tree. We want to
-- check that certain shapes are possible *AND* that Prim's can run on those shapes.
-- (The below combines these checks, since prim_trace includes wellformedgraph)

-- Find a graph where all the edges are different lengths. 
-- A broken version of this might be:
pred difflengthedges_broken {
    all disj n1, n2: Node | all disj m1, m2: Node | { 
        (n1->m1 in edges.Int implies n1.edges[m1] != n2.edges[m2])
    }
}
-- Note how this breaks. Consider what happens if n1=m2 and m1=n2.

-- Instead, here are two alternative fixes (of many):
pred difflengthedges {
    all disj n1, n2: Node | all disj m1, m2: Node | { 
        (n1 != m2) implies
        (n1->m1 in edges.Int implies n1.edges[m1] != n2.edges[m2])
    }
}
pred difflengthedges_2 {
    all disj n1, n2: Node | all disj m1, m2: Node | { 
        #(n1 + m1 + n2 + m2) >= 3 implies 
          (n1->m1 in edges.Int implies n1.edges[m1] != n2.edges[m2])
    }
}
// Are 2 of these options equivalent? If yes, our confidence increases:
test expect {
  {wellformedgraph implies {difflengthedges iff difflengthedges_2}}
    for 5 Node is theorem
}

-- Find a graph where all possible edges are present (at some weight)
pred complete_graph {
    all disj n1, n2: Node | {
        n1->n2 in edges.Int
    }
}

-- Run most of these on arbitrary 5-node graphs to force Forge to try more realistic cases.
test expect {    
   localize_a: {wellformedgraph and difflengthedges} 
      for exactly 5 Node, 5 Int is sat

    -- Make sure we can generate and run Prim's on graphs with different/same weights
    prims_difflengths_5: {prim_trace and difflengthedges} 
      for exactly 5 Node, 5 Int is sat
    prims_samelengths_5: {prim_trace and not difflengthedges} 
      for exactly 5 Node, 5 Int is sat

    -- Make sure we can generate and run Prim's on graphs of 1 and 2 nodes 
    -- (*Don't* expect 0-node graphs to work.)
    prims_single_node: {prim_trace} 
      for exactly 1 Node, 5 Int is sat  
    prims_double_node: {prim_trace} 
      for exactly 2 Node, 5 Int is sat  

    -- Make sure we can generate and run Prim's on complete/incomplete graphs
    prims_complete_graph_5: {prim_trace and complete_graph}
      for exactly 5 Node, 5 Int is sat
    prims_non_complete_graph_5: {prim_trace and not complete_graph}
      for exactly 5 Node, 5 Int is sat
}

-- These are great, but...
-- We'd really love to check that Prim's can actually run on _every_ possible 
-- non-empty graph. The problem is that Temporal Forge only finds lasso traces,
-- so "can actually run" must fit into a lasso trace. This is why doNothing's 
-- guard is just "prim_step isn't enabled"; we want to allow even buggy lasso traces! 

test expect {
  doNothing_not_used_buggily: {
    -- Can we find a Prim trace where doNothing is used before the full tree is explored?
    prim_trace
    eventually {prim_doNothing and Prim.pnodes != Node}
  } for 5 Node, 5 Int 
  -- Hopefully not!
  is unsat
}



-------------------------------------------------------------
-- Requirements
-------------------------------------------------------------

-- (1) The spanning tree generated actually spans the graph, and 
--     uses only valid edges 
pred current_is_spanning {
  -- Spans
  all disj n1, n2: Node | n1 in n2.^(Prim.ptree)
  -- Valid edges only
  Prim.ptree in edges.Int
}
-- (2) The spanning tree generated is a tree
pred current_is_tree {
  -- This is subtle to express for _undirected_ trees, since Forge represents them 
  -- as symmetric directed trees. We'll say "whenever two nodes are directly 
  -- connected, removing that connection removes connectivity between them"
  all disj n1, n2: Node | n2 in n1.(Prim.ptree) implies {
    n2 not in n1.^(Prim.ptree - (n1->n2 + n2->n1))
  }
  -- Note that the symmetry is vital for this predicate to work. Otherwise, 
  -- consider N0 -> N1 -> N2 -> N0. 
}
-- Note: make sure not to separate these into different time indexes:
--  "eventually current_is_spanning and eventually current_is_tree"
--  would allow starting as a tree but ending as a DAG!
pred req_eventually_is_spanning_tree {
    eventually {current_is_spanning and current_is_tree }
}
assert prim_trace is sufficient for req_eventually_is_spanning_tree
    for 5 Node, 5 Int

-------------------------------------------------------------------------------

-- (3) the spanning tree generated is minimal 

-- Minimality is much harder to express for the moment. The original model
-- this was taken from ran Kruskal's algorithm too, and compared the total
-- weight of the results: a variation of Property-Based Testing! 

-- ...but let's try to express minimality, anyway. 
-- First, we need to pull the separate criteria into a predicate we can 
-- check _other_ trees on. We'll copy from above, with minor edits:
pred isSpanningTree[t: set Node -> Node] {
  -- is spanning:
  all disj n1, n2: Node | n1 in n2.^t
  -- is tree:
  all disj n1, n2: Node | n2 in n1.t implies {
    n2 not in n1.^(t - (n1->n2 + n2->n1))
  }
  -- Since "t" is an arbitrary relation, and this definition of "tree" is for 
  -- the symmetric encoding of undirected trees, we need to require symmetry:
  t = ~t
  -- valid edges:
  t in edges.Int
}
-- and we need to measure the weight of a given edge set: 
fun totalWeight[eset: set Node -> Node]: one Int {
  sum n1: Node | sum n2: Node | {
    n1->n2 in eset =>   edges[n1][n2]
                   else 0
  }
}

-- We might want to write a predicate like this, but it involves higher-order 
-- universal quantification, and anyway Forge will reject this Alloy-style syntax: 
/*
pred current_is_minimal_st {
  -- Current state of Prim's is a spanning tree
  isSpanningTree[Prim.ptree]  
  -- For all _other_ trees... (FORGE WILL NOT SUPPORT THIS SYNTAX!)
  all t2: set Node -> Node | {
    isSpanningTree[t2] implies totalWeight[t2] <= totalWeight[Prim.ptree]
  }
}
*/

-- But verification is different. What happens if we try to flip the scenario, and 
-- search for a counterexample? Forge, like Alloy, will handle higher-order existentials,
--  although we need to create a helper relation ourselves, rather than using Alloy-style syntax.

-- Find a counterexample, i.e., a better spanning tree:
one sig Helper {
  otherTree: set Node -> Node
}
pred counter_example_to_req_prim_is_minimal {
  -- Finish running Prim's
  prim_trace 
  -- But we also have some other tree that is better. 
  eventually { 
    -- Setup to avoid overflow, need to count much higher than edge weights can be
    totalWeight[Helper.otherTree] > 0
    totalWeight[Prim.ptree] > 0      
    all n1, n2: Node | n1->n2 in edges.Int implies edges[n1][n2] <= 2
    
    -- Prim's is done at this point
    isSpanningTree[Prim.ptree]
    -- Other relation is a tree, spans, and is lower cost
    isSpanningTree[Helper.otherTree]
    totalWeight[Helper.otherTree] < totalWeight[Prim.ptree]
  }
}
-- This takes longer to run than I am willing to wait. So we can express the goal, 
-- but it is not compiled/solved efficiently. Some of the problem is symmetry, but also
-- the handling of integers for counting...
// test expect {
//   no_ce: {counter_example_to_req_prim_is_minimal} for 5 Node, 5 Int is unsat
// }
-- Fortunately, we can recast this using an optimizer instance to limit edge 
-- weights, rather than a constraint. Let's try it:
test expect {
  no_ce_optimized: {counter_example_to_req_prim_is_minimal} 
    for 5 Node, 5 Int 
    for {
      Node = `Node0 + `Node1 + `Node2 + `Node3 + `Node4
      -- Allow only 0, 1, and 2 as edge weights
      edges in Node -> Node -> (0+1+2)
    }
    is unsat
}
-- With the optimizer instance, the test passes in about 5 minutes.

