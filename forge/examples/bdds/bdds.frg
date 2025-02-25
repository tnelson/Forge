#lang forge

/*
  Model of ROBDDs
  Tim Nelson (December 2024)

  Reduced Ordered Binary Decision Diagrams (often just called BDDs) are a data structure for 
  representing boolean formulas. They have a remarkable property: with respect to a fixed 
  variable ordering, the BDD for a formula is canonical up to logical equivalence. That is, 
  there is exactly one BDD for each truth table. 

  We represent multiple BDDs via reference to multiple roots. We can thus avoid a `BDD` sig. 
*/

/** A boolean variable */ 
sig Variable {}

abstract sig Node {}
/** An internal BDD node: branch on the given variable. */
sig Split extends Node {
    v: one Variable,
    t: one Node,
    f: one Node
}
/** Not `one`; allow duplicate True, False nodes in the overall model to show reduction. */
sig True, False extends Node {}

fun tree[root: Node]: set Node {
    -- We use ^ and + here rather than * to help Forge infer the correct type; 
    -- reflexive transitive closure (*) includes the identity on, say, Int.
    root.^(t+f) + root
}

/** Wellformedness: is this a binary decision diagram? (It may still be unordered or not reduced.) */
pred is_bdd[root: Node] {
    // The root has no parents
    no root.(~t + ~f)
    // There are no cycles reachable from this root (including to the root itself). 
    all n1, n2: Split & tree[root] | n1 in n2.^(t+f) implies n2 not in n1.^(t+f)
}

/** Is the diagram ordered with respect to a particular variable ordering? */
pred is_ordered[root: Node] {
    // There is an ordering of variables that the induced node-ordering of t, f respects. 
    // We won't make this explicit, but rather will say that any time there is reachability 
    // from n1 to n2, no other path in the tree exists with the variables flipped.
    all disj n1, n2: Split & tree[root] | n2 in n1.^(t+f) => {
        no m1, m2: Split & tree[root] | {
            m2 in m1.^(t+f)
            m1.v = n2.v
            m2.v = n1.v
        }
    }
}

/** The reduction condition: given this, subject to an ordering, BDDs are canonical. */
pred is_reduced[root: Node] {
    // (Criterion 1) No node has the same t-child and f-child
    all s: Split & tree[root] | s.t != s.f

    // (Criterion 2) No 2 nodes are roots of isomorphic subgraphs. We'll encode this in a way that 
    // doesn't require describing an isomorphism. Instead, we'll describe the condition locally.
    // By induction (outside of Forge, on reverse tree depth), these together mean that the projected 
    // truth table for any 2 nodes must differ in some way. 
    //   Leaf case: no duplicate terminal nodes.
    lone True
    lone False 
    //   Internal case: no two internal nodes point to same T/F children.
    all disj s1, s2: Split & tree[root] | {
        s1.t != s2.t or s1.f != s2.f
    }
}

pred is_robdd[root: Node] {
    is_bdd[root]
    is_ordered[root]
    is_reduced[root]
}

visualize_10node_1root: run { 
    some root: Node | is_robdd[root] and (all n: Node-root | n in root.^(t+f))
} for exactly 10 Node

-------------------
-- Examples
-------------------

/** Siddhartha's non-ordered BDD Example */
pred all_bdds_ordered { all n: Node | is_bdd[n] implies is_ordered[n] }
example not_ordered is {not all_bdds_ordered} for {
    Variable = `V0 + `V1 + `V2
    True = `True 
    False = `False 
    Split = `N0 + 
          `N1 + `N2 + 
          `N3 + `N4 
    Node = True + False + Split

    `N0.v = `V0 -- rank 0
    `N1.v = `V1 -- rank 1
    `N2.v = `V2 -- rank 1
    `N3.v = `V2 -- rank 2
    `N4.v = `V1 -- rank 2
    
    `N0.t = `N1 
    `N0.f = `N2
    
    `N1.t = `N3 
    `N1.f = True
    `N2.t = True 
    `N2.f = `N4
    
    `N3.t = True 
    `N3.f = False 
    `N4.t = False 
    `N4.f = True
}

-----------------------------------------------------------------------------------------
-- Validation
-----------------------------------------------------------------------------------------

pred connected[n1, n2: Node] {
    some common: Node | { 
        (n1 = common or n1 in common.^(t+f))
        (n2 = common or n2 in common.^(t+f))
    }
}

-- Since this model is meant to allow multiple BDDs in the same instance, let's confirm that is possible.
-- (We say the roots need to be internal nodes to avoid the trivial "False is a BDD" and "True is a BDD" response.)
sat_2_robdds: assert {
    some disj root1, root2: Split | {
        is_robdd[root1] 
        is_robdd[root2]
        not connected[root1, root2]
    }
} is sat for 5 Node

sat_2_bdds_one_reduced: assert {
    some disj root1, root2: Split | {
        is_bdd[root1] 
        is_bdd[root2]
        not connected[root1, root2]
        is_reduced[root1]
        not is_reduced[root2]
    }
} is sat for 5 Node

// option solver MiniSatProver
// option logtranslation 2
// option coregranularity 2
// option core_minimization rce

sat_2_obdds_different_orderings: assert {
    some disj root1, root2: Split | 
    some disj n1, n2, m1, m2: Split | { 
        is_bdd[root1] 
        is_bdd[root2] 
        not connected[root1, root2]
        is_ordered[root1]
        is_ordered[root2] 
        (n1+m1) in tree[root1]
        (n2+m2) in tree[root2]
        n1.v = n2.v
        m1.v = m2.v
        m1 in n1.^(t+f) 
        n2 in m2.^(t+f) -- reverse order between these variables
    }
} is sat for 7 Node


-----------------------------------------------------------------------------------------
// Now I have some things I want to confirm about BDDs (up to this definition).
-----------------------------------------------------------------------------------------

// Conjecture: the final non-terminal "tier" of the ROBDD must always have either 0 or 2 nodes, 
// as a consequence of the fact that no 2 nodes can represent the same boolean function, and 
// any branching done on the final variable will go directly to true/false, leaving room only 
// for <p> and <!p>. 

// If we express the final "tier" just using transpose, we will run into trouble: 
// The expression    True.~(t+f) + False.~(t+f)   might contain nodes for multiple variables, since 
// intermediate nodes can skip directly to True/False. 

// Instead, we need to say that there can be only 0, 1, or 2 nodes for the _final variable in 
// the ordering_. Say that, for any node in an ROBDD, if that node's variable appears in >2 nodes... 
final_nonterminal_012: assert all root: Split, n: Split | {
    n in tree[root]
    is_robdd[root] 
    #{n2: tree[root] | n2.v = n.v} > 2
// Then this can't be the final tier
} is sufficient for not_final_variable[root, n] for 8 Node

pred not_final_variable[root: Node, n: Node] {
    some upper, lower: Split & tree[root] | {
        upper.v = n.v 
        lower in upper.^(t+f)
    }
}

final_nonterminal_012_vacuity: assert {
    some root: Node | {
        is_robdd[root]
        some n: tree[root] | #{n2: tree[root] | n2.v = n.v} > 2
    }
} is sat for 8 Node 

---------------------------------------------------------------------------------
-- Having defined the basic data structure, let's think about its semantics some.
--   It's not so straightforward to model this as with boolean logic, where we 
--   had a Valuation sig. A non-root node in a ROBDD is the root of its own ROBDD 
--   over a restricted variable set. We will model unmentioned variables as "don't 
--   care"s, and consider a root to satisfy a valuation if there exists a corresponding 
--   path to the true node (these are sometimes called "minterms").
---------------------------------------------------------------------------------

/** We will consider only one boolean valuation at a time. This will help avoid accidentally 
    universally quantifying over a Valuation sig, and running into problems because not every 
    one of the possible 2^n valuations is instantiated. */

one sig Semantics {
    valuation: set Variable,
    satisfies: set Node  
}
/** The valuation satisfies a node if... 
    (Assume there are no cycles.) */
pred wellformed_semantics {
    True in Semantics.satisfies 
    False not in Semantics.satisfies 
    all n: Split | n in Semantics.satisfies iff { 
        (n.v in Semantics.valuation and n.t in Semantics.satisfies)
        or
        (n.v not in Semantics.valuation and n.f in Semantics.satisfies)
    }
}

view_bdd_semantics: run {
    wellformed_semantics
    some Semantics.valuation 
    some root: Node | {
        is_robdd[root]
        -- Avoid including extraneous nodes in the example
        all n: Node - root | n in root.^(t+f)
    }
}

