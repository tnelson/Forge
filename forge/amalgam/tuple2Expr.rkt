#lang racket

#|
/*
The following is the code from the original tuple2Expr code from the Amalgam Visitor Helper in Java: 
    static public Expr tuple2Expr(A4Solution orig, A4Tuple tup) throws Err {
        Expr product = null;

        for(int ii=0;ii<tup.arity();ii++) {
            String name = tup.atom(ii);
            // DO NOT CALL THIS, see comment above
            // String name = orig.atom2name(tup.atom(ii));
            cacheTuple2ExprIfNeeded(orig, name);            
            Map<String, Expr> modelCache = tuple2ExprCache.get(orig);            
            if(modelCache.containsKey(name)) {
                if(product == null) product = modelCache.get(name);
                else product = product.product(modelCache.get(name));                
            } else {
                System.err.println("Error context: atoms="+orig.getAllAtoms()+" inst="+orig.debugExtractKInstance());
                throw new ErrorFatal("tuple2Expr: unable to find atomic sig in passed instance for (post atom2name): "+name); 
            }
        }         
        return product;            
    }


// We have a list of edges. This list contains tuples: (Node0, Node1, Node2). Say from here we want to check if
// Node0 in edges. Then, based on the recursive tree, we would need to get the upper bound of either of the relations
// edges or Node0. Say we get the upper bound of edges. The upper bound of edges is (if there are 5 nodes)((Node$0 Node$0),
//(Node$1 Node$1), (Node$2 Node$2),.... (Node$5 Node$5)). 
// Having this in mind, we want to go over each tuple of the upper bounds of edges and check whether the tuple that we're currently
//looking at is in the LHS expression (Node0) and in the RHS expression (edges).

// and we want to turn the list of tuples into a relation and then do
//the product of all elements in the relation to create an expression. In order to make them a relation, we can use
// (forge:Run-atom-rels foo5) --> (Node0 Node0). With this, we can build the product of the result of converting each atom to its
// corresponding relation from the list. Map within a filter 

//In order to understand how (forge:Run-atom-rels foo5) works, where 'foo5' is name of the command you're running, we can create
// a Forge program as an example. Tim suggested we can do this as a 
// input : '((Node0) (Node1) (Node2) (Node3) (Node4)))
//(define (tuple2Expr tupleList))
//`(`(node1) `(node2), ...)

//1. (forge:Run-atom-rels ex) --> (())
//1.a Map for everything in the list of tuples 
//2. Filter that (()) to only care for the things using the current x that we're looking at
//3. Create the product with all of these individual relations

|# 

sig Atom {edges: set Atom}

pred isDirectedTree (r: Atom -> Atom) {
    #| we want our tree to not contain cycles--> ANTI-REFLEXIVE
    -- reflexive: a binary relation R over a set X is reflexive if it relates every element of X to itself
    -- Transitive closure can be done with an adjacency matrix where we see
    // if a given node is reachable.
    -- there should not be an intersection between ourselves and everything that is reachable in the graph  |#
    no iden & ^r 
    
    #| we want our tree to be connected|#
    (Atom -> Atom) in *(r + ~r) 

    #| each child has only one parent
    -- we saw in forge 1: injectivity: every parent should only be pointing to one child
    -- to satisfy the 'in iden' part, each node should have at most one incoming edge |#
    r.~r in iden 
}

(run {isDirectedTree[edges]} for 7 Atom) 
(require racket/list)

