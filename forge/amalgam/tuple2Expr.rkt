#lang forge

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
*/

// We have a list of tuples ((Node0 Node1) (Node1 Node2)) and we want to turn the list of tuples into a relation and then do
//the product of all elements in the relation to create an expression. In order to make them a relation, we can use
// (forge:Run-atom-rels foo5). With this, we can build the product of the result of converting each atom to its
// corresponding relation from the list.

//In order to understand how (forge:Run-atom-rels foo5) works, where 'foo5' is name of the command you're running, we can create
// a Forge program as an example. 

//(define (tuple2Expr tupleList))
