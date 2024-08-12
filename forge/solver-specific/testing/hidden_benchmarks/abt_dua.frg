#lang forge

option backend smtlibtor

//---------------------------------------------------------------------------------------
/*
 * A transitive closure based axiomatization of a "connex" order.
 * Recall that, a binary relation R in S -> S is called connex if: 
 * (1) all a, b:S | a->b in R or b->a in R or a = b.
 * Unlike total order, connex order do not require identical elements to stay in relation.
 *
 * One can prove that (S, ^nextS) is a connex order.
 *
 * author: Aboubakr Achraf El Ghazi
*/

// Orderd signatures: Book 

one sig firstS in Book {}
one sig lastS in Book {}

pred order_facts {
 all s: Book | s !in s.^nextS
 all s: Book | s = firstS or s in firstS.^nextS
 // Only for the finite case
 no lastS.nextS
}

/** first */
fun first: one Book { firstS }

/** last */
fun last: one Book { lastS }

/** return a mapping from each element to its predecessor */
fun prev : Book->Book { ~(nextS) }

/** return a mapping from each element to its successor */
fun next : Book ->Book { nextS }

/** return elements prior to e in the ordering */
fun prevs [s: Book]: set Book { s.^(~(nextS)) }

/** return elements following e in the ordering */
fun nexts [s: Book]: set Book { s.^(nextS) }

//---------------------------------------------------------------------------------------



abstract sig Target { }
sig Addr extends Target { }
abstract sig Name extends Target { }

sig Alias, Group extends Name { }

// new declaration of the orderd signature
sig Book {
	names: set Name,
	addr: set Name -> Target,
     // For the finite case use lone instead of one
     // Also to check with AA (w.r.t a scope) use lone 
     // otherwise the model itself is unsat
     nextS: lone Book
} 
pred book_facts {
	all b : Book | {
		no n: Name | n in n.^(b.addr)
		all a: Alias | lone a.(b.addr)
		all n : Name | n in b.names => some n.(b.addr) else no n.(b.addr)
	}
}


pred add [b, b1: Book, n: Name, t: Target] {
	t in Addr or some lookup [b, Name&t]
	b1.addr = b.addr + n->t
}

pred del [b, b1: Book, n: Name, t: Target] {
	no b.addr.n or some n.(b.addr) - t
	b1.addr = b.addr - n->t
}

fun lookup [b: Book, n: Name] : set Addr { n.^(b.addr) & Addr }

pred init [b: Book]  { no b.addr }


// Original trace spesification
/*
fact traces {
	init [first]
	all b: Book-last |
	//all b: Book |
	  let b1 = b.nextS |
	    some n: Name, t: Target |
	      add [b, b1, n, t] or del [b, b1, n, t]
}
*/
// New trace spesification
pred traces1 {
	init [first]
}
pred traces2 {
	all b, b1: Book |
        b1 in b.nextS =>
	    (some n: Name, t: Target | add [b, b1, n, t] or
                                                 del [b, b1, n, t])
}

pred model_facts {
    order_facts and book_facts and traces1 and traces2
}


------------------------------------------------------

pred delUndoesAdd {
	all b, b1, b2: Book, n: Name, t: Target |
        b in firstS.*nextS => {
		  no n.(b.addr) and add [b, b1, n, t] and del [b1, b2, n, t]
		  implies
		  b.addr = b2.addr
       }
}

// This should not find any counterexample.
test expect {
    ab_dua : {model_facts => delUndoesAdd} is theorem
}

