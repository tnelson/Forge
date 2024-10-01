#lang forge

option backend smtlibtor

/** This is a model of an address book as described on Page 23 of the software abstractions book.
 * The model has the following interesting constructs:
 *  -- an multi-level type hierarchy
 *  -- abstract types
 *  -- relation multiplicity keywords
 *  -- relations with arities higher than 2
 *  -- transitive closure over a join expression with a ternary relation (n.^(b.addr))
 *  -- transitive closure in both sides of an equality (addLocal)
 *  -- quantifiers
 *
 * This model contains 2 unsatisfiable and 2 satisfiable assertions.
 */


abstract sig Target { }
sig Addr extends Target { }
abstract sig Name extends Target { }

sig Alias, Group extends Name { }

sig Book {
	names: set Name,
	addr: set Name -> Target // forall names, exists some Target
} 
pred book_facts {
	all b : Book | {
		no n: Name | n in n.^(b.addr)
		all a: Alias | lone a.(b.addr)
		all n : Name | n in b.names => some n.(b.addr) else no n.(b.addr)
	}
}

pred add [b, b_: Book, n: Name, t: Target] { b_.addr = b.addr + n->t }
pred del [b, b_: Book, n: Name, t: Target] { b_.addr = b.addr - n->t }
fun lookup [b: Book, n: Name] : set Addr { n.^(b.addr) & Addr }

pred addIdempotent {
	all b, b_, b__: Book, n: Name, t: Target |
		add [b, b_, n, t] and add [b_, b__, n, t]
		implies
		b_.addr = b__.addr
}

test expect {
    ab_ai: {book_facts => addIdempotent} is theorem
}
    


