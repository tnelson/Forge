#lang forge

option backend smtlibtor
option verbose 0

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
	addr: set Name -> Target
} 
/*{
	no n: Name | n in n.^addr
	all a: Alias | lone a.addr
}*/

pred model_facts {
  all b : Book | all n: Name | n in b.names <=> some n.(b.addr)
  all b: Book, n: Name | n !in n.^(b.addr)
  all b: Book, a: Alias | lone a.(b.addr)}

pred add [b, b_: Book, n: Name, t: Target] { b_.addr = b.addr + n->t }
pred del [b, b_: Book, n: Name, t: Target] { b_.addr = b.addr - n->t }
fun lookup [b: Book, n: Name] : set Addr { n.^(b.addr) & Addr }

pred delUndoesAdd {
	all b, b_, b__: Book, n: Name, t: Target |
		no n.(b.addr) and add [b, b_, n, t] and del [b_, b__, n, t]
		implies
		b.addr = b__.addr
}

// This should not find any counterexample.
test expect {
    ab_dua : {model_facts => delUndoesAdd} is theorem
}

