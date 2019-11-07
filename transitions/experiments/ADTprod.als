

/*
Tuples/products can be represented as relations but it performs horribly. (bar)
They can easily be represented with multiple vars. (foo)
No modification to Kodkod is needed.
*/


sig A {}

pred foo {
	some a1, a2, a3, a4, a5: A {
		a1 != a2
		a1 != a3
		a1 != a4
		a1 != a5
		a2 != a3
		a2 != a4
		a2 != a5
		a3 != a4
		a3 != a5
		a4 != a5
	}
}

run foo for exactly 6 A	// 332 vars. 30 primary vars. 536 clauses. 7ms.

pred bar {
	some r: A->A->A->A->A {
		one r
		let a1 = r .A .A .A .A, 
			a2 = r .A .A .A [A],
			a3 = r .A .A [A][A],
			a4 = r .A [A][A][A],
			a5 = r [A][A][A][A]  {
			a1 != a2
			a1 != a3
			a1 != a4
			a1 != a5
			a2 != a3
			a2 != a4
			a2 != a5
			a3 != a4
			a3 != a5
			a4 != a5
		}
	}
}

run bar for exactly 6 A	// 94407 vars. 7776 primary vars. 164307 clauses. 1141ms.







