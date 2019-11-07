
/*
Sum types can easily be represented with multiple vars. (foo)
No modification to Kodkod is needed.
However, a more concise representation is possible with extra boolVars.
*/


sig A {}
sig B {}
sig C {}

one sig Z { choose: A+B+C }
pred chosen[x: A+B+C] { Z.choose = x }

pred foo {
	some a: lone A, b: lone B, c: lone C {
		one a+b+c
		one a implies chosen[a]
		one b implies chosen[b]
		one c implies chosen[c]
	}
}

run foo for exactly 3 A, exactly 3 B, exactly 3 C	// 143 vars. 19 primary vars. 241 clauses. 4ms.


/*pred bar {
	some r: A->B->C {
		
	}
}*/
