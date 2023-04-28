#lang forge


/*
Finding traces with mutliple transition types using ADTs.
Double-dash comments show proposed syntax.
Project over Solution.
3 Solutions: 2+1, 1+2, 1+1+1.
*/

sig S {	stuff: univ }
sig A {}
sig B {}
sig C {}

--------
--------

pred f[s: S, s': S, a: A] {
	s'.stuff = s.stuff+a
	a not in s.stuff
}
pred g[s: S, s': S, b: B, c: C] {
	s'.stuff = s.stuff+b+c
	b not in s.stuff
	c not in s.stuff
}

pred sApBC[a: A, b: B, c: C] {
	(lone a) (lone b) (lone c)
	(one a) implies (no b+c) else pBC[b, c]
}
pred pBC[b: B, c: C] {
	(one b) (one c)
}
//run sApBC for 0 but exactly 2 A, exactly 2 B, exactly 2 C

pred h[s: S, s': S, a: A, b: B, c: C] {
	sApBC[a, b, c]
	(one a)   implies f[s, s', a]
	pBC[b, c] implies g[s, s', b, c]
}
//run h for exactly 2 S, exactly 2 A, exactly 2 B, exactly 2 C

--------
--------

pred initialState[s: S] {
	no s.stuff
}
pred finalState[s: S] {
	#(s.stuff) = 3
}
pred myInv[s: S] {
	s.stuff in (A+B+C)
}

one sig Solution {
	state: S,
	init, term: state,
	transition: (state-term) one->one (state-init),

	-- args: (state-term) -> (A+B*C)
	argA: (state-term) -> A,
	argB: (state-term) -> B,
	argC: (state-term) -> C
} {
	all s: state | myInv[s]
	initialState[init]
	finalState[term]
	all s: (state-term) {
		-- (f+g)[s, s.transition, s.args]
		-- h[s, s.transition, s.args]
		h[s, s.transition, s.argA, s.argB, s.argC]
	}
}
pred neaten {
	Solution.state = S
	Solution.argA[S] = A
	Solution.argB[S] = B
	Solution.argC[S] = C
}
run neaten