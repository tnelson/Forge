
sig S {
	move1: A -> S,
	move2: B -> C -> S
}
sig A {}
sig B {}
sig C {}

// one sig U {} // used for arity alignment

--------
--------

pred f[s, s': S, a: A] {
	s.move1[a] = s'
}

pred g[s, s': S, b: B, c: C] {
	s.move2[b][c] = s'
}


pred sApBC[a: lone A, b: lone B, c: lone C] {
	(one a) implies (no b+c) else pBC[b, c]
}
pred pBC[b: lone B, c: lone C] {
	(one b) and (one c)
}
run sApBC for 0 but exactly 2 A, exactly 2 B, exactly 2 C

pred h[s, s': S, a: lone A, b: lone B, c: lone C] {
	-- succint but slow:
	// one a->U+(b->c)	// looks like the ADT (ignore padding `->U`s)
	-- ugly but fast (avoid ->):
	sApBC[a, b, c]

	one a    implies f[s, s', a]
	one b->c implies g[s, s', b, c]

	-- neaten (these should be implemented with bounds, not preds):
	s != s'
	s.move1[A] + s.move2[B][C] = s'
	lone s.move1 
	lone s.move2
	(one s.move1) iff not (one s.move2)
	no (s'.move1[A] + s'.move2[B][C])
	// move1[A] + move2[B][C] = s->s'
	//S.move1[A]   = s->s'
	//C.(B.move2) = s->s'
	//no s'.move1
	//no s'.move2
	//#move1 + #move2 = 1
}

run h for exactly 2 S, exactly 2 A, exactly 2 B, exactly 2 C

--------
--------

/*pred trace {
	some s1, s2: S, 
}*/



/*
TODO:
- fix commented line arity mismatch issue (xor?)
- get one transition to work
- get trace to work as in g+w (state->(A+B*C) or edge->(A+B*C))
*/
