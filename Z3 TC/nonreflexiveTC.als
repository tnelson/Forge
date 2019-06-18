sig Atom {
	edges: set Atom,
	--real_tc: set Atom,
	--irref: set Atom,
	--valid_reflexives: set Atom,
	TC_ref: set Atom,
	TC_nonref: set Atom
}

fact {
	-- This is the reflexive transitive closure; all reflexive pairs added in, although some
	-- may already have been present. This is what the Z3 algorithm gives us.
	TC_ref = *edges

	TC_nonref = edges.TC_ref

	-- nonref is built from TC_ref. We take away all reflexive pairs not in the original relation directly.
	---let nonref = (TC_ref - iden) + edges |
 		---all a, b: Atom |
			--(a->b) in TC_nonref iff ((a = b and (some a.nonref & nonref.a)) or (a->b) in nonref)
}

assert madeNonreflexiveTransitiveClosure {
	TC_nonref = ^edges
}

check madeNonreflexiveTransitiveClosure for 7
