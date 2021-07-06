#lang forge/check-ex-spec "forge1/directed_tree" "anon@gmail.com"

/*
--     A0
--    /  \
--   A1  A2
--  /      \
-- A3      A4
--First test a general tree
--it passes the wheat - good start
example twoBranches is isDirectedTree[edges] for {
	Atom = `A0 + `A1 + `A2 + `A3 + `A4
	edges = `A0->`A1 + `A0->`A2 + `A1->`A3 + `A2->`A4
}

--can trees be empty?
--passes the wheat
--it's a valid test :))
--tells the student that the empty Tree should be allowed
--catches a chaff too
example emptyTree is isDirectedTree[edges] for {
	no Atom
}

--no Atom caught a chaff - how about one Atom?
--catches another chaff - good start testing by the student
example oneTree is isDirectedTree[edges] for {
	one Atom
}

--can trees contain cycles?
--fails the wheat
--is an invalid test because Directed Trees cannot contain a cycle
--tells the student to disallow cycles
example containsCycle is isDirectedTree[edges] for {
	Atom = `A0 + `A1 + `A2 + `A3
	edges = `A0->`A1 + `A0->`A2 + `A0->`A3 + `A1->`A2
}

--Since containsCycle was invalid, allowing cycles is a possible bug
--good to test for possible bugs
--indeed catches a chaff
example noCycle is not isDirectedTree[edges] for {
	Atom = `A0 + `A1 + `A2 + `A3
	edges = `A0->`A1 + `A0->`A2 + `A0->`A3 + `A1->`A2
}

--It should be a tree, not a forest
--another possible bug - important to test for
--catches a chaff
example forest is not isDirectedTree[edges] for {
	Atom = `A0 + `A1 + `A2 + `A3 + `A4
	edges = `A0->`A1 + `A2->`A3 + `A2->`A4
}
*/
