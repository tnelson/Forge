from z3 import *

# SETUP
Node = DeclareSort('Node')
Edge = Function('Edge', Node, Node, BoolSort())
s = Solver()
solver = s
A, B, C, D, E, F = Consts('A B C D E F', Node)

# CARDINALITY CONSTRAINT
X, Y = Consts('X Y', Node)
s.add(ForAll([X], Or(X == A, X == B, X == C, X == D, X == E, X == F)))


# OK so lifting should be fine in the simple case.
# Could get crazy later...
# OK so these lifted functions could be for the entire function we need to compute, not
# just one at a time.

# What's a use case?

# OK so we could just say for all x, some(x.R)

# The first A is the identifier, the second is the actual object in the relation
join0 = Function("join0", A, A, BoolSort())

# Do I put constraints at the top level, or in the quantifier where it's used?
# We could probably have a function to generate a constraint????
# Let's see how that works. Ok, it's fine with something like join.

s.add(ForAll([X], nodes in X.^(edge.Int)))


s.add(ForAll([X], ForAll([Y], X.^(edge.Int)(Y))))

What does this mean:
Y e X.^(edge.Int)

this means (X, Y) in ^(edge.Int), or ^(edge.Int)(X, Y)




OK so that doesn't work as an example. Here's another:

	not (some a, b: Node |
		((a -> b) in s.E) and ((a->b) in ^(s.E - (a->b))))

simpler:

Not(Exists([A, B], (A->B) in ^(E - (A->B))))
Not(Exists([A, B], ^(E - (A->B))(A, B)   ))

# OK so we're gonna have to build the transitive closure of E - (A->B)
# Do we have to? Can we just use the TC of E? Not worth trying to figure that out.
# What about using the iterative TC? Can that be used to solve this? Again,
# probably not worth it, that's implementation dependent.

# Conclusion: I need to actually generate the TC of E - (A->B)
# And that's dependent on what A and B actually are.
tc_E_minus_AB = Function("tc_E_minus_AB", Node, Node, Node, Node, BoolSort())

# so what constraints do I put on that?
TC0 = ForAll([N, A, B, X, Y], Implies(N < 1, Not(TC(N, A, B, X, Y))))
TC1 = ForAll([A, B, X, Y], TC(1, A, B, X, Y) == (E - (A->B))(X, Y))
TC2 = ForAll([N, A, B, X, Y], Implies(And(N > 1, N <= 9), TC(N, A, B, X, Y) == Or(
					TC(N - 1, A, B, X, Y),
					Exists([Z], And(TC(N - 1, A, B, X, Z), (E - (A->B))(Z, Y))))
))
TC3 = ForAll([N, A, B, X, Y], Implies(N > 9, Not(TC(N, A, B, X, Y))))

Not(Exists([A, B], E(A, B) and ^(E - (A->B))(A, B)   ))




TC0 = ForAll([X], )


#	all n: Node | Node in n.^(edges.Int)

R = Function("R", A, A, BoolSort())

s.add(ForAll([X], Exists([Y], R(X, Y))


Some(Join(X, R))))

#	let occ = FrontDesk.occupant {
#		-- The Guest had to have been an occupant at this time.
#		some occ.t.g


'''
Join is a relation for all functions.

Join takes two functions?

Join(f1, f2)(val) = True or false
'''


s.add(Not(Edge(A, A)))
s.add(Edge(A, B))
s.add(Not(Edge(A, C)))
s.add(Not(Edge(A, D)))
s.add(Not(Edge(A, E)))
s.add(Edge(A, F))

s.add(Not(Edge(B, A)))
s.add(Edge(B, B))
s.add(Edge(B, C))
s.add(Not(Edge(B, D)))
s.add(Not(Edge(B, E)))
s.add(Not(Edge(B, F)))

s.add(Edge(C, A))
s.add(Not(Edge(C, B)))
s.add(Not(Edge(C, C)))
s.add(Not(Edge(C, D)))
s.add(Edge(C, E))
s.add(Not(Edge(C, F)))

s.add(Not(Edge(D, A)))
s.add(Not(Edge(D, B)))
s.add(Not(Edge(D, C)))
s.add(Edge(D, D))
s.add(Edge(D, E))
s.add(Not(Edge(D, F)))

s.add(Not(Edge(E, A)))
s.add(Not(Edge(E, B)))
s.add(Not(Edge(E, C)))
s.add(Not(Edge(E, D)))
s.add(Not(Edge(E, E)))
s.add(Not(Edge(E, F)))

s.add(Edge(F, A))
s.add(Not(Edge(F, B)))
s.add(Not(Edge(F, C)))
s.add(Edge(F, D))
s.add(Not(Edge(F, E)))
s.add(Not(Edge(F, F)))

# Transitive closure of edges relation
TC = Function('TC', Node, Node, BoolSort())
s.add(ForAll([X, Y], TC(X, Y) == Exists(
	[A, B, C, D], And(
		Or(Edge(X, A), X == A),
		Or(Edge(A, B), A == B),
		Or(Edge(B, C), B == C),
		Or(Edge(C, D), C == D),
		#Or(Edge(D, E), D == E),
		Edge(D, Y)
	)
)))

print(s.check())
print(s.model())
m = s.model()

floop = [A, B, C, D, E, F]
names = ["A", "B", "C", "D", "E", "F"]

for x in range(6):
    for y in range(6):
        print("TC(" + names[x] + ", " + names[y] + ") = " + str(m.eval(TC(floop[x], floop[y]))))

print()
