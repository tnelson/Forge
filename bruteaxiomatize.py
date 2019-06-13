from z3 import *

# On 5 nodes, this takes about: 1 minute, and correct
# On 6 nodes, takes at least one hour, and more than one gigabyte.


# SETUP
Node = DeclareSort('Node')
Edge = Function('Edge', Node, Node, BoolSort())
s = Solver()
solver = s
A, B, C, D, E, F = Consts('A B C D E F', Node)

# CARDINALITY CONSTRAINT
X, Y = Consts('X Y', Node)
s.add(ForAll([X], Or(X == A, X == B, X == C, X == D, X == E, X == F)))


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
