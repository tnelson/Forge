from z3 import *

# SETUP
Node = DeclareSort('Node')
Edge = Function('Edge', Node, Node, BoolSort())
s = Solver()
solver = s
s.set(unsat_core=True)


A, B, C, D, E, F, G, H, I, J = Consts('A B C D E F G H I J', Node)
X, Y, Z = Consts('X Y Z', Node)
N = Int("N")

CC0 = ForAll([X], Or(X == A, X == B, X == C, X == D, X == E, X == F, X == G, X == H, X == I, X == J))

G = And(

Not(Edge(A, A)),
Edge(A, B),
Not(Edge(A, C)),
Not(Edge(A, D)),
Not(Edge(A, E)),
Edge(A, F),

Not(Edge(B, A)),
Edge(B, B),
Edge(B, C),
Not(Edge(B, D)),
Not(Edge(B, E)),
Not(Edge(B, F)),

Edge(C, A),
Not(Edge(C, B)),
Not(Edge(C, C)),
Not(Edge(C, D)),
Edge(C, E),
Not(Edge(C, F)),

Not(Edge(D, A)),
Not(Edge(D, B)),
Not(Edge(D, C)),
Edge(D, D),
Edge(D, E),
Not(Edge(D, F)),

Not(Edge(E, A)),
Not(Edge(E, B)),
Not(Edge(E, C)),
Not(Edge(E, D)),
Not(Edge(E, E)),
Not(Edge(E, F)),

Edge(F, A),
Not(Edge(F, B)),
Not(Edge(F, C)),
Edge(F, D),
Not(Edge(F, E)))

# Transitive closure of edges relation
TC = Function('TC', IntSort(), Node, Node, BoolSort())
TC0 = ForAll([I, X, Y], Implies(I < 1, Not(TC(I, X, Y))))
TC1 = ForAll([X, Y], TC(1, X, Y) == Edge(X, Y))
TC2 = ForAll([I, X, Y], Implies(And(I > 1, I <= 5), TC(I, X, Y) == Or(
					TC(I - 1, X, Y),
					Exists([Z], And(TC(I - 1, X, Z), Edge(Z, Y))))
))
TC3 = ForAll([I, X, Y], Implies(I > 5, Not(TC(I, X, Y))))


print(s.check(CC0, G, TC0, TC1, TC2))
print(s.unsat_core())
#print(s.model())
m = s.model()

floop = [A, B, C, D, E, F]
names = ["A", "B", "C", "D", "E", "F"]

for i in range(1, 6):
    for x in range(len(floop)):
        for y in range(len(floop)):
            print("TC(" + str(i) + ", " +  names[x] + ", " + names[y] + ") = " + str(m.eval(TC(i, floop[x], floop[y]))))

print()
