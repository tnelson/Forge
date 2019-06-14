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
CC1 = Distinct(A, B, C, D, E, F, G, H, I, J)

GC = And(
    ForAll([X], Edge(A, X) == (X == B)),
    ForAll([X], Edge(B, X) == Or(X == C, X == D)),
    ForAll([X], Edge(C, X) == Or(X == C, X == E)),
    ForAll([X], Edge(D, X) == Or(X == B, X == D)),
    ForAll([X], Edge(E, X) == Or(X == B, X == F, X == G)),
    ForAll([X], Not(Edge(F, X))),
    ForAll([X], Edge(G, X) == Or(X == H, X == I)),
    ForAll([X], Edge(H, X) == (X == I)),
    ForAll([X], Not(Edge(I, X))),
    ForAll([X], Edge(J, X) == (X == J)))


# Transitive closure of edges relation
TC = Function('TC', IntSort(), Node, Node, BoolSort())
TC0 = ForAll([N, X, Y], Implies(N < 1, Not(TC(N, X, Y))))
TC1 = ForAll([X, Y], TC(1, X, Y) == Edge(X, Y))
TC2 = ForAll([N, X, Y], Implies(And(N > 1, N <= 9), TC(N, X, Y) == Or(
					TC(N - 1, X, Y),
					Exists([Z], And(TC(N - 1, X, Z), Edge(Z, Y))))
))
TC3 = ForAll([N, X, Y], Implies(N > 9, Not(TC(N, X, Y))))


print(s.check(CC0, CC1, GC, TC0, TC1, TC2, TC3))
print(s.unsat_core())
#print(s.model())
m = s.model()

floop = [A, B, C, D, E, F, G, H, I, J]
names = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J"]

for i in range(1, 10):
    '''
    print("TC(" + str(i) + ", " + "G" + ", " + "F" + ") = " + str(m.eval(TC(i, G, F))))
    print("TC(" + str(i) + ", " + "H" + ", " + "F" + ") = " + str(m.eval(TC(i, H, F))))
    '''

    for x in range(len(floop)):
        for y in range(len(floop)):
            print("TC(" + str(i) + ", " +  names[x] + ", " + names[y] + ") = " + str(m.eval(TC(i, floop[x], floop[y]))))

print()
