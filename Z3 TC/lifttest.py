from z3 import *

# SETUP
Node = DeclareSort('Node')
s = Solver()
solver = s

s.set(unsat_core=True)

'''
A, B, C, D, E, F = Consts('A B C D E F', Node)
N = Int("N")

# CARDINALITY CONSTRAINT
X, Y, Z = Consts('X Y Z', Node)
CC = ForAll([X], Or(X == A, X == B, X == C, X == D, X == E, X == F))
'''


A, B, C, D, E, F, G, H, I, J = Consts('A B C D E F G H I J', Node)
X, Y, Z = Consts('X Y Z', Node)
N = Int("N")

CC0 = ForAll([X], Or(X == A, X == B, X == C, X == D, X == E, X == F, X == G, X == H, X == I, X == J))
CC1 = Distinct(A, B, C, D, E, F, G, H, I, J)

# Basic edges relation
R = Function('R', Node, Node, BoolSort())

'''
GC = And(
    ForAll([X], R(A, X) == (X == B)),
    ForAll([X], R(B, X) == Or(X == C, X == D)),
    ForAll([X], R(C, X) == Or(X == C, X == E)),
    ForAll([X], R(D, X) == Or(X == B, X == D)),
    ForAll([X], R(E, X) == Or(X == B, X == F, X == G)),
    ForAll([X], Not(R(F, X))),
    ForAll([X], R(G, X) == Or(X == H, X == I)),
    ForAll([X], R(H, X) == (X == I)),
    ForAll([X], Not(R(I, X))),
    ForAll([X], R(J, X) == (X == J)))
'''

GC = And(
    ForAll([X], R(A, X) == (X == B)),
    ForAll([X], R(B, X) == Or(X == C, X == D)),
    ForAll([X], R(C, X) == Or(X == E)),
    ForAll([X], R(D, X) == False),
    ForAll([X], R(E, X) == Or(X == F, X == G)),
    ForAll([X], Not(R(F, X))),
    ForAll([X], R(G, X) == Or(X == H, X == I)),
    ForAll([X], R(H, X) == (X == I)),
    ForAll([X], Not(R(I, X))),
    ForAll([X], Not(R(J, X))))

SEARCHLEN = 9




# Takes out (A->B) and also (B->A)
DiffR = Function("DiffR", Node, Node, Node, Node, BoolSort())
Diff0 = ForAll([A, B, X, Y], DiffR(A, B, X, Y) == And(R(X, Y), Not(Or(
		And(X == A, Y == B),
		And(X == B, Y == A)))))

TC = Function("TC", IntSort(), Node, Node, Node, Node, BoolSort())

# so what constraints do I put on that?
TC0 = ForAll([N, A, B, X, Y], Implies(N < 1, Not(TC(N, A, B, X, Y))))
TC1 = ForAll([A, B, X, Y], TC(1, A, B, X, Y) == DiffR(A, B, X, Y))
TC2 = ForAll([N, A, B, X, Y], Implies(And(N > 1, N <= SEARCHLEN), TC(N, A, B, X, Y) == Or(
					TC(N - 1, A, B, X, Y),
					Exists([Z], And(TC(N - 1, A, B, X, Z), DiffR(A, B, Z, Y))))))

TC3 = ForAll([N, A, B, X, Y], Implies(N > SEARCHLEN, Not(TC(N, A, B, X, Y))))

# This is testing cyclicity
NOCYCLE = Not(Exists([A, B], And(R(A, B), TC(SEARCHLEN, A, B, A, B))))





'''
s.add(Not(R(A, A)))
s.add(R(A, B))
s.add(Not(R(A, C)))
s.add(Not(R(A, D)))
s.add(Not(R(A, E)))
s.add(R(A, F))

s.add(Not(R(B, A)))
s.add(R(B, B))
s.add(R(B, C))
s.add(Not(R(B, D)))
s.add(Not(R(B, E)))
s.add(Not(R(B, F)))

s.add(R(C, A))
s.add(Not(R(C, B)))
s.add(Not(R(C, C)))
s.add(Not(R(C, D)))
s.add(R(C, E))
s.add(Not(R(C, F)))

s.add(Not(R(D, A)))
s.add(Not(R(D, B)))
s.add(Not(R(D, C)))
s.add(R(D, D))
s.add(R(D, E))
s.add(Not(R(D, F)))

s.add(Not(R(E, A)))
s.add(Not(R(E, B)))
s.add(Not(R(E, C)))
s.add(Not(R(E, D)))
s.add(Not(R(E, E)))
s.add(Not(R(E, F)))

s.add(R(F, A))
s.add(Not(R(F, B)))
s.add(Not(R(F, C)))
s.add(R(F, D))
s.add(Not(R(F, E)))
s.add(Not(R(F, F)))
'''


# Wait it's saying there are no cycles. But there is a cycle!!!!
print(s.check(CC0, CC1, GC, Diff0, TC0, TC1, TC2, TC3, NOCYCLE))
print(s.unsat_core())
#print(s.model().sexpr())
#print(s.model())

'''
floop = [A, B, C, D, E, F]
names = ["A", "B", "C", "D", "E", "F"]

for x in range(6):
    for y in range(6):
        print("TC(" + names[x] + ", " + names[y] + ") = " + str(m.eval(TC(floop[x], floop[y]))))

print()
'''
