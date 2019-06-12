from z3 import *

# SETUP
Node = DeclareSort('Node')
B = BoolSort()
Edge = Function('Edge', Node, Node, B)
s = Solver()
solver = s
A, B, C, D, E = Consts('A B C D E', Node)

name = 0

def Union(a, b):
    if (a.arity() != b.arity() or a.range() != BoolSort() or b.range() != BoolSort() or a.arity() == 0):
        print("error: arity mismatch")
    else:
        global name
        name += 1
        consts = []
        sorts = []
        for x in range(a.arity()):
            if a.domain(x) != b.domain(x):
                print("error: sort mismatch")
            else:
                consts.append(Const(str(x), a.domain(x)))
                sorts.append(a.domain(x))
        sorts.append(BoolSort())
        out = Function(name, *sorts)
        s.add(ForAll(consts, (Or(a(*consts), b(*consts)) == out(*consts))))
        return out


def Diff(a, b):
    if (a.arity() != b.arity() or a.range() != BoolSort() or b.range() != BoolSort() or a.arity() == 0):
        print("error: arity mismatch")
    else:
        global name
        name += 1
        consts = []
        sorts = []
        for x in range(a.arity()):
            if a.domain(x) != b.domain(x):
                print("error: sort mismatch")
            else:
                consts.append(Const(str(x), a.domain(x)))
                sorts.append(a.domain(x))
        sorts.append(BoolSort())
        out = Function(name, *sorts)
        s.add(ForAll(consts, And(a(*consts), Not(b(*consts))) == out(*consts)))
        return out

def Join(a, b):
    global name
    name += 1
    if (a.arity() == 0 or b.arity() == 0):
        print("Cannot join arity-0 relations")
    elif (a.domain(a.arity() - 1) != b.domain(0)):
        print("Sort mismatch, cannot join " + a.domain(a.arity() - 1) + " to " + b.domain(0))
    else:
        sortsA = []
        constsA = []
        sortsB = []
        constsB = []
        for x in range(a.arity() - 1):
            sortsA.append(a.domain(x))
            constsA.append(Const("A" + str(name) + str(x), a.domain(x)))
        for x in range(1, b.arity()):
            sortsB.append(b.domain(x))
            constsB.append(Const("B" + str(name) + str(x), b.domain(x)))

        joinsort = a.domain(a.arity() - 1)
        joinconst = Const("joinconst", joinsort)
        
        out = Function("joinout" + str(name), *(sortsA + sortsB + [BoolSort()]))
        solver.add(ForAll(constsA + constsB, out(*(constsA + constsB)) == Exists([X], And(a(*(constsA + [joinconst])), b(*([joinconst] + constsB))))))
        return out

def Intersect(a, b):
    global name
    name += 1
    if (a.arity() != b.arity() or a.range() != BoolSort() or b.range() != BoolSort() or a.arity() == 0):
        print("error: arity mismatch")
    else:
        consts = []
        sorts = []
        for x in range(a.arity()):
            if a.domain(x) != b.domain(x):
                print("error: sort mismatch")
            else:
                consts.append(Const("iconst" + str(name) + str(x), a.domain(x)))
                sorts.append(a.domain(x))
        sorts.append(BoolSort())
        out = Function("Intersectout" + str(name), *sorts)
        solver.add(ForAll(consts, (And(a(*consts), b(*consts)) == out(*consts))))
        return out

def Some(r):
    global name
    name += 1
    if r.arity() == 0:
        print("error: not a relation")
    elif r.arity() == 1:
        return False
    else:
        sorts = []
        consts = []
        for x in range(r.arity()):
            consts.append(Const("sconst" + str(name) + str(x), r.domain(x)))
            sorts.append(r.domain(x))
        return Exists(consts, r(*consts))

def Single(const):
    global name
    name += 1
    out = Function("Single" + str(name), const.sort(), BoolSort())
    name += 1
    temp = Const("Temp" + str(name), const.sort())
    s.add(ForAll([temp], Implies(out(temp), temp == const)))
    return out

# CARDINALITY CONSTRAINT
X = Const('X', Node)
s.add(ForAll([X], Or(X == A, X == B, X == C, X == D)))

# IDEN
Iden = Function('Iden', Node, Node, BoolSort())
s.add(ForAll([A, B], ((A == B) == Iden(A, B))))

# Transitive reflexive closure of edges relation
TCR_N = Function('TCR_N', Node, Node, BoolSort())
Connected = Function('Connected', Node, Node, Node, BoolSort())
s.add(ForAll([A, B], TCR_N(A, B) == Or(Connected(A, B, B), A == B)))

#AFun = Function(Node, BoolSort())
#s.add(ForAll([B], AFun(B) == (B == A)))

NonRef = Union(Diff(TCR_N, Iden), Edge)

# Transitive NONREFLEXIVE closure of edges relation (well, not necessarily reflexive)
# This will be generated from the reflexive closure,
TC_N = Function('TC_N', Node, Node, BoolSort())
s.add(ForAll([A, B], TC_N(A, B) == Or(NonRef(A, B), And(A == B, Some(Intersect(Join(Single(A), NonRef), Join(NonRef, Single(A))))))))

q = Connected(A, A, B) == False
w = Implies(And(Connected(A, B, D), Connected(B, C, D)), Connected(A, C, D))
e = Implies(And(Connected(A, B, B), And(Connected(B, C, C), A != C)), Connected(A, C, C))
r = Implies(And(Edge(A, B), A != B), Connected(A, B, B))
t = Implies(Connected(A, B, B), Exists([E], And(TCR_N(A, E), Connected(A, E, B))))
y = Implies(And(Connected(A, B, C), B != C), Connected(B, C, C))


s.add(ForAll([A, B, C, D], And(q, w, e, r, t, y)))

s.add(Edge(A, B))
s.add(Edge(A, D))
s.add(Not(Edge(A, C)))

s.add(Edge(B, A))
s.add(Not(Edge(B, C)))
s.add(Not(Edge(B, D)))

s.add(Edge(D, C))
s.add(Not(Edge(D, A)))
s.add(Not(Edge(D, B)))

s.add(Not(Edge(C, A)))
s.add(Not(Edge(C, D)))
s.add(Not(Edge(C, B)))


print(s.check())
m = s.model()
print(m)

print()

floop = [A, B, C, D]
names = ["A", "B", "C", "D"]

JoinTest = Join(Single(A), TCR_N)

for x in range(4):
    print("JoinTest(" + names[x] + ") = " + str(m.eval(JoinTest(floop[x]))))

print()

for x in range(4):
    for y in range(4):
        print("TC_N(" + names[x] + ", " + names[y] + ") = " + str(m.eval(TC_N(floop[x], floop[y]))))

print()

for x in range(4):
    for y in range(4):
        print("TCR_N(" + names[x] + ", " + names[y] + ") = " + str(m.eval(TCR_N(floop[x], floop[y]))))

print()

for x in range(4):
    for y in range(4):
        print("TCR_N(" + names[x] + ", " + names[y] + ") = " + str(m.eval(NonRef(floop[x], floop[y]))))

#print(s.check())
#print(s.model())
#print(s.unsat_core())
