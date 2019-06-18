from z3 import *

def MakeJoin(a, b, solver, name):
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
            constsA.append(Const("A" + str(x), a.domain(x)))
        for x in range(1, b.arity()):
            sortsB.append(b.domain(x))
            constsB.append(Const("B" + str(x), b.domain(x)))

        joinsort = a.domain(a.arity() - 1)
        joinconst = Const("joinconst", joinsort)
        
        out = Function(name, *(sortsA + sortsB + [BoolSort()]))
        solver.add(ForAll(constsA + constsB, out(*(constsA + constsB)) == Exists([X], And(a(*(constsA + [joinconst])), b(*([joinconst] + constsB))))))
        return out

def MakeIntersect(a, b, solver, name):
    if (a.arity() != b.arity() or a.range() != BoolSort() or b.range() != BoolSort() or a.arity() == 0):
        print("error: arity mismatch")
    else:
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
        solver.add(ForAll(consts, (And(a(*consts), b(*consts)) == out(*consts))))
        return out

def Some(r, solver):
    if r.arity() == 0:
        print("error: not a relation")
    elif r.arity() == 1:
        return False
    else:
        sorts = []
        consts = []
        for x in range(r.arity()):
            consts.append(Const(str(x), r.domain(x)))
            sorts.append(r.domain(x))
        return Exists(consts, r(*consts))

def Singleton(const, name):
    out = Function(name, const.sort(), BoolSort())
    return out

# SETUP
Node = DeclareSort('Node')
B = BoolSort()
Edge = Function('Edge', Node, Node, B)
s = Solver()
A, B, C, D, E = Consts('A B C D E', Node)

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

# Transitive NONREFLEXIVE closure of edges relation (well, not necessarily reflexive)
# This will be generated from the reflexive closure,
TC_N = Function('TC_N', Node, Node, BoolSort())
s.add(ForAll([A], TC_N(A, A) == Some(MakeIntersect(MakeJoin(Singleton(A, "sing"), TCR_N, s, "boi"), MakeJoin(TCR_N, Singleton(A, "sin"), s, "boyo"), s, "inty"), s)))

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
print(m.eval(TC_N(A, A)))
print(m.eval(TC_N(A, B)))
print(m.eval(TC_N(A, C)))
print(m.eval(TC_N(A, D)))
print(m.eval(TC_N(B, A)))
print(m.eval(TC_N(B, B)))
print(m.eval(TC_N(B, C)))
print(m.eval(TC_N(B, D)))
print(m.eval(TC_N(C, A)))
print(m.eval(TC_N(C, B)))
print(m.eval(TC_N(C, C)))
print(m.eval(TC_N(C, D)))
print(m.eval(TC_N(D, A)))
print(m.eval(TC_N(D, B)))
print(m.eval(TC_N(D, C)))
print(m.eval(TC_N(D, D)))


#print(s.check())
#print(s.model())
#print(s.unsat_core())
