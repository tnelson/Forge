from z3 import *


'''
This took about half a second to run on 5 nodes, compared to one minute for brute force.

This took about 25 minutes to run for all six nodes. Here was hte output:

Join
sat
()
TCR_N(A, A) = True
TCR_N(A, B) = True
TCR_N(A, C) = True
TCR_N(A, D) = True
TCR_N(A, E) = True
TCR_N(A, F) = True
TCR_N(B, A) = True
TCR_N(B, B) = True
TCR_N(B, C) = True
TCR_N(B, D) = True
TCR_N(B, E) = True
TCR_N(B, F) = True
TCR_N(C, A) = True
TCR_N(C, B) = True
TCR_N(C, C) = True
TCR_N(C, D) = True
TCR_N(C, E) = True
TCR_N(C, F) = True
TCR_N(D, A) = False
TCR_N(D, B) = False
TCR_N(D, C) = False
TCR_N(D, D) = True
TCR_N(D, E) = True
TCR_N(D, F) = False
TCR_N(E, A) = False
TCR_N(E, B) = False
TCR_N(E, C) = False
TCR_N(E, D) = False
TCR_N(E, E) = True
TCR_N(E, F) = False
TCR_N(F, A) = True
TCR_N(F, B) = True
TCR_N(F, C) = True
TCR_N(F, D) = True
TCR_N(F, E) = True
TCR_N(F, F) = True
()
TC_N(A, A) = True
TC_N(A, B) = True
TC_N(A, C) = True
TC_N(A, D) = True
TC_N(A, E) = True
TC_N(A, F) = True
TC_N(B, A) = True
TC_N(B, B) = True
TC_N(B, C) = True
TC_N(B, D) = True
TC_N(B, E) = True
TC_N(B, F) = True
TC_N(C, A) = True
TC_N(C, B) = True
TC_N(C, C) = True
TC_N(C, D) = True
TC_N(C, E) = True
TC_N(C, F) = True
TC_N(D, A) = False
TC_N(D, B) = False
TC_N(D, C) = False
TC_N(D, D) = True
TC_N(D, E) = True
TC_N(D, F) = False
TC_N(E, A) = False
TC_N(E, B) = False
TC_N(E, C) = False
TC_N(E, D) = False
TC_N(E, E) = False
TC_N(E, F) = False
TC_N(F, A) = True
TC_N(F, B) = True
TC_N(F, C) = True
TC_N(F, D) = True
TC_N(F, E) = True
TC_N(F, F) = True
()
'''

# SETUP
Node = DeclareSort('Node')
Edge = Function('Edge', Node, Node, BoolSort())
s = Solver()
solver = s
A, B, C, D, E, F = Consts('A B C D E F', Node)

# CARDINALITY CONSTRAINT
X = Const('X', Node)
s.add(ForAll([X], Or(X == A, X == B, X == C, X == D, X == E, X == F)))

name = 0

def Join(a, b):
	print("Join")
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
		joinconst = Const("joinconst" + str(name), joinsort)

		out = Function("joinout" + str(name), *(sortsA + sortsB + [BoolSort()]))
		solver.add(ForAll(constsA + constsB, out(*(constsA + constsB)) == Exists([joinconst], And(a(*(constsA + [joinconst])), b(*([joinconst] + constsB))))))
		return out

'''
def Union(a, b):
	print("Union")
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
	print("Diff")
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

def Intersect(a, b):
	print("Intersect")
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
	print("Some")
	global name
	name += 1
	if r.arity() == 0:
		print("error: not a relation")
	else:
		sorts = []
		consts = []
		for x in range(r.arity()):
			consts.append(Const("sconst" + str(name) + str(x), r.domain(x)))
			sorts.append(r.domain(x))
		return Exists(consts, r(*consts))


def Single(const):
	print("Single")
	global name
	name += 1
	out = Function("Single" + str(name), const.sort(), BoolSort())
	temp = Const("Temp" + str(name), const.sort())
	s.add(ForAll([temp], out(temp) == (temp == const)))
	return out
'''

# Transitive reflexive closure of edges relation
TCR_N = Function('TCR_N', Node, Node, BoolSort())
Connected = Function('Connected', Node, Node, Node, BoolSort())
s.add(ForAll([A, B], TCR_N(A, B) == Or(Connected(A, B, B), A == B)))


# This axiomatization uses a bunch of variables,
# But it will never need to add in a new value. So it won't be like z3py closure
# and add in an unwanted variable.

q = Connected(A, A, B) == False
w = Implies(And(Connected(A, B, D), Connected(B, C, D)), Connected(A, C, D))
e = Implies(And(Connected(A, B, B), Connected(B, C, C), A != C), Connected(A, C, C))
r = Implies(And(Edge(A, B), A != B), Connected(A, B, B))
#t = Implies(Connected(A, B, B), Exists([E], And(TCR_N(A, E), Connected(A, E, B))))
t = Implies(Connected(A, B, B), Exists([E], And(Edge(A, E), Connected(A, E, B))))
y = Implies(And(Connected(A, B, C), B != C), Connected(B, C, C))


s.add(ForAll([A, B, C, D], And(q, w, e, r, t, y)))


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


print(s.check())
m = s.model()
print(m)


TC_N = Join(Edge, TCR_N)
print(s.check())

floop = [A, B, C, D, E, F]
names = ["A", "B", "C", "D", "E", "F"]

for x in range(len(floop)):
    for y in range(len(floop)):
        print("TCR_N(" + names[x] + ", " + names[y] + ") = " + str(m.eval(TCR_N(floop[x], floop[y]))))

print()

for x in range(len(floop)):
    for y in range(len(floop)):
        print("TC_N(" + names[x] + ", " + names[y] + ") = " + str(m.eval(TC_N(floop[x], floop[y]))))
print()
