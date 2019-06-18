from z3 import *

# SETUP
Node = DeclareSort('Node')
s = Solver()
solver = s
A, B = Consts('A B', Node)
name = 0

# CARDINALITY CONSTRAINT
X = Const('X', Node)
s.add(ForAll([X], Or(X == A, X == B)))
s.add(Not(A == B))

#def Func(const):
#	return (const.get_id() == 5)

def Single(const):
	print("Single")
	global name
	name += 1
	out = Function("Single" + str(name), const.sort(), BoolSort())
	temp = Const("Temp" + str(name), const.sort())
	s.add(ForAll([temp], out(temp) == (temp == const)))
	return out

#x = Single(A)
#s.add(x(A))

'''
OK!!!! Here's the deal.
Single(A) is just one function. Even in s.add(ForAll([A], Single(A)(A))),
Single(A) is evaluated only once, on A. now, we don't know ahead of time what value
A will contain. And the ForAll checks that it will work for all values of A.
But in the end, all Single(A)s will be the same function, and all As will be the same value.
And the function syntax isn't like "check the value of A passed in at the time to the function"
It's not like that. That's complicated.
It's just like, what is the value of A. Just one value.
'''


# OK work on axiomatizing TC in the other ways (although check ours)
# Consider ways of axiomatizing things like Union without introducing new functions.
# OR SingleA. The just put constraitns on A in the forall.
# But that would mean putting Union in a forall? probably?
# Maybe not.... Could expand to a forall. OR could just expand to an expression.


s.add(Forall([A], Single(A)(A)))

# OK the question is, when we pass a constant inside a funtion in a forall,
# does it really work for all the constants?

# How can I phrase that better?

# I need to check the old functions to see what they did.

# Ok, what would be bad? It would be bad, if, for instance, a ForAll returned true
# when it should have returned false.

#s.add(ForAll([A], Func(A)))

#print(Func(A))
#print(Func(B))
#print(B.get_id())


print(s.check())
m = s.model()
#print(m.eval(Single(B)(B)))
print(m)
