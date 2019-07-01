import string
import random
import sys


# Time to complete on...
# 1 bit: 3.010s
# 2 bits: 3.312s
# 3 bits: 3.650s
# 4 bits: 3.399s
# 5 bits: 3.703s
# 6 bits: 4.706s
# 7 bits: 5.799s
# 8 bits: 23.194s
# 9 bits: 3m31.831s


BITWIDTH = int(sys.argv[1])
INT_MAX = 2**BITWIDTH

print("#lang rosette")
print()
print("(require ocelot)")
print("(require \"ocelot/nextbutton.rkt\")")
print()

# Print the list of integer atoms
int_atoms = []
print("(bind-universe U B S (", end = "")
for i in range(BITWIDTH):
    print("i" + str(i) + " ", end="")
for i in range(INT_MAX):
    int_atoms.append("z" + str(i))
print(" ".join(int_atoms), end = "")
print("))")

print()
print("(define verum (= none none))")
print("(define falsum (! verum))")
print()

# Generate the bitvector for each int
def genBitList(i, bitwidth):
    power = 2**(bitwidth-1)

    if bitwidth == 0: return []
    elif i >= power:
        return ["verum"] + genBitList(i - power, bitwidth - 1)
    else: return ["falsum"] + genBitList(i, bitwidth - 1)


# print the bitvector definitions
allBitVectors = []
for i in range(INT_MAX):
    bits = genBitList(i, BITWIDTH)
    allBitVectors.append(bits)
    print("(define bv" + str(i) + " (list " + " ".join(bits) + "))")

# Print the indices relation
print()
print("(define indices (declare-relation 1 \"indices\"))")
print("(define indices-bounds (make-exact-bound indices '(", end="")
for i in range(BITWIDTH):
    print("(i" + str(i) + ") ", end="")
print(")))")
    

print()

print("(define ints (declare-relation 1 \"ints\"))")
print("(define ints-bound (make-exact-bound ints '(", end="")
singleton_int_atoms = map(lambda x: "(" + x + ")", int_atoms)
print(" ".join(singleton_int_atoms), end="")
print(")))")
print()

# print the bounds on the map from int atoms to bit values
print("(define ints-map (declare-relation 2 \"ints-map\"))")
print("(define ints-map-bound (make-exact-bound ints-map '(", end=""),
ints_map_bounds_helper = []
for i in range(INT_MAX):
    i_bounds = []
    for j in range(BITWIDTH):
        if allBitVectors[i][j] == "verum":
            i_bounds.append("(z" + str(i) + " i" + str(BITWIDTH - j - 1) + ")")
    if "verum" in allBitVectors[i]:
        ints_map_bounds_helper.append(i_bounds)

ints_map_bounds = []
for i in range(len(ints_map_bounds_helper)):
    ints_map_bounds.append(" ".join(ints_map_bounds_helper[i]))
print("\n".join(ints_map_bounds), end="")
print(")))")
print()


# Print some constant definitions
print(
        '''(define all-bounds (instantiate-bounds (bounds U (append B (list indices-bounds ints-bound ints-map-bound)))))

(define (iff a b)
(and (=> a b) (=> b a)))

; returns sum, carry
(define (halfadd b0 b1)
(define band (and b0 b1))
(values (and (or b0 b1) (not band))
band))

; returns sum, carry
(define (fulladd b0 b1 carry)
(define-values (h1-sum h1-carry) (halfadd b0 b1))
(define-values (h2-sum h2-carry) (halfadd h1-sum carry))
(define carry-out (or h1-carry h2-carry))
(values h2-sum carry-out))
''')

# Print the plus function on bitvectors
print("; returns atom representing x + y")
print("(define (plus x y)")

for i in range(BITWIDTH):
    print("(define x" + str(i) + " (list-ref x " + str(BITWIDTH - i - 1) + "))")

for i in range(BITWIDTH):
    print("(define y" + str(i) + " (list-ref y " + str(BITWIDTH - i - 1) + "))")

print("(define-values (b0-sum b0-carry) (fulladd x0 y0 falsum))")
for i in range(1, BITWIDTH):
    print("(define-values (b" + str(i) + "-sum b" + str(i) + "-carry) (fulladd x" + str(i) + " y" + str(i) + " b" + str(i-1) + "-carry))")

list_return = "(list"
for i in range(BITWIDTH):
    list_return += " b" + str(BITWIDTH - i - 1) + "-sum"
list_return += "))"
print(list_return)

print()

# Print the bitvector equality function
print("(define (same-bv bva bvb)")
print("(and ")
for i in range(BITWIDTH):
    print("(iff (list-ref bva " + str(i) + ")" + "(list-ref bvb " + str(i) + "))", end="")
    if i != BITWIDTH - 1:
        print()
print("))")

print()

# Print some basic bitvector tests
random.seed();
print("(define constraints (and", end="")
for i in range(200):
    a = random.randrange(INT_MAX)
    b = random.randrange(INT_MAX - a)
    print("\n[same-bv (plus bv" + str(a) + " bv" + str(b) + ") bv" + str(a + b) + "]", end="")
print("))")

print()
print("(println \"Finished constraint interpretation, beginning translation.\")")
print("(get-model constraints all-bounds S)")
