import sys
from time import time
from copy import copy, deepcopy
import random
from multiprocessing import Process, Queue
from queue import Empty


class Literal:
    def __init__(self, name, sign):
        self.name = name  # integer
        self.sign = sign  # boolean

    def __repr__(self):
        return ("-" if not self.sign else "") + self.name

    def __eq__(self, other):
        if type(other) != Literal:
            return False
        return self.name == other.name and self.sign == other.sign

    def __hash__(self):
      return hash((self.name, self.sign))


class Clause:
    def __init__(self, id, literalSet):
        self.id = id
        self.literalSet = literalSet

    def __repr__(self):
        return f"{self.id}: {str(self.literalSet)}"

    def __eq__(self, other):
        if type(other) != Clause:
            return False
        return self.id == other.id


class Assignment:
    def __init__(self, assn):
        self.assn = assn

    def __getitem__(self, item):
        return self.assn[item]

    def __setitem__(self, key, value):
        self.assn[key] = value

    def __delitem__(self, key):
        del self.assn[key]

    def __contains__(self, item):
        return item in self.assn

    def __repr__(self):
        return self.assn.__repr__()


class SetWrapper:
    def __init__(self, baseSet):
        self.baseSet = baseSet

    def __getitem__(self, item):
        return self.baseSet[item]

    def __setitem__(self, key, value):
        self.baseSet[key] = value

    def __delitem__(self, key):
        del self.baseSet[key]

    def __contains__(self, item):
        return item in self.baseSet

    def __repr__(self):
        return self.baseSet.__repr__()

    def append(self, item):
        self.baseSet.append(item)

    def remove(self, item):
        self.baseSet.remove(item)


class VarSet(SetWrapper):
    pass


def rmoms(var, clauseSet):
    # implementation of randomized MOMS (max occurances of min size)
    smallClauseSize = 20
    k = 1.5

    negOcc = 0
    posOcc = 0

    posLit = Literal(var, True)
    negLit = Literal(var, False)

    for clause in clauseSet:
        if posLit in clause:
            posOcc += 1
        if negLit in clause:
            negOcc += 1

    return (posOcc + negOcc) * (2 ** k) + (posOcc * negOcc), posOcc > negOcc


def jerWang(var, clauseSet):
    # implementation of Jeroslow-Wang
    posScore = 0
    negScore = 0
    for clause in clauseSet:
        if Literal(var, True) in clause.literalSet:
            posScore += 2 ** (-len(clause.literalSet))
        if Literal(var, False) in clause.literalSet:
            negScore += 2 ** (-len(clause.literalSet))

    return max(posScore, negScore), posScore > negScore


def rdlis(var, clauseSet):
    # implementation of randomized DLIS (dynamic largest individual sum)
    negOcc = 0
    posOcc = 0

    posLit = Literal(var, True)
    negLit = Literal(var, False)

    for clause in clauseSet:
        if posLit in clause:
            posOcc += 1
        if negLit in clause:
            negOcc += 1

    return max(posOcc, negOcc), posOcc > negOcc


def chooseVariableSplit(varbs, clauseSet, heuristic):
    # if we don't use heuristic, just use the first var and random assignment
    if heuristic is None:
        return varbs[0], bool(random.getrandbits(1))

    # dictionary of our top candidates of form {var: (score, sign)}
    topCands = {}
    for v in varbs.baseSet:
        vscore, vsign = heuristic(v, clauseSet)

        # if top candidates not full, add to it
        if len(topCands) < 5:
            topCands[v] = (vscore, vsign)
            continue

        # otherwise replace worst candidate if better
        worstTopCand = None
        for cand in topCands:
            if worstTopCand is None or topCands[cand][0] < topCands[worstTopCand][0]:
                worstTopCand = cand
        
        if vscore > topCands[worstTopCand][0]:
            del topCands[worstTopCand]
            topCands[v] = (vscore, vsign)

    # randomly choose a top candidate              
    var, scoreSign = random.choice(list(topCands.items()))
    return var, scoreSign[1]


def unitClauseElim(varbs, clauseSet, assignment):
    changed = False

    for clause in copy(clauseSet):
        if len(clause.literalSet) == 1:
            unitLit = clause.literalSet[0]
            unitLitInv = Literal(unitLit.name, not unitLit.sign)

            assignment[unitLit.name] = unitLit.sign
            # this var might've already been removed - skip it
            if unitLit.name in varbs:
                varbs.remove(unitLit.name)
            else:
                continue
            changed = True

            for otherClause in copy(clauseSet):
                # remove everywhere inverse shows up
                for otherLit in copy(otherClause.literalSet):
                    if otherLit == unitLitInv:
                        otherClause.literalSet.remove(otherLit)

                # remove clauses containing the normal literal
                if len(otherClause.literalSet) > 1 and unitLit in otherClause.literalSet:
                    clauseSet.remove(otherClause)
    
    return assignment, changed


def sameSignElim(varbs, clauseSet, assignment):
    changed = False

    # get all literals
    allLiterals = set()
    for clause in clauseSet:
        for lit in clause.literalSet:
            allLiterals.add(lit)

    # find pure literals, remove them, and add to assignment
    for lit in copy(allLiterals):
        oppLit = Literal(lit.name, not lit.sign)
        if oppLit not in allLiterals:
            assignment[lit.name] = lit.sign
            if lit.name in varbs:
                varbs.remove(lit.name)
            changed = True

            # remove all clauses containing that literal
            for clause in copy(clauseSet):
                if lit in clause.literalSet:
                    clauseSet.remove(clause)

    return assignment, changed


def assignVariable(var, sign, varbs, clauseSet, assignment):
    assignment[var] = sign
    varbs.remove(var)

    for clause in copy(clauseSet):
        for lit in copy(clause.literalSet):
            if lit.name == var and lit.sign == sign:
                # remove this clause and stop checking this clause's literals
                clauseSet.remove(clause)
                break
            elif lit.name == var and lit.sign != sign:
                clause.literalSet.remove(lit)
    
    return clauseSet, assignment


def solve(varbs, clauseSet, assignment):
    origVarbs = deepcopy(varbs.baseSet)
    origAssn = deepcopy(assignment.assn)

    # do inference with unit clause and same sign elim until we can't anymore
    changed = True
    while changed:
        assignment, uceChange = unitClauseElim(varbs, clauseSet, assignment)
        assignment, sseChange = sameSignElim(varbs, clauseSet, assignment)
        changed = uceChange or sseChange

    if any(c.literalSet == [] for c in clauseSet):
        # if there's an empty clause, it's UNSAT - clean up and return
        varbs.baseSet = origVarbs
        assignment.assn = origAssn
        return None
    elif not clauseSet:
        # if there are no clauses, it's SAT - no need to clean up
        return assignment

    # clean up our copies and only keep the differences
    varbsDiff = []
    for v in origVarbs:
        if v not in varbs:
            varbsDiff.append(v)
    assnDiff = {}
    for v in assignment.assn:
        if v not in origAssn:
            assnDiff[v] = assignment[v]

    del origVarbs
    # del origClauseSet
    del origAssn

    # decide which variable to split on
    var, sign = chooseVariableSplit(varbs, clauseSet, jerWang)

    fClauseSet = deepcopy(clauseSet)

    # try solving down the first branch
    fClauseSet, assignment = assignVariable(var, sign, varbs, fClauseSet, assignment)
    newAssignment = solve(varbs, fClauseSet, assignment)

    # if this branch isn't UNSAT, return its assignment - it's a solution
    if newAssignment is not None:
        return newAssignment
    
    # if we need to try the other sign assignment, flip the assignment
    del newAssignment
    del assignment[var]
    varbs.append(var)
    clauseSet, assignment = assignVariable(var, not sign, varbs, clauseSet, assignment)
    newAssignment = solve(varbs, clauseSet, assignment)

    # if this branch is also not UNSAT, return it - it's a solution
    if newAssignment is not None:
        return newAssignment

    # otherwise, clean up our varbs and assignment to be back at the original state
    del assignment[var]
    varbs.append(var)

    # also clean up changes from inference (uce, sse)
    for vd in varbsDiff:
        varbs.append(vd)
    for ad in assnDiff:
        del assignment[ad]

    return None


def runSolver(conn, varbs, clauseSet):
    assignment = solve(deepcopy(varbs), deepcopy(clauseSet), Assignment({}))
    # assign any variable not already assigned to true (if SAT)
    if assignment is not None:
        for v in varbs.baseSet:
            if v not in assignment:
                assignment[v] = True

    conn.put(assignment)


def verifySolution(assignment, clauseSet):
    for clause in clauseSet:
        clauseSatisfied = False
        for lit in clause.literalSet:
            # this literal satisfied the clause
            if assignment[lit.name] == lit.sign:
                clauseSatisfied = True
                break

        # this clause not satisfied
        if not clauseSatisfied:
            return False

    # all clauses satisfied
    return True


def readInput(cnfFile):
    variableSet = VarSet([])
    clauseSet = []
    nextCID = 0
    with open(cnfFile, "r") as f:
        for line in f.readlines():
            tokens = line.strip().split()
            if tokens and tokens[0] != "p" and tokens[0] != "c":
                literalSet = []
                for lit in tokens[:-1]:
                    sign = lit[0] != "-"
                    variable = lit.strip("-")

                    literalSet.append(Literal(variable, sign))
                    if variable not in variableSet:
                        variableSet.append(variable)

                clauseSet.append(Clause(nextCID, literalSet))
                nextCID += 1
    
    return variableSet, clauseSet


def printOutput(file, assignment, runTime):
    result = ""
    isSat = (assignment is not None)
    if isSat:
        for var in assignment.assn:
            result += ("" if assignment[var] else "-") + str(var) + " "

    print(f"s {'SATISFIABLE' if isSat else 'UNSATISFIABLE'}")
    if isSat:
        print(f"v {result}")


if __name__ == "__main__":
    inputFile = sys.argv[1]
    varbset, clauseSet = readInput(inputFile)
    verifClauseSet = deepcopy(clauseSet)

    queueConn = Queue()
    numProcs = 7
    solvers = [Process(target=runSolver, args=(queueConn, varbset, clauseSet)) for _ in range(numProcs)]

    startTime = time()

    # do a restart, with growing time increments
    timeout = 30
    timeoutMult = 2
    for solverProc in solvers:
        solverProc.start()
    while True:
        try:
            assignment = queueConn.get(block=True, timeout=timeout)
            break
        except Empty:
            # kill solver and restart
            timeout *= timeoutMult
            print(f"Restarting solver with timeout {timeout:.2f}s")

            for solverProc in solvers:
                solverProc.terminate()
            solvers = [Process(target=runSolver, args=(queueConn, varbset, clauseSet)) for _ in range(numProcs)]
            for solverProc in solvers:
                solverProc.start()

    for solverProc in solvers:
        solverProc.terminate()

    runtime = time() - startTime
    printOutput(inputFile, assignment, runtime)
