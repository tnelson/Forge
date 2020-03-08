#lang forge

--option verbosity 10

sig City {roads: set Int->City}
/*one sig A extends City {}
one sig B extends City {}
one sig C extends City {}
one sig D extends City {}
one sig E extends City {}
one sig F extends City {}
one sig G extends City {}
one sig H extends City {}
one sig I extends City {}
one sig J extends City {}

pred slow {
  let onedirection = 
    A->sing[4]->D + D->sing[5]->I + A->sing[6]->B + B->sing[4]->C + A->sing[3]->C +
    B->sing[3]->E + C->sing[5]->D + C->sing[2]->E + E->sing[6]->F + E->sing[5]->G +
    D->sing[2]->H + D->sing[3]->F + F->sing[4]->G + F->sing[2]->H + H->sing[2]->I +
    G->sing[3]->J + H->sing[5]->J + I->sing[4]->J | {
  roads = onedirection +
          {a: City, w: Int, b: City | b->w->a in onedirection}
  }
}

pred faster {
    roads =
    A->sing[4]->D + D->sing[5]->I + A->sing[6]->B + B->sing[4]->C + A->sing[3]->C +
    B->sing[3]->E + C->sing[5]->D + C->sing[2]->E + E->sing[6]->F + E->sing[5]->G +
    D->sing[2]->H + D->sing[3]->F + F->sing[4]->G + F->sing[2]->H + H->sing[2]->I +
    G->sing[3]->J + H->sing[5]->J + I->sing[4]->J +
    D->sing[4]->A + I->sing[5]->D + B->sing[6]->A + C->sing[4]->B + C->sing[3]->A +
    E->sing[3]->B + D->sing[5]->C + E->sing[2]->C + F->sing[6]->E + G->sing[5]->E +
    H->sing[2]->D + F->sing[3]->D + G->sing[4]->F + H->sing[2]->F + I->sing[2]->H +
    J->sing[3]->G + J->sing[5]->H + J->sing[4]->I
}
*/

-- Try this with one sigs + the slow predicate
-- "problem size: variables = 3203, clauses = 3202, state = 3200 bits"
-- " translation = 857, SAT = 3"
--run {slow} for 10 City, 5 Int

-- "solving time (ms): total = 363, parsing = -1, translation = 343, SAT = 21"
--run {faster} for 10 City, 5 Int

pred wellformedgraph {
  all n1,n2: City | lone n1.roads.n2
}

-- Faster, and you don't need (and shouldn't use!!) sing in the declarations
-- (This is something, again, Alloy didn't have.)
-- DO WITH ONE CONCRETE PRED AT FIRST
inst concreteCity {
    City = A + B + C + D + E + F + G + H + I + J
}
inst concrete {
   concreteCity
   roads =
    A->4->D + D->5->I + A->6->B + B->4->C + A->3->C +
    B->3->E + C->5->D + C->2->E + E->6->F + E->5->G +
    D->2->H + D->3->F + F->4->G + F->2->H + H->2->I +
    G->3->J + H->5->J + I->4->J +
    D->4->A + I->5->D + B->6->A + C->4->B + C->3->A +
    E->3->B + D->5->C + E->2->C + F->6->E + G->5->E +
    H->2->D + F->3->D + G->4->F + H->2->F + I->2->H +
    J->3->G + J->5->H + J->4->I
}
--run {} for 5 Int for concrete
--run {wellformedgraph} for 5 Int for concreteCity


--------------------------------------------------------------------------------

// Note: sentinel value, beware! HIDDEN! ZERO
// Note also: if we allowed multiple edges 
//   of different weights between the same cities,
//   this would break!

fun w[a: City, b: City]: Int {
  a.roads.b 
}

-- How would we take a subset of the edges (like a spanning tree or path)
-- and produce total weight? 

fun weighta[es: City -> Int -> City]: Int {
  sum[City.es.City]  
}

fun weightb[es: City -> Int -> City]: Int {
  sum a: es[City][Int] | sum b: es.City.Int | sum[es[a].b]
}

pred reachableWithHopsOfBug[src: City, dst: City, hopmax: Int] {
  dst in src.^({a: City, b: City | sum[w[a, b]] <= hopmax})  
}

pred reachableWithHopsOf[src: City, dst: City, hopmax: Int] {
  dst in src.^({a: City, b: City | some a.roads.b and sum[w[a, b]] <= hopmax})
}

run {some src, dst: City | reachableWithHopsOfBug[src, dst, 0] and not reachableWithHopsOf[src, dst, 0]} for 5 Int for concrete