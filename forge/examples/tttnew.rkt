#lang forge

/*
  Tests for instances, etc. but also a lecture framework...
  Begin lecture with only the base model; no tests or instances.
*/

abstract sig Player {} -- abstract not yet in
one sig X extends Player {} -- X,O not yet in
one sig O extends Player {}

abstract sig Index {inverse: set Index }
one sig A extends Index {}
one sig B extends Index {}
one sig C extends Index {}

sig Board {places: set Index -> Index -> Player}

-- *** works
run {}

pred wellformed {
  all p: Player | p in X+O
  all i: Index | i in A+B+C

  inverse = A->C + B->B + C->A

  all b: Board | all r: Index | all c: Index | {
    lone r.(c.(b.places))    
  }
}

pred xturn[b: Board] {
  #b.places.X = #b.places.O  
} 
pred somexturn {
   some b: Board | xturn[b]
}
pred oturn[b: Board] { 
  subtract[#b.places.X,1] = #b.places.O
}


test expect {
  try_xturn: {wellformed somexturn } for exactly 2 Player, 3 Index, 1 Board is sat
  try_oturn : {wellformed some b: Board | oturn[b]} for exactly 2 Player, 3 Index, 1 Board is sat
  xodisjoint : {wellformed some b1, b2: Board | oturn[b1] and xturn[b2]} for exactly 2 Player, 3 Index, 1 Board is unsat
}


pred winH[b: Board, p: Player] {
  some r: Index | all c: Index |
    p in b.places[r][c]
}

pred winV[b: Board, p: Player] {
  some c: Index | all r: Index |
    p in b.places[r][c]
}

pred winDBroken[b: Board, p: Player] {
  all i: Index | {
    p in b.places[i][i] or
    p in b.places[i][i.inverse]    
  }
}

pred winD[b: Board, p: Player] {
  (all i: Index | i->i->p in b.places)
  or
  (all i: Index | i->i.inverse->p in b.places)
}

pred valid[b: Board] {
  oturn[b] or xturn[b]
}

/*
test expect {
  diagsNotEquiv : {
    wellformed
  
    some b: Board | {
       -- NO ERROR FROM THIS? TODO
       -- xturn or oturn
       valid[b]
       
       ({winD[b, O] and not winDBroken[b, O]}
       or
       {not winD[b, O] and winDBroken[b, O]})
     }
  } for 2 Player, 3 Index, 1 Board is sat
}*/
       
pred winning[b: Board, p: Player] {
  winH[b, p] or winV[b, p] or winD[b, p]
}

/*
inst boiler {
  Board = Board0
  A = A0 and B = B0 and C = C0 and X = X0 and O = O0
  Index = A+B+C Player = X+O
  inverse = A->C + B->B + C->A -- also in constraints; can remove there?  
}

inst emptyGame {
  boiler
  places = none
}

--run {wellformed} for exactly emptyGame
test expect {
    wfEmpty1: {wellformed} for exactly emptyGame is sat
    wfEmpty2: {not wellformed} for exactly emptyGame is unsat    
}*/
/*
inst fullGame1 {
  boiler
  places = Board0->A->A->X + Board0->A->B->O + Board0->A->C->X +
           Board0->B->A->O + Board0->B->B->X + Board0->B->C->O +
           Board0->C->A->X + Board0->C->B->O + Board0->C->C->X
}

inst fullGame2 {
  boiler
  places = Board0->A->A->X + Board0->A->B->O + Board0->A->C->X +
           Board0->B->A->O + Board0->B->B->X + Board0->B->C->O +
           Board0->C->A->X + Board0->C->B->O + Board0->C->C->X
}


test expect {
    full1_wf: {wellformed} for exactly fullGame1 is sat
    full1_nowinner1: {winning[Board, X]} for exactly fullGame1 is sat
    full1_nowinner2: {winning[Board, O]} for exactly fullGame1 is unsat
}*/

/*
inst partGame1 {
  boiler
  places =                                     Board0->A->C->O +
           Board0->B->A->O + Board0->B->B->X + Board0->B->C->X
           
}

test expect {
    part1_wf: {wellformed} for exactly partGame1 is sat
    part1_valid: {valid[Board]} for exactly partGame1 is sat
    --part1_doesntwork: {valid[Board0]} for exactly partGame1 is sat -- NOT A BUG this won't work, because Board0 is an atom, not a name
    part1_xturn: {xturn[Board]} for exactly partGame1 is sat
    part1_oturn: {oturn[Board]} for exactly partGame1 is unsat
    part1_nobodywinning: {no p: Player | winning[Board, p]} for exactly partGame1 is sat -- note sat = true with total insts
}*/

-- *** Breaks
run {} --for 2 Player, 3 Index, 1 Board