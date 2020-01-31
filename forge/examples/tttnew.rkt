#lang forge

// TODO: allow multi sigs on one line
abstract sig Player {}
one sig X extends Player {}
one sig O extends Player {}

abstract sig Index {inverse: set Index }
one sig A extends Index {}
one sig B extends Index {}
one sig C extends Index {}

-- TODO
pred abstractness {
  all p: Player | p in X+O
  all i: Index | i in A+B+C
}

-- test : run {} for exactly 2 Player, exactly 3 Index

pred fixInverses { 
   inverse = A->C + B->B + C->A 
}

sig Board {places: set Index -> Index -> Player}


pred xturn[b: Board] {
  #b.places.X = #b.places.O  
} 
pred somexturn[] {
   some b: Board | xturn[b]
}

--try_xturn: run {abstractness fixInverses somexturn } for exactly 2 Player, 3 Index, 1 Board

pred oturn[b: Board] { 
  subtract[#b.places.X,1] = #b.places.O
}

/*
try_oturn : run{
  abstractness
  fixInverses
  some b: Board | oturn[b]}
for exactly 2 Player, 3 Index, 1 Board
*/



pred atMostOneMarkPerSquare[] {
  all b: Board | all r: Index | all c: Index | {
    lone r.(c.(b.places))    
  }
}

pred winH[b: Board, p: Player] {
  some r: Index | all c: Index |
    r->c->p in b.places
--p in b.places[r][c]
}

pred winV[b: Board, p: Player] {
  some c: Index | all r: Index |
    r->c->p in b.places
--p in b.places[r][c]
}

pred winDBroken[b: Board, p: Player] {
  all i: Index | {
    --p in b.places[i][i]
    --or
    --p in b.places[i][i.inverse]

    i->i->p in b.places
    or
    i->i.inverse->p in b.places
  }
}

pred winD[b: Board, p: Player] {
  {all i: Index | i->i->p in b.places}
  or
  {all i: Index | i->i.inverse->p in b.places}
}

       

diags_equiv : run {
  atMostOneMarkPerSquare
  fixInverses
  some b: Board | {
     -- NO ERROR FROM THIS? TODO
     -- (xturn or oturn)
     (xturn[b] or oturn[b])
     ({winD[b, O] and not winDBroken[b, O]}
     or
     {not winD[b, O] and winDBroken[b, O]})
   }
} for 2 Player, 3 Index, 1 Board
-- they are not!

/*
pred winning[b: Board, p: Player] {
  winH[b, p] or winV[b, p] or winD[b, p]
}


pred valid_board[b: Board] {
  oturn[b] or xturn[b]
}*/

