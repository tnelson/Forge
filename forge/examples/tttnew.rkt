#lang forge

// TODO: allow multi sigs on one line
sig Player {}
sig X extends Player {}
sig O extends Player {}

sig Index {inverse: set Index }
one sig A extends Index {}
one sig B extends Index {}
one sig C extends Index {}

--test : run {} for exactly 2 Player, exactly 3 Index

pred fixInverses[] { 
   inverse = A->C + B->B + C->A 
}

sig Board {places: set Index -> Index -> Player}


pred xturn[b: Board] {
  //#b.places.X = #b.places.O
  b.places.X = b.places.O
} 
pred somexturn[] {
   some b: Board | xturn[b]
}

try_xturn : run somexturn for 2 Player, 3 Index, 1 Board


/*
pred oturn[b: Board] { 
  subtract[#b.places.X,1] = #b.places.O
}


       
pred atMostOneMarkPerSquare[] {
  all b: Board | all r: Index | all c: Index | {
    (#b.places[r][c] = 1)
    implies
    (#b.places[r][c] = 0) // inefficient
  }
}

pred winH[b: Board, p: Player] {
  some r: Index | all c: Index | p in b.places[r][c]
}

pred winV[b: Board, p: Player] {
  some c: Index | all r: Index | p in b.places[r][c]
}


pred winDBroken[b: Board, p: Player] {
  all i: Index | {p in b.places[i][i] or p in b.places[i][i.inverse]}
}

pred winD[b: Board, p: Player] {
  {all i: Index | p in b.places[i][i]}
  or
  {all i: Index | p in b.places[i][i.inverse]}
}
*/
       

/*diags_equiv : run {
  atMostOneMarkPerSquare
  fixInverses
  --some b: Board | {
  --   {winD[b, O] and not winDBroken[b, O]}
  --   or
  --   {not winD[b, O] and winDBroken[b, O]}
  -- }
} for 2 Player, 3 Index, 1 Board
*/

/*
pred winning[b: Board, p: Player] {
  winH[b, p] or winV[b, p] or winD[b, p]
}


pred valid_board[b: Board] {
  oturn[b] or xturn[b]
}*/

