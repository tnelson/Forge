#lang forge/froglet 

/*
  Froglet allows the "in" operator to be used under very specific circumstances. 
  This suite is meant to check that this feature doesn't negatively impact things
  like error messages, and to confirm that the desired functionality actually works.

  To test this, we need a fairly deep type hierarchy, so let's use a small subset of 
  the taxonomy. We'll (arbitrarily) say that animals can have best friends, and chordates
  can eat cakes. 
*/

sig Eukaryote {}
sig Animal extends Eukaryote {
    bestFriend: lone Animal
}
sig Ctenophore extends Animal {}
sig Chordate extends Animal {
    cakesEaten: one Int
}
sig Dog extends Chordate {}
sig Cat extends Chordate {}
sig Human extends Chordate {}
one sig NimTelson extends Human {}

option run_sterling off

pred hasFriend[e: Eukaryote] { some e.bestFriend }
pred eatenCakes[e: Eukaryote] { some e.cakesEaten }
pred eatenCakesImpossible[c: Ctenophore] { some c.cakesEaten }

test expect {
    disallowRHS_unary_join: {some e: Eukaryote | e in cakesEaten.Int} 
      is forge_error "was not an object"
    disallowLHS_non_singleton: { Animal in Eukaryote } is forge_error 
    disallowLHS_non_singleton_op: { Dog+Cat in Eukaryote } is forge_error

    allow_basic: {some e: Eukaryote | e in Animal} is sat
    allow_pred1: {some e: Eukaryote | hasFriend[e]} is sat
    allow_pred2: {some e: Eukaryote | eatenCakes[e]} is sat
    error_pred: {some c: Ctenophore | eatenCakesImpossible[c]} 
      is forge_error "Sig Ctenophore does not have such a field"

}

