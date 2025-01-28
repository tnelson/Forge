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
sig Dog extends Chordate {
    buddy: one Cat
}
sig Cat extends Chordate {
    bff: one Dog
}
one sig Boatswain extends Dog {}

option run_sterling off

pred hasFriend[e: Eukaryote] { some e.bestFriend }
pred eatenCakes[e: Eukaryote] { some e.cakesEaten }
pred eatenCakesImpossible[c: Ctenophore] { some c.cakesEaten }

/* This "narrowing" will not occur as of January 2025. */ 
fun getBuddyOrBFF[c: Chordate]: lone Chordate {
  (c in Dog => c.buddy else c in Cat => c.bff else none)
}

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
    
    // Tests for non-existence of "narrowing" behavior (related, but not same as "in")
    no_narrowing_needed: {some c: Chordate | some getBuddyOrBFF[c]} is sat
    no_narrowing_option1: {some d: Dog | some getBuddyOrBFF[d]} 
      is forge_error "Sig Dog does not have such a field in \(d.bff\)"  
    no_narrowing_option2: {some c: Cat | some getBuddyOrBFF[c]}
      is forge_error "Sig Cat does not have such a field in \(c.buddy\)"  

    // Tests for unusual LHS terms
    no_general_sig_lhs: {Dog in Chordate} is forge_error "invalid use of"
    ok_one_sig_lhs: {Boatswain in Chordate} is sat

    // #{...} is allowed in Froglet, and we should allow proper "in" use inside and outside. 
    singleton_in_numeric: {#{a: Animal | a in Dog} in Int} is sat
    // No Forge error expected here; for the moment we are allowing always-false "instanceof" 
    nonsingleton_in_numeric: {#{a: Animal | a in Dog} in Animal} is unsat
    


}


