#lang forge

/* 
  Copying the sig hierarchy from froglet-in-checks.frg, since we need a deep
  hierarchy to test these messages. 
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
option verbose 0

pred pAnimalCtenophore[a: Animal, c: Ctenophore] { }

pred pAnimalRelation[ar: Animal->Animal] {}

// TODO: move tests from error/main.rkt where possible. 
test expect {
    pred_type: {some d: Dog | pAnimalCtenophore[d, d]} 
      is forge_error "Expected type in \(\(Ctenophore\)\), but argument was in type \(\(Dog\)\)"    
    
    // Predicates can take arguments that have arity >1. We may not succeed at shrinking the list 
    // of types in this case, however. 
    pred_relation_arg: { pAnimalRelation[Animal->Animal] } is sat
    pred_relation_arg_type: { pAnimalRelation[Eukaryote->Eukaryote] } 
      is forge_error "argument was in type" // until we deprimify >1-ary arguments

}