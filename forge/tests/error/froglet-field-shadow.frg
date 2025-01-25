#lang forge/froglet

// Student created a quantified variable that's named the same as a field, then tried to use the field.
// (https://edstem.org/us/courses/31922/discussion/2548781?comment=5844098).
// https://edstem.org/us/courses/31922/discussion/2548781?comment=5844098

sig Person {
  spouse: lone Person
}

pred ownGrandparent {
  one me: Person, spouse: Person | {
    spouse.spouse = me
  }
}

run {ownGrandparent}
// TODO: this won't error without the join issue; shadowing isn't being warned

      
//pred oG2 {
//  some me: Person, spouse: Person | {
//    spouse.spouse = me
//  }
//}
//
