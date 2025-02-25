#lang forge 

/*
  Extending Int

  Because child sigs cannot overlap, we omit testing "one sig" and "lone sig" and 
  interactions with normal extension in this file. 

  TODO: doublecheck especially "one sig A extends Int" in combination with this shape. 
  TODO: script-view will error out for Int-extending sigs, because it will detect double-registration
    of an atom (from Int, which is pre-populated, and from the extender.)
*/

option verbose 0
option run_sterling off 

sig A extends Int {}

properSubset: assert {some (Int-A)} is sat
nonEmptySubset: assert {some A} is sat
emptySubset: assert {#A = 0} is sat
card1: assert {#A = 1} is sat
card2: assert {#A = 2} is sat
card3: assert {#A = 3} is sat
singleton0: assert {A = 0} is sat
singleton1: assert {A = 1} is sat
singletonNeg2: assert {A = -2} is sat
pair1_5: assert {A = 1 + 5} is sat
pairNeg3_1_5: assert {A = -3 + 1 + 5} is sat
pairNeg3_1_5_6: assert {A = -3 + 1 + 5 + 6} is sat

--run {some A}
