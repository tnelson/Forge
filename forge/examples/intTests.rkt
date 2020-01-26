#lang forge

<<<<<<< HEAD
=======

>>>>>>> 5b818b398cea100e6b68646a0ccab37a636aa1c6
sig A {
    i: one Int
}

<<<<<<< HEAD
run {
    --some a1: A | some a2: A | a1.i > a2.i
    3 < 4
} for exactly 4 A
=======
run {} for exactly 4 A
>>>>>>> 5b818b398cea100e6b68646a0ccab37a636aa1c6
