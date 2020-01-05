#lang forge

sig C {}
sig A { f: C }
sig B { g: C }

fact f: func
fact g: func

pred p { some univ }

q1 : run p for exactly 1 A, exactly 1 B, exactly 2 C
