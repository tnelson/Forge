#lang forge

sig C {}
sig A { f: C }
sig B { g: C }

fact f: func
fact g: func

run {} for exactly 1 A, exactly 1 B, exactly 2 C
