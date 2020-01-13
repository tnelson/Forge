#lang forge

sig X {}
sig A { r: X->X }

run {} for exactly 2 A, exactly 3 X
