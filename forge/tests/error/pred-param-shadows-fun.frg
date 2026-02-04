#lang forge
option run_sterling off

sig Person {}

fun getP[p: Person]: one Person { p }

-- Parameter 'getP' shadows function 'getP'
pred shadowsFun[getP: Person] { some getP }
